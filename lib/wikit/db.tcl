# Collection of low-level Wiki database accessors and mutators
#
# admin:
# Wiki - 
# WikiDatabase - open the Wiki database
#
# accessors:
# GetTitle - get page's title by id
# GetPage - get page's content by id
# pagevars - get vars from page view
#
# mutators:
# FixPageRefs - traverse pages building a reference view
# SavePage - save a changed page to page view
# DoCommit - commit db changes
# DoSync - sync Wikit to the contents of a URL

# Structure of the database (wdb)
# 2 views: 'pages' and 'refs'.

# "pages" is the main view. It contains all pages managed by the wiki,
# and their contents.

# "refs" is a view with <from,to> page id pairs, one for each hyperlink,
# used to generate the back-references page (N!), for example.

# pages
# - string      name    Title of page
# - string      page    Contents of page.
# - integer     date    date of last modification of the page as time_t value.
# - string      who     some string identifying who modified the page last.

# refs
# - integer	from	id of the page containing the reference
# - integer	to	id of the referenced page

# Note II: The code here is able to maintain external copies for all
# pages (and all revisions) and an external log of changes. This
# functionality is activated by having a directory matching the value
# of the $WIKI_HIST environment variable.  The system only tracks dates
# and ip's inside the datafile for the recent page, not page contents.

package provide Wikit::Db 1.1
package require Wikit::Utils
package require Wikit::Format

namespace eval Wikit {
  # accessors
  namespace export pagevars Wiki WikiDatabase GetTitle GetPage pagevars pagevarsDB
  namespace import ::Wikit::Format::*

  # mutators
  namespace export SavePage SavePageDB DoCommit DoSync FixPageRefs
  
  variable readonly -1	;# use the file permissions
  
  # Code for opening, closing, locking, searching, and modifying views
  
  proc Wiki {name args} {
    if {$name == "-"} {
      set page [lindex [split [lindex $args 0] "/"] end]
      catch { set name [mk::get wdb.pages!$page name] }
    }
    link - [Wikit::Format::quote $name] [join $args ?]
  }
  
  proc WikiDatabase {name {db wdb}} {
    variable readonly
    variable wikifile $name

    if {[lsearch -exact [mk::file open] $db] == -1} {
      if {$readonly == -1} {
        if {[file exists $name] && ![file writable $name]} {
          set readonly 1
        } else {
          set readonly 0
        }
      }
      
      if {$readonly} {
        set flags "-readonly"
        set tst readable
      } else {
        set flags ""
        set tst writable
      }
      
      set msg ""
      if {[catch {mk::file open $db $name -nocommit $flags} msg] \
            && [file $tst $name]} {
        
        # if we can write and/or read the file but can't open
        # it using mk then it is almost always inside a starkit,
        # so we copy it to memory and open it from there
        
        set readonly 1
        mk::file open $db
        set fd [open $name]
        mk::file load $db $fd
        close $fd
        set msg ""
      }
      
      if {$msg != "" && ![string equal $msg $db]} {
        error $msg
      }
    }
    
    mk::view layout $db.pages	{name page date:I who}
    mk::view layout $db.refs	{from:I to:I}
    
    # if there are no references, probably it's the first time, so recalc
    if {!$readonly && [mk::view size $db.refs] == 0} {
      # this can take quite a while, unfortunately - but only once
      ::Wikit::FixPageRefs
    }
    
    # get rid of some old cruft, now that stored data has become incompatible
    mk::view layout $db.archive {} ;# get rid of old def
    catch { mk::view size $db.admin 0 }
    catch { mk::view size $db.scripts 0 }
    catch { mk::view size $db.archive 0 }
  }
  
  # get page info into specified var names
  proc pagevarsDB {db num args} {
    # mk::get returns an item, not a list, if given a single property name
    if {[llength $args] == 1} {
      uplevel 1 [list set $args [mk::get $db.pages!$num $args]]
    } else {
      foreach x $args y [eval mk::get $db.pages!$num $args] {
        uplevel 1 [list set $x $y]
      }
    }
  }
  
  proc pagevars {num args} {
    # mk::get returns an item, not a list, if given a single property name
    if {[llength $args] == 1} {
      uplevel 1 [list set $args [mk::get wdb.pages!$num $args]]
    } else {
      foreach x $args y [eval mk::get wdb.pages!$num $args] {
        uplevel 1 [list set $x $y]
      }
    }
  }

  proc GetTitle {id {db wdb}} {
    set title [mk::get $db.pages!$id name]
    return $title
  }
  
  proc GetPage {id {db wdb}} {
    switch $id {
      2		{ SearchResults [SearchList] }
      4		{ RecentChanges $db}
      default	{ return [mk::get $db.pages!$id page] }
    }
  }
  
  # addRefs - a newly created page $id contains $refs references to other pages
  # Add these references to the .ref view.
  proc addRefs {id db refs} {
    if {$id != 2 && $id != 4} {
      foreach x $refs {
        if {$id != $x} {
          mk::row append $db.refs from $id to $x
        }
      }
    }
  }
  
  # delRefs - remove all references from page $id to anywhere
  proc delRefs {id db} {
    set v [mk::select $db.refs from $id]	;# the set of all references from $id
    
    # delete from last to first
    set n [llength $v]
    while {[incr n -1] >= 0} {
      mk::row delete $db.refs![lindex $v $n]
    }
  }
  
  # FixPageRefs - recreate the entire refs view
  proc FixPageRefs {{db wdb}} {
    mk::view size $db.refs 0	;# delete all contents from the .refs view
    
    # visit each page, recreating its refs
    mk::loop c $db.pages {
      set id [mk::cursor position c]
      pagevarsDB $db $id date page
      if {$date != 0} {
        # add the references from page $id to .refs view
        addRefs $id $db [StreamToRefs [TextToStream $page] [list ::Wikit::InfoProc $db]]
      }
    }
    DoCommit
  }
  
  # Helper to 'SavePage'. Changes all references to page 'name'
  # contained in the 'text' into references to page 'newName'. This is
  # performed if a page changes its title, to keep all internal
  # references in sync. Only pages which are known to refer to the
  # changed page (see 'SavePage') are modified.
  
  proc replaceLink {text old new} {
    # this code is not fullproof, it misses links in keyword lists
    # this means page renames are not 100% accurate (but refs still are)
    
    set newText ""
    foreach line [split $text \n] {
      # don't touch quoted lines, except if its a list item
      if {![regexp "^\[ \t\]\[^\\*0-9\]" $line] ||
          [regexp "^(   |\t)\[ \t\]*(\\*|\[0-9\]\\.) " $line]} {
        #23nov02 jcw: this failed on title contents such as C++
        #regsub -all -nocase "\\\[$old\\\]" $line "\[$new\]" line
        set line [string map [list "\[$old\]" "\[$new\]"] $line]
      }
      lappend newText $line
    }
    join $newText \n
  }
  
  # SavePageDB - store page $id ($who, $text, $newdate)
  proc SavePageDB {db id text who newName {newdate ""}} {
    set changed 0
    
    pagevarsDB $db $id name page

    if {$newName != $name} {
      set changed 1
      
      # rewrite all pages referencing $id changing old name to new
      foreach x [mk::select $db.refs to $id] {
        set x [mk::get $db.refs!$x from]
        set y [mk::get $db.pages!$x page]
        mk::set $db.pages!$x page [replaceLink $y $name $newName]
      }
      
      # don't forget to adjust links in this page itself
      set text [replaceLink $text $name $newName]
      
      AdjustTitleCache $name $newName $id
      mk::set $db.pages!$id name $newName
    }
    
    if {$newdate != ""} {
      # change the date if requested
      mk::set $db.pages!$id date $newdate
    }
    
    # avoid creating a log entry and committing if nothing changed
    set text [string trimright $text]
    if {!$changed && $text == $page} return
    
    # make sure it parses before deleting old references
    set newRefs [StreamToRefs [TextToStream $text] [list ::Wikit::InfoProc $db]]
    delRefs $id $db
    addRefs $id $db $newRefs
    
    if {$id == 3} {
      catch { gButton::modify Help -text [lindex [Wikit::GetTitle 3] 0] }
    }
    
    mk::set $db.pages!$id page $text who $who
    
    if {$newdate == ""} {
      mk::set $db.pages!$id date [clock seconds]
      AddLogEntry $id $db
      DoCommit $db
    }
  }
  
  # SavePage - store page $id ($who, $text, $newdate)
  proc SavePage {id text who newName {newdate ""}} {
    return [SavePageDB wdb $id $text $who $newName $newdate]
  }

  # DoCommit - commit changes to the database
  proc DoCommit {{db wdb}} {
    mk::file commit $db
  }
  
  # DoSync - sync Wikit to the contents of a URL
  proc DoSync {url {db wdb}} {
    puts "Looking for changes at $url ..."
    package require http
    package require autoproxy
    autoproxy::init
    set re \
      "^Title:\\s+(\[^\n]+)\nDate:\\s+(\[^\n]+)\nSite:\\s+(\[^\n]+)\n\n(.*)"
    set index [graburl $url/index]
    if {[regexp {^0 \d+ \d+} $index]} {
      set i 0
      foreach {xpage xvers xdate} $index {
        if {$xpage >= [mk::view size $db.pages]} {
          mk::view size $db.pages [expr {$xpage+1}]
        }
        pagevarsDB $db $xpage date
        if {$date != $xdate} {
          puts -nonewline [format %6d $xpage]
          flush stdout
          set page [graburl $url/$xpage]
          if {[regexp $re $page - t d s p]} {
            puts "  $t - $d"
            SavePageDB $db $xpage $p $s $t $xdate
            if {[incr i] % 10 == 0} { DoCommit $db }
          } else {
            puts ?
          }
        }
      }
      DoCommit $db
      puts "Update done."
    } else {
      puts "No suitable index found, update ignored."
    }
  }
}

### Local Variables: ***
### mode:tcl ***
### tcl-indent-level:2 ***
### tcl-continued-indent-level:2 ***
### indent-tabs-mode:nil ***
### End: ***
