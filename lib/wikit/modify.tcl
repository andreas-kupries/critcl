# These are the main WiKit database modification routines

package provide Modify 1.0

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
#
# Note II: The code here is able to maintain external copies for all
# pages (and all revisions) and an external log of changes. This
# functionality is activated by having a directory matching the value
# of the $WIKI_HIST environment variable.  The system only tracks dates
# and ip's inside teh datafile for the recent page, not page contents.

proc AddRefs {id refs} {
  foreach x $refs {
    if {$id != 2 && $id != 4 && $id != $x} {
      mk::row append wdb.refs from $id to $x
    }
  }
}

proc DelRefs {id} {
  set v [mk::select wdb.refs from $id]
  # delete from last to first
  set n [llength $v]
  while {[incr n -1] >= 0} {
    mk::row delete wdb.refs![lindex $v $n]
  }
}

proc FixPageRefs {} {
  mk::view size wdb.refs 0
  mk::loop c wdb.pages {
    set id [mk::cursor position c]
    pagevars $id date page
    if {$date != 0} {
      AddRefs $id [StreamToRefs [TextToStream $page] InfoProc]
    }
  }
  DoCommit
}

# Helper to 'SavePage'. Changes all references to page 'name'
# contained in the 'text' into references to page 'newName'. This is
# performed if a page changes its title, to keep all internal
# references in sync. Only pages which are known to refer to the
# changed page (see 'SavePage') are modified.

proc ReplaceLink {text old new} {
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

proc SavePage {id text who newName {newdate ""}} {
  set changed 0
  pagevars $id name page
  
  if {$newName != $name} {
    set changed 1
  
      # rename old names to new in all referencing pages
    foreach x [mk::select wdb.refs to $id] {
      set x [mk::get wdb.refs!$x from]
      set y [mk::get wdb.pages!$x page]
      mk::set wdb.pages!$x page [ReplaceLink $y $name $newName]
    }
    
      # don't forget to adjust links in this page itself
    set text [ReplaceLink $text $name $newName]

    AdjustTitleCache $name $newName $id
    mk::set wdb.pages!$id name $newName
  }
  
  if {$newdate != ""} {
    mk::set wdb.pages!$id date $newdate
  }

    # avoid creating a log entry and committing if nothing changed
  set text [string trimright $text]
  if {!$changed && $text == $page} return
  
  # make sure it parses before deleting old references
  set newRefs [StreamToRefs [TextToStream $text] InfoProc]
  DelRefs $id
  AddRefs $id $newRefs
  
  if {$id == 3} {
    catch { gButton::modify Help -text [lindex [Wikit::GetTitle 3] 0] }
  }
  
  mk::set wdb.pages!$id page $text who $who

  if {$newdate == ""} {
    mk::set wdb.pages!$id date [clock seconds]
    AddLogEntry $id
    DoCommit
  }
}

# Enters an external log entry for a changed into the changelog view if an
# external $env(WIKI_HIST) directory is present.  The names of the files for
# the changed pages contain their id and also data and changing entity. The
# date is encoded as time_t value. Sorting the filenames alpahabetically will
# allow other applications to created reports and diffs between revisions.

proc AddLogEntry {id} {
  upvar #0 env(WIKIT_HIST) ewh
  set fmt {%e %b %Y %H:%M:%S GMT}

  pagevars $id date page who name
  
  if {[info exists ewh] && [file isdirectory $ewh]} {
    set t [string trim $page]
    if {$t != ""} {
      set fd [open $ewh/$id-$date-$who w]
      puts $fd "Title:\t$name"
      puts $fd "Date:\t[clock format $date -gmt 1 -format $fmt]"
      puts $fd "Site:\t$who"
      puts $fd ""
      puts $fd $t
      close $fd
    }
    set fd [open $ewh/.index a]
    puts $fd [list x $id $date $who $name]
    close $fd
  }

  #mk::row append wdb.archive id $id name $name date $date who $who
}
