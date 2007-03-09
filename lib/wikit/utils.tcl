# This used to be called cli.tcl, now moved into Wikit

package provide Wikit::Utils 1.0

namespace eval Wikit {
  variable readonly -1  ;# use the file permissions

  # needed because non-readonly mode hasn't been moved into namespace
  namespace export pagevars InfoProc DoCommit Wiki GetPage \
			BuildTitleCache AdjustTitleCache

  # get page info into specified var names
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

  # Code for opening, closing, locking, searching, and modifying views
  # Comment: nah, quite a messy collection of loose ends, really.

  proc DoCommit {} {
    mk::file commit wdb
  }

  proc AcquireLock {lockFile {maxAge 900}} {
    for {set i 0} {$i < 60} {incr i} {
      catch {
        set fd [open $lockFile]
        set opid [gets $fd]
        close $fd
        if {$opid != "" && ![file exists [file join / proc $opid]]} {
	  file delete $lockFile
	  set fd [open savelog.txt a]
	  set now [clock format [clock seconds]]
	  puts $fd "# $now drop lock $opid -> [pid]"
	  close $fd
        }
      }
      catch {close $fd}

      if {![catch {open $lockFile {CREAT EXCL WRONLY}} fd]} {
        puts $fd [pid]
        close $fd
        return 1
      }
      after 1000
    }

      # if the file is older than maxAge, we grab the lock anyway
    if {[catch {file mtime $lockFile} t]} { return 0 }
    return [expr {[clock seconds] > $t + $maxAge}]
  }

  proc ReleaseLock {lockFile} {
    file delete $lockFile
  }

  proc AdjustTitleCache {name newName id} {
    global titleCache
    set name [string tolower $name]
    if {[info exists titleCache($name)]} {
      unset titleCache($name)
      set titleCache([string tolower $newName]) $id
    }
  }
    
  proc BuildTitleCache {} {
    global titleCache
    mk::loop c wdb.pages {
      set title [mk::get $c name]
      if {[info exists titleCache([string tolower $title])]} {
        #puts stderr "duplicate page! [mk::cursor position c] -> $title"
        #puts stderr [mk::get $c page]
      } else {
	  set titleCache([string tolower $title]) [mk::cursor position c]
      }
    }
  }

  proc LookupPage {name} {
    if {[regexp {^[0-9]+$} $name]} {
      set n $name
    } else {
      global titleCache
      set lcname [string tolower $name]
      if {[info exists titleCache($lcname)]} {
        set n $titleCache($lcname)
      } else {
        set n [mk::select wdb.pages -count 1 name $name]
      }
    }
    if {$n == ""} {
      set n [mk::view size wdb.pages]
      mk::set wdb.pages!$n name $name
      DoCommit
    }
    return $n
  }

  proc GetTimeStamp {{t ""}} {
    if {$t == ""} { set t [clock seconds] }
    clock format $t -gmt 1 -format {%Y/%m/%d %T}
  }

  # Code for opening, closing, locking, searching, and modifying views

  proc WikiDatabase {name} {
    global tcl_version
    variable readonly
    if {[lsearch -exact [mk::file open] wdb] == -1} {
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
        if {[catch {mk::file open wdb $name -nocommit $flags} msg] \
                && [file $tst $name]} {
            # if we can write and/or read the file but can't open
            # it using mk then it is almost always inside a starkit,
	    # so we copy it to memory and open it from there
            set readonly 1
            mk::file open wdb
            set fd [open $name]
            mk::file load wdb $fd
            close $fd
            set msg ""
        }
        if {$msg != "" && ![string equal $msg wdb]} {
            error $msg
        }
        mk::view layout wdb.pages   {name page date:I who}
        #mk::view layout wdb.archive {name date:I who id:I}
        mk::view layout wdb.refs    {from:I to:I}
    }
    # if there are no references, probably it's the first time, so recalc
    if {!$readonly && [mk::view size wdb.refs] == 0} {
      # this can take quite a while, unfortunately - but only once
      FixPageRefs
    }
    # get rid of some old cruft, now that stored data has become incompatible
    catch { mk::view size wdb.admin 0 }
    catch { mk::view size wdb.scripts 0 }
    catch { mk::view size wdb.archive 0 }
  }

  proc GetPage {id} {
    switch $id {
      2   	{ SearchResults [SearchList] }
      4   	{ RecentChanges }
      default 	{ return [mk::get wdb.pages!$id page] }
    }
  }

  proc Wiki {name args} {
    if {$name == "-"} {
      catch { set name [mk::get wdb.pages![lindex $args 0] name] }
    }
    link - [Wikit::Format::quote $name] [join $args ?]
  }

  # Special page: Recent Changes.

  proc RecentChanges {} {
    set count 0
    set result ""
    set lastDay 0
    set threshold [expr {[clock seconds] - 7 * 86400}]
    
    foreach id [mk::select wdb.pages -rsort date] {
      lassign [mk::get wdb.pages!$id date name who] date name who
      
        # these are fake pages, don't list them
      if {$id == 2 || $id == 4} continue
      
        # only report last change to a page on each day
      set day [expr {$date/86400}]
      
        #insert a header for each new date
      incr count
      if {$day != $lastDay} {
          # only cut off on day changes and if over 7 days reported
        if {$count > 100 && $date < $threshold} {
          append result "''Older entries omitted...''"
          break
        }

        set lastDay $day
        append result "'''[clock format $date -gmt 1 \
                -format {%B %e, %Y}]'''\n"
      }
      
      append result "   * \[$name\] . . . $who\n"        
    }
    
    return $result
  }

  set searchKey ""
  set searchLong 0

  proc SearchList {} {
    variable searchKey
    variable searchLong
    
    if {$searchKey == ""} {return ""}
    
    set fields name
    if {$searchLong} {
      lappend fields page
    }
    
    return [mk::select wdb.pages -rsort date -keyword $fields $searchKey]
  }
    
  proc SearchResults {rows} {
    variable searchKey
    variable searchLong

    # tclLog "SearchResults key <$searchKey> long <$searchLong>"
    if {$searchKey == ""} {return ""}
    
    set count 0

    set result "Searched for \"'''$searchKey'''\" (in page titles"
    if {$searchLong} {
      append result { and contents}
    }
    append result "):\n\n"
    
    foreach i $rows {
      pagevars $i date name
      
        # these are fake pages, don't list them
      if {$i == 2 || $i == 4 || $date == 0} continue
      
      incr count
      if {$count > 100} {
        append result "''Remaining matches omitted...''"
        break
      }

      append result "   * [GetTimeStamp $date] . . . \[$name\]\n"        
    }
    
    if {$count == 0} {
      append result "   * '''''No matches found'''''\n"
    }
    
    if {!$searchLong} {
      append result "\n''Tip: append an asterisk\
                to search the page contents as well as titles.''"
    }
    
    return $result
  }

  # Rendering Wiki pages in HTML and as styled text in Tk

  proc InfoProc {ref} {
    set id [LookupPage $ref]
    pagevars $id date name

    if {$date == 0} {
      append id @ ;# enter edit mode for missing links
    } else {
      #append id .html
    }
    
    return [list /$id $name $date]
  }

  proc Expand_HTML {str} {
    StreamToHTML [TextToStream $str] $::env(SCRIPT_NAME) InfoProc
  }

  proc GetTitle {id} {
      set title [mk::get wdb.pages!$id name]
      return $title
      # the following allows links in titles to be followed - originally
      # used for following a Page 3 link for the Help button. Not used now
      # (regarded as feeping creaturism) but left here in case
      set ref [StreamToRefs [TextToStream $title] InfoProc]
      if {$ref == ""} {
          set ref $id
      } else {
          set title [mk::get wdb.pages!$ref name]
      }
      return [list $ref $title]
  }

  proc ScanForInlinedImages {} {
    array set inlined {}
    mk::loop c wdb.pages {
      set id [mk::cursor position c]
      pagevars $id page
      set urls [StreamToUrls [TextToStream $page]]
      if {[llength $urls] > 0} {
	foreach {url type} $urls {
	  if {$type eq "ref" && [regexp {\.(gif|jpg|png)$} $url]} {
	    lappend inlined($url) $id
	  }
	}
      }
    }
    array get inlined
  }

  proc graburl {url} {
    set result ""
    set token [::http::geturl $url]
    upvar #0 $token state
    if {$state(status) == "ok"} {
      #puts "http: $state(http)"
      if {[string match *404* $state(http)]} {
	#puts "not found: $url"
	return ""
      }
      set result $state(body)
    } else {
      #puts "grab? $state(status)"
      return ""
    }
    ::http::reset $token
    return $result
  }

  proc LocalImages {cmd} {
    mk::view layout wdb.images {url date:I image:B}
    switch -- $cmd {
      clear {
        mk::view size wdb.images 0
      }
      scan {
	puts "Scanning [mk::view size wdb.pages] wiki pages..."
	array set scanned [ScanForInlinedImages]
	set dels 0
	set adds 0
	set obsolete {}
	mk::loop c wdb.images {
	  set url [mk::get $c url]
	  if {[info exists scanned($url)]} {
	    unset scanned($url)
	  } else {
	    set obsolete [linsert $obsolete 0 $c]
	    incr dels
	  }
	}
	foreach x $obsolete { mk::row delete $x }
	foreach x [array names scanned] {
	  mk::row append wdb.images url $x
	  incr adds
	}
	puts "$adds urls added, $dels urls deleted"
      }
      list {
	puts "[mk::view size wdb.images] image cache entries:"
	foreach x [mk::select wdb.images -sort url] {
	  set s [mk::get wdb.images!$x -size image]
	  set d [clock format [mk::get wdb.images!$x date] -format "%Y/%m/%d"]
	  if {$s == 0} {
	    set d " (absent) "
	    set s ""
	  }
	  puts [format {%10s  %s  %s} $s $d [mk::get wdb.images!$x url]]
	}
      }
      get {
	set todo [mk::select wdb.images date 0]
	puts "Fetching [llength $todo] images..."
	package require http
	set count 0
	foreach x $todo {
	  array set f [mk::get wdb.images!$x]
	  puts -nonewline "  $f(url) ... "
	  flush stdout
	  if {[catch {graburl $f(url)} y]} {
	    puts "\n\t$y"
	  } elseif {$y eq ""} {
	    puts FAILED
	  } elseif {[string length $y] > 102400} {
	    puts "OMITTED > 100 Kb"
	  } else {
	    puts OK
	    mk::set wdb.images!$x date [clock seconds] image $y
	    if {[incr count] % 10 == 0} { mk::file commit wdb }
	  }
	}
      }
      local {
	set n 0
	mk::loop c wdb.images {
	  set url [mk::get $c url]
	  set lfn [file tail $url]
	  if {[file exists $lfn]} { 
	    set fd [open $lfn]
	    fconfigure $fd -translation binary
	    set data [read $fd]
	    close $fd
	    mk::set $c image $data date [file mtime $lfn]
	    incr n
	  }
	}
	puts "$n local images copied"
      }
      default {
        puts stderr "bad -images command, must be one of:\
				scan, list, get, local, or clear"
	exit 1
      }
    }
  }

  proc DoSync {url} {
    puts "Looking for changes at $url ..."
    package require http
    set re \
      "^Title:\\s+(\[^\n]+)\nDate:\\s+(\[^\n]+)\nSite:\\s+(\[^\n]+)\n\n(.*)"
    set index [graburl $url/index]
    if {[regexp {^0 \d+ \d+} $index]} {
      set i 0
      foreach {xpage xvers xdate} $index {
	if {$xpage >= [mk::view size wdb.pages]} {
	  mk::view size wdb.pages [expr {$xpage+1}]
	}
        pagevars $xpage date
	if {$date != $xdate} {
	  puts -nonewline [format %6d $xpage]
	  flush stdout
	  set page [graburl $url/$xpage]
	  if {[regexp $re $page - t d s p]} {
	    puts "  $t - $d"
	    SavePage $xpage $p $s $t $xdate
	    if {[incr i] % 10 == 0} DoCommit
	  } else {
	    puts ?
	  }
	}
      }
      DoCommit
      puts "Update done."
    } else {
      puts "No suitable index found, update ignored."
    }
  }
}
