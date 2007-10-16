# Wikit Utilities
#
# RecentChanges - return the Recent Changes special page
# InfoProc - decode references from a page
# graburl - grab the contents of a Url
# AddLogEntry - create an external log entry for a page change

package provide Wikit::Utils 1.0
package require Wikit::Cache

namespace eval Wikit {
  namespace export InfoProc
  
  # Special page: Recent Changes.
  proc RecentChanges {{db wdb}} {
    set count 0
    set result ""
    set lastDay 0
    set threshold [expr {[clock seconds] - 7 * 86400}]
    
    foreach id [mk::select $db.pages -rsort date] {
      lassign [mk::get $db.pages!$id date name who] date name who
      
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
        append result "'''[clock format $date -gmt 1 -format {%B %e, %Y}]'''\n"
      }
      
      append result "   * \[$name\] . . . $who\n"
    }
    
    return $result
  }
  
  # InfoProc - get a page for the reference
  # Used for rendering Wiki pages in HTML and as styled text in Tk
  proc InfoProc {db ref} {
    set id [::Wikit::LookupPage $ref $db]
    pagevarsDB $db $id date name
    
    if {$date == 0} {
      append id @ ;# enter edit mode for missing links
    } else {
      #append id .html
    }
    
    return [list /$id $name $date]
  }
  
  
  # graburl - grab the contents of a URL
  proc graburl {url} {
    set result ""
    set token [::http::geturl $url -binary 1 -timeout 10]
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
    ::http::cleanup $token
    return $result
  }
  
  # Enters an external log entry for a changed into the changelog view if an
  # external $env(WIKI_HIST) directory is present.  The names of the files for
  # the changed pages contain their id and also data and changing entity. The
  # date is encoded as time_t value. Sorting the filenames alpahabetically will
  # allow other applications to created reports and diffs between revisions.
  
  proc AddLogEntry {id db} {
    upvar #0 env(WIKIT_HIST) ewh
    set fmt {%e %b %Y %H:%M:%S GMT}
    
    pagevarsDB $db $id date page who name
    
    if {[info exists ewh] && [file isdirectory $ewh]} {
      set t [string trim $page]
      if {$t != ""} {
        set fd [open $ewh/$id-$date-$who w]
	fconfigure $fd -encoding utf-8 ;# 2006-01-12 ticket #1
        puts $fd "Title:\t$name"
        puts $fd "Date:\t[clock format $date -gmt 1 -format $fmt]"
        puts $fd "Site:\t$who"
        puts $fd ""
        puts $fd $t
        close $fd
      }
      set fd [open $ewh/.index a]
      fconfigure $fd -encoding utf-8 ;# 2006-01-12 ticket #1
      puts $fd [list x $id $date $who $name]
      close $fd
    }
    
    #mk::row append $wdb.archive id $id name $name date $date who $who
  }
}
