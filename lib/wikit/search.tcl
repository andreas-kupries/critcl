# Wikit search interface
#
# SearchList - return a list of search results specified by searchKey and searchLong
# SearchResults - format a search specified by searchKey and searchLong
# GetTimeStamp - return a formatted clock time

package provide Wikit::Search 1.0

namespace eval Wikit {
  namespace export SearchList SearchResults GetTimeStamp

  variable searchKey ""
  variable searchLong 0

  proc SearchList {{db wdb}} {
    variable searchKey
    variable searchLong

    if {$searchKey == ""} {
      return ""
    }

    set fields name
    if {$searchLong} {
      lappend fields page
    }

    return [mk::select $db.pages -rsort date -keyword $fields $searchKey]
  }

  proc GetTimeStamp {{t ""}} {
    if {$t == ""} { set t [clock seconds] }
    clock format $t -gmt 1 -format {%Y/%m/%d %T}
  }

  proc SearchResults {rows {db wdb}} {
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
      pagevarsDB $db $i date name

      # these are fake pages, don't list them
      if {$i == 2 || $i == 4 || $date == 0} continue

      # ignore "near-empty" pages with at most 1 char, 30-09-2004
      if {$i != 8 && [mk::get $db.pages!$i -size page] <= 1} continue

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
}
