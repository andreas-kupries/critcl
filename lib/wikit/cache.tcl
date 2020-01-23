# Wikit Cache - Caches page titles in an array, to speed access
#
# BuildTitleCache - traverse pages and cache their titles
# AdjustTitleCache - change a title in the cache
# LookupPage - look up a title in the cache

package provide Wikit::Cache 1.0
package require Wikit::Db

namespace eval Wikit {
  namespace export BuildTitleCache AdjustTitleCache

  variable titleCache	;# an array mapping name to id

  # AdjustTitleCache for renamed page
  proc AdjustTitleCache {name newName id {db wdb}} {
    variable titleCache
    set name [string tolower $name]
    if {[info exists titleCache($name)]} {
      unset titleCache($db,$name)
    }
    set titleCache($db,[string tolower $newName]) $id
  }

  # BuildTitleCache by traversing all pages and caching their title
  proc BuildTitleCache {{db wdb}} {
    variable titleCache
    mk::loop c $db.pages {
      set title [mk::get $c name]
      if {[info exists titleCache($db,[string tolower $title])]} {
        #puts stderr "duplicate page! [mk::cursor position c] -> $title"
        #puts stderr [mk::get $c page]
      } else {
        set titleCache($db,[string tolower $title]) [mk::cursor position c]
      }
    }
  }

  # LookupPage - find a name in the titleCache
  proc LookupPage {name {db wdb}} {
    variable titleCache

    set lcname [string tolower $name]
    if {[info exists titleCache($db,$lcname)]} {
      set n $titleCache($db,$lcname)
    } else {
      set n [mk::select $db.pages -count 1 name $name]
      set titleCache($db,$lcname) $n
    }

    if {$n == ""} {
      set n [mk::view size $db.pages]
      mk::set $db.pages!$n name $name
      set titleCache($db,$lcname) $n
      DoCommit $db
    }

    return $n
  }
}
