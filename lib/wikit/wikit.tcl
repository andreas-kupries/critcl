# Set up for embedded use

package provide Wikit 1.0
package require Tk
package require Wikit::Utils
package require Wikit::Format

if {[catch {package require Wikit::Gui}]} {
    puts stderr "cannot load Wikit::Gui"
    exit 1
}

namespace eval Wikit {
    variable readonly -1

    namespace import ::Wikit::Format::*

    # if called via Wikit::init (i.e. via a package) then default to readonly

    proc init {db {ro 1} {topwin ""} {page ""}} {
      variable readonly
      set readonly $ro
      WikiDatabase $db
      if {$page == "" \
            || [set id [mk::select wdb.pages -count 1 name $page]] == ""} {
        set id 0
      }
      LocalInterface $topwin $id
   }
} 
