package require starkit
if {[starkit::startup] == "sourced"} return
package require critcl::app 3
#puts [package ifneeded critcl [package require critcl::app 3]]
critcl::app::main $argv
