package require starkit
if {[starkit::startup] == "sourced"} return
package require critcl::app
critcl::app::main $argv
