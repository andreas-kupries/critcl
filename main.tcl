if {![catch {
  # kit related main:
  package require starkit
}]} {
  if {[starkit::startup] == "sourced"} return
} else {
  # direct invoke without kit (sourced/debug/dev-edition):
  lappend ::auto_path [file join [file dirname [info script]] lib]
}
package require critcl::app 3
#puts [package ifneeded critcl [package require critcl::app 3]]
critcl::app::main $argv
