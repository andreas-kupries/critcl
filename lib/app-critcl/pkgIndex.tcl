if {![package vsatisfies [package provide Tcl] 8.6]} {return}
package ifneeded critcl::app 3.2 [list source [file join $dir critcl.tcl]]
