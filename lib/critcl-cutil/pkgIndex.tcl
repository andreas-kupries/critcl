if {![package vsatisfies [package provide Tcl] 8.6]} {return}
package ifneeded critcl::cutil 0.3 [list source [file join $dir cutil.tcl]]
