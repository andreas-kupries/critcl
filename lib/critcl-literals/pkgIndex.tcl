if {![package vsatisfies [package provide Tcl] 8.6]} {return}
package ifneeded critcl::literals 1.4 [list source [file join $dir literals.tcl]]
