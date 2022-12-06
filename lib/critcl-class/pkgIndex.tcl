if {![package vsatisfies [package provide Tcl] 8.6]} {return}
package ifneeded critcl::class 1.2 [list source [file join $dir class.tcl]]
