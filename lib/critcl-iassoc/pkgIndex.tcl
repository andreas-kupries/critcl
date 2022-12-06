if {![package vsatisfies [package provide Tcl] 8.6]} {return}
package ifneeded critcl::iassoc 1.2 [list source [file join $dir iassoc.tcl]]
