if {![package vsatisfies [package provide Tcl] 8.6 9]} {return}
package ifneeded critcl 3.3 [list source [file join $dir critcl.tcl]]
