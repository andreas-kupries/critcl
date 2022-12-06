if {![package vsatisfies [package provide Tcl] 8.6]} {return}
package ifneeded critcl::util 1.2 [list source [file join $dir util.tcl]]
