if {![package vsatisfies [package provide Tcl] 8.6]} {return}
package ifneeded critcl::emap 1.3 [list source [file join $dir emap.tcl]]
