if {![package vsatisfies [package provide Tcl] 8.6]} {return}
package ifneeded critclf  0.2 [list source [file join $dir critclf.tcl]]
package ifneeded wrapfort 0.2 [list source [file join $dir wrapfort.tcl]]
