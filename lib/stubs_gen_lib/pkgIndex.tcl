if {![package vsatisfies [package provide Tcl] 8.6]} {return}
#checker -scope global exclude warnUndefinedVar
package ifneeded stubs::gen::lib 1.1 [list source [file join $dir gen_lib.tcl]]
