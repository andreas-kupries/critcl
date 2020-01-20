#!/usr/bin/env tclsh

package require Tcl 8.5
package require ingress

# Main point is checking that all the conversions of all
# (non-deprecated) builtin argument types generate code which compiles
# without errors or even warnings.
#
# The command names match the type names.

ex boolean   true
ex int       0
ex long      0
ex wideint   0

ex double    0
ex float     0

ex char*     string	;# const!?
ex pstring   pstring
ex bytes     bytearray

ex bytearray bytearray ;# deprecated

ex Tcl_Obj*  an-object
ex list      {a list}

ex channel   stdout

foreach t {int long wideint double float} {
    foreach {r v} {
	{> 0} 1	    {>= 0} 0	    {> 1} 2	    {>= 1} 1
	{< 0} -1    {<= 0} 0	    {< 1} 0	    {<= 1} 1
    } {
	set type "$t $r"
	ex $type $v
    }
}
