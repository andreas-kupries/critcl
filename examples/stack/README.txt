Example of critcl-based packages.

A larger example written to demonstrate

	Export of an API as Tcl stubs table	(Package "cstack")
	Import of an API defined as stubs-table	(Package "stackc")

	Easy writing of C classes, with class and instances
	represented as commands, through the utility package
	critcl::class

Package "cstack" also demonstrate the export of a companion header
file containing declarations of package specific data structures and
macros which are not expressible in the basic .decls file and header
derived from it, plus, incidentally, the separation of public and
internal headers.

Package "stackc" incidentially also demonstrates the use of companion
.c and .h files in the implementation of a package.

Sources
	Package	"cstack":	cstack.tcl, cstack.c, cstack.h, cstackInt.h
	Package "stackc":	stackc.tcl, stackc/*.[ch]

Notes:

*	"cstack" implements an abstract stack data type and exports
	a C-level API for it, as a stubs table.

*	"stackc" wraps the abstract stack data type of "cstack" into
	a Tcl class/object system where stacks are objects, each
	represented as a Tcl command.
