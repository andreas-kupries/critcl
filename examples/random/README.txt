Example of critcl-based packages.

A larger example written to demonstrate

	Export of an API as Tcl stubs table	(Package "rnmath")
	Import of an API defined as stubs-table	(Package "random")

Package "random" incidentially also demonstrates

	Programmatic access to the imported API for use in C code
	generation, and

	Complex C code generation (class and instance commands) from a
	few parameters (function name, arguments, ...).

Sources
	Package	"rnmath":	rnmath.tcl
	Package "random":	random.tcl

Notes:

*	"rnmath" contains and exports low-level functions for
	generating random numbers following a variety of
	distributions. The functions are state-less. The system
	function random() (On win: rand()) is used internally as the
	basic source of random values.

*	"random" wraps the low-level math into a Tcl class/object
	system where generators are objects, each represented as a Tcl
	command.
