[section {Changes for version 3.1.9 to be}]
[list_begin enumerated]

[enum]	Fixed issue #28. Added missing -mXX flags for linking at the
	linux-{32,64}-* targets.

[enum]	Fixed issue #27. Added missing platform definitions for
	various alternate linux and OS X targets.

[enum]	Fixed issue #32. Merged pull request by Andrew Shadoura.
	Various typos in documentation and comments.

[enum]	Fixed the issue behind rejected pull request #30 by Andrew
	Shadoura. Dynamically extract the stubs variable declarations
	from the Tcl header files and generate matching variable
	definitions for use in the package code. The generated code
	will now be always consistent with the headers, even when
	critcl's own copy of them is replaced by system headers.

[list_end]
