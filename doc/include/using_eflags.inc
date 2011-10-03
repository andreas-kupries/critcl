[subsection {Customizing the compile and link steps}]

The commands [cmd critcl::cflags] and [cmd critcl::ldflags] enable you
to provide custom options to the compile and link phases for a
[file .critcl] file.

[para] This usually becomes necessary if the C code in question comes
from an external library we are writing a Tcl binding for, with
multiple configurations to select, non-standard header locations, and
other things.

Among the latter, especially platform-specific settings, for example
byteorder.

[para] This makes [cmd critcl::check] an important adjunct command, as
this is the API for [sectref {Checking The Environment}], and then
selecting the compile & link flags to use.

[para]
[emph {I currently have no specific example to demonstrate these commands.}]
