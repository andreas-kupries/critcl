[subsection {Finding header files}]

A notable thing in the example shown in the section about
[sectref {Blocks of arbitrary C}] is the

[example {
    #include "sha1.h"
}]

statement. Where does this header come from ?

Looking at the Tcllib module we will find that the header is actually
a sibling to the [file sha1c.tcl] file containing the embedded C code.

However, [package critcl] does not know that. It has to be told.
While without that knowledge it will invoke the compiler just fine,
the compilation will fail because the header is not on the include
paths used by the compiler, and therefore will not be found.

[para]

For this we have the [cmd critcl::cheaders] command. It enables us to
either tell the compiler the path(s) where the required headers can be
found, using

[example {
    critcl::cheaders -I/path/to/headers/
}]

or to tell it directly which headers we are using and where they live:

[example {
    critcl::cheaders sha1.h
}]

And now critcl knows that [file sha1.h] is important, and that it
lives besides the [file .critcl] file which referenced it (because of
the relative path used).

[emph Note] that this doesn't absolve us of the need to "#include" the
header through a [cmd critcl::ccode] block. This only tells critcl
where it lives so that it can configure the compiler with the proper
include paths to actually find it on use.

[para] Further note that a C development environment is usually
configured to find all the system headers, obviating the need for a
[cmd critcl::cheaders] declaration when such are used. For these a
plain "#include" in a [cmd critcl::ccode] block is good enough.

In other words, the second form of invoking [cmd critcl::cheaders] is
pretty much only for headers which accompany the [file .critcl] file.

[para] Note that it is possible to mix all the possible forms both
separately, and in a single command. Further note that the information
from multiple invokations is aggregated by the system.

[example {
    critcl::cheaders -I/path/to/headers/ sha1.h
}]

[example {
    critcl::cheaders -I/path/to/crc/headers/
    critcl::cheaders sha1.h
}]
