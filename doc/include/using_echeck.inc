[subsection {Checking The Environment}]

... may be necessary when creating a binding to some third-party
library. The headers for this library may be found in non-standard
locations, ditto for the library/ies itself. We may not have the
headers and/or library on the build host. Types with
platform-dependent sizes and definitions. Endianness issues. Any number
of things.

[para] TEA-based packages can use [syscmd autoconf] and various
predefined macros to deal with all this.

We have the [term {Power Of Tcl (tm)}] and [cmd critcl::check].

[para] This command takes a piece of C code as argument, like
[cmd critcl::ccode]. Instead of saving it for later it however tries
to compile it immediately, using the current settings, and then
returns a boolean value reporting on the success ([const true]) or
failure ([const false]). From there we can then branch to different
declarations.

[para] As example let us check for the existence of some header
[file FOO.h]:

[example {
    package require critcl

    if {[critcl::check {
        #include <FOO.h>
    }]} {
        ... Code for when FOO.h is present.
    } else {
        ... Code for when FOO.h is not present.
    }
}]

Should we, on the other hand, wish to search for the header ourselves,
in non-standard locations we have the full power of Tcl available,
i.e. loops, the [cmd file] and [cmd glob] commands, etc., which can
then be followed by a [cmd critcl::cheader] command to declare the
location we found (See also [sectref {Finding header files}]).

[para] A nice extension to critcl would be a package collecting
pocedures for common tasks like that, sort of like an
[syscmd autoconf] for Tcl. [package critcl::config] seems to be nice
name for such a package.

[para] Obvious adjunct commands which can be driven by results from
[cmd critcl::check] are

[list_begin definitions]
[def [cmd critcl::cflags]]
[def [cmd critcl::cheaders]]
[def [cmd critcl::clibraries]]
[def [cmd critcl::framework]]
[def [cmd critcl::ldflags]]
[list_end]

[para] Less obvious, yet still valid are also

[list_begin definitions]
[def [cmd critcl::ccode]]
[def [cmd critcl::ccommand]]
[def [cmd critcl::cdata]]
[def [cmd critcl::cproc]]
[def [cmd critcl::csources]]
[def [cmd critcl::ctsources]]
[list_end]

and pretty much everything else you can imagine.
