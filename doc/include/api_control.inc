
The package provides thirteen commands to control the details of
compilation and linking, enabling [file .critcl] files to provide
custom information about their environment and dependencies.

[para] In important thing to note about all these commands is that the
package manages their information on a per-file basis.

I.e. information provided by and in a file [file FOO.tcl] is kept
separate from the information provided by and in a file
[file BAR.tcl], preventing them from interfering with each other.

[para]
The commands are:

[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cheaders] [opt [arg arg]...]]

This command provides the compile step with additional header files
and header locations.

[para] All arguments matching the glob pattern [const -*] are
forwarded to the compiler's command line when it is invoked for the
current [file .critcl] file.

[para] All other arguments are interpreted as glob pattern and the
matching files are made available to the compiler when it is invoked
for the current [file .critcl] file. Patterns matching no file or
non-existing files cause the command to throw an error.

[para] [emph Note] that patterns which are not beginning with an
absolute path are interpreted relative to the directory containing the
current [file .critcl] file.

[para] [emph Note] further that this declaration does not cause the
specified header files to be [const {#include}]'d automatically. This
still has to be done via [cmd critcl::ccode] where necessary.

It does simply ensure that the compiler will be able to find these
files when invoked, by providing the necessary command line flags
extending the compiler's search paths.

[para] Multiple invocations of this command accumulate their
information.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::csources] [opt [arg pattern]...]]

This command provides the compile step with additional C source files.

[para] All arguments are intepreted as glob patterns. Patterns
matching no file or non-existing files cause the command to throw an
error. The files matching the patterns are made available to the
compiler when it is invoked for the current [file .critcl] file. This
means that the files in question are compiled together with the
[file .c] file backing the [file .critcl] file into a single object.

[para] [emph Note] that patterns which are not beginning with an
absolute path are interpreted relative to the directory containing the
current [file .critcl] file.

[para] Multiple invocations of this command accumulate their
information.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::clibraries] [opt [arg arg]...]]

This command provides the link step with additional libraries to link
and library locations.

[para] All arguments matching the glob pattern [const -*] are
forwarded to the linkers's command line when it is invoked for the
current [file .critcl] file.

[para] All other arguments are interpreted glob patterns. Patterns
matching no file or non-existing files cause the command to throw an
error. The files matching the patterns are made available to the
linker when it is invoked for the current [file .critcl] file. This
means that the files in question are linked together with the object
file backing the [file .critcl] file into a single shared library.

[para] [emph Note] that patterns which are not beginning with an
absolute path are interpreted relative to the directory containing the
current [file .critcl] file.

[para] Multiple invocations of this command accumulate their
information.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::source] [arg path]]

This command evaluates the critcl commands in the file specified by
[arg path] in the context of the current [file .critcl] file.

[para] The argument is actually considered as glob pattern and all
matching files are evaluated. A pattern matching no file or
non-existing files cause the command to throw an error.

[para] [emph Note] that a pattern not beginning with an absolute path
is interpreted relative to the directory containing the current
[file .critcl] file.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::tsources] [arg pattern]...]

This command provides the critcl package with information about
additional Tcl script files to source when the shared library is
loaded.

[para] All arguments are considered as glob patterns and the matching
files are made available to generated shared library when it is loaded
for the current [file .critcl] file. Patterns matching no file or
non-existing files cause the command to throw an error.

[para] [emph Note] that patterns which are not beginning with an
absolute path are interpreted relative to the directory containing the
current [file .critcl] file.

[para] Multiple invocations of this command accumulate their
information.

[para] The declared files are sourced after the shared library has
been loaded, in the same order they were provided to [cmd critcl::tsources].

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::owns] [arg pattern]...]

This command is ignored by the regular build modes, i.e. both "compile
and run", and "generate package". It is present to support the static
code scanner of critcl v3's new mode to "generate TEA" packages.

[para] In that situation it provides the critcl package with
information about any files which have to be wrapped and could not be
figured out from the previous commands (i.e. [cmd critcl::csources],
[cmd critcl::tsources]) because of getting specified dynamically, or
getting directly sourced and this not visible to critcl in any other
way.


[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cflags] [opt [arg arg]...]]

This command provides the compile step with additional compiler flags.

[para] All arguments are forwarded to the compiler's command line when
it is invoked for the current [file .critcl] file.

[para] Multiple invocations of this command accumulate their
information.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::ldflags] [opt [arg arg]...]]

This command provides the link step with additional linker flags.

[para] All arguments are forwarded to the linkers's command line when
it is invoked for the current [file .critcl] file.

[para] Multiple invocations of this command accumulate their
information.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::framework] [opt [arg arg]...]]

This command provides the link step with the names of additional
frameworks to link on MacOS X. The command is ignored if we are not
building for OS X. This means that it is possible to declare the OS X
specific frameworks unconditionally. The package itself takes care to
not use them when building for non-OS X platforms.

[para] All arguments are forwarded to the linkers's command line when
it is invoked for the current [file .critcl] file.

[para] Multiple invocations of this command accumulate their
information.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::tcl] [arg version]]

This command tells critcl for what minimum version of the Tcl runtime
to compile and link the package for. If not specified critcl falls
back to the default of [const 8.4]. 

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::tk]]

This command informs critcl that the package in question is based on
Tk, and therefore needs the Tk headers for compilation, and the Tk
stubs for linking.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::preload] [arg lib]...]

This command arranges that the named dependent external shared library is loaded
before the generated package's shared library.

[para] Multiple invocations of this command accumulate their
information.

[para][include preload_search.inc]

[comment {
	XXX preload - compile & run - will it work ?
}]

If you are a developer wishing to understand or modify the internals
of the [package critcl] package then you possibly should read the
section explaining how the [sectref {Preloading functionality}] is
implemented.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::debug] [arg area]...]

This tells critcl if the package is to be compiled for debugging, and
which areas to activate. Internally each area is translated into
area-specific flags for the compiler which are then handed over to
[cmd critcl::cflags].

[list_begin definitions]
[def [const memory]]

Specification of this area activates Tcl memory debugging
for the package code.

[def [const symbols]]

Specification of this area activates compilation and linking with
debugging symbols, for use by a debugger or other tool.

[def [const all]]

Specification of this area translates ino the activation of all other
legal areas.

[list_end]
[list_end]
