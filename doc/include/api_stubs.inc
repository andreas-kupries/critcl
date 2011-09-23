
Newly introduced with critcl version 3 is the support for stubs
tables, Tcl's dynamic linking mechanism handling the resolution of
symbols between C extensions.

We won't go into its details here. See [uri http://wiki.tcl.tk/285]
for an introduction in general, and section [sectref {Stubs Tables}]
for the details of critcl's particular variant.

[para] Critcl supports this via a single command, [cmd critcl::api],
and its methods.

[para] First, importing stubs tables, i.e. APIs, from another
extension:

[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::api] [method import] [arg name] [arg version]]

Critcl prepares the [file .critcl] file [emph and] its companion
[file .c] files by including the headers

[list_begin enumerated]
[enum] [file [var name]/[var name]Decls.h]
[enum] [file [var name]/[var name]StubLib.h]
[list_end]

in the appropriate places. It is checked that the compiler will be
able to find these header files somewhere on the include search
path, using the paths defined so far (See [cmd critcl::cheaders], and
the critcl application's [option -I] and [option -includedir]
options). Note how critcl expects the headers of package [var foo] to
reside in a sub-directory [file foo] of the known include search
paths.

[para] [emph Important:] If [var foo] is a namespaced package name,
like, for example "c::stack", then the namespace separators "::" are
converted into underscores ("_") in path names, C code, etc.

[para] The first header is expected to contain contains all the
necessary stubs table type declarations, mapping macros, etc., and may
include package specific headers (See [cmd {critcl::api header}]
below).

This header is included at the beginning of the C code backing the
[file .critcl] file, and at the beginning of all companion [file .c]
files. This means that the writer of these files doesn't have to write
the necessary [term {#include}] directory, critcl does it for them.

[para] The second header is expected to contain the stubs table
variable definition, and the C code, i.e. definition, of the function
to initialize it.

This, and a call to this initializer function are added to the
[file .critcl] file's initialization code.

[para] If the directory containing the aforementioned headers also
contains the file [file [var name]/[var name].decls] then it is
assumed that this file contains the external representation of the
stubs table used to generate the headers. The file is read and the
internal representation of the stubs table returned as result of the
command, for the importing package to use as it sees fit. If no such
file is present the command returns the empty string as its result.

[para] One possible use would be the automatic generation of C code
calling on the functions listed in the imported API.

[para] When generating a TEA wrapper the names of the imported APIs
are used to declare [syscmd configure] options with which the user can
declare a non-standard location for the headers of the API. Any API
[var FOO] is translated a single configure option
[option --with-[var FOO]-include].

[list_end]


[para] Second, declaration and export of a stubs table, i.e. API, for
the current package, [var foo]:

[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::api] [method function] [arg resulttype] [arg name] [arg arguments]]

This method declares that the function [arg name] is in the public API
of the package, and its signature (type of the result, number, names
and types of its arguments).

Using this method automatically causes critcl to generate both the
code for a stubs table in the package, the headers needed by packages
using this API, and a [file .decls] file containing the stubs table
implied by the exports, usable by [cmd {critcl::api import}].

[para] [arg arguments] is a list of C types and associated argument
names. Like a dictionary, except that keys (argument names) and values
(argument types) are swapped. The [arg resulttype] is a C type as well.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::api] [method header] [opt [arg pattern]...]]

This method notifies critcl of companion header files which have to be
exported together with the generated stubs headers.

[para] All arguments are interpreted as glob pattern and the matching
files are copied into the directory containing the generated headers
well. As an importing package uses only [file [var foo]Decls.h] to
access the API this generated header will contain the necessary
[term {#include}] directives to make these companion header files and their
declarations available too.

Patterns matching no file or non-existing files cause the command to
throw an error.

[para] [emph Note] that patterns which are not beginning with an
absolute path are interpreted relative to the directory containing the
current [file .critcl] file.

[comment {
	I am intentionally not documenting "critcl::api export".
	I am not convinced yet that this methis is needed.
	The default, using the package name as the stubs table
	library and interface names seems to me to be not only
	reasonable, but the only setting truly needed. I simply
	do not see a use case for having the library and interface
	named different than the package.
	(In a bundle, like tcllibc each bundled package still declares
	itself properly).
}]

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::api] [method extheader] [opt [arg file]...]]

This method is similar [cmd {::critcl::api header}], in that it
notifies critcl of companion header files which have to be exported
together with the generated stubs headers.

[para] The difference is that these headers will be expected to exist
in the external development environment.

As such they will be [term {#include}]d in the generated header for
the package, but not copied to the package header directory. Nor are
they allowed to be glob patterns, as critcl has no context, i.e
directory, in which to expand such patterns.

[list_end]

Note that the generated headers for an exported API are included in
the package like it is done when importing it somewhere else. To repeat:

[para] The [file [var foo]Decls.h] header is included at the beginning
of the C code backing the [file .critcl] file, and at the beginning of
all companion [file .c] files. This means that the writer of these
files doesn't have to write the necessary [term {#include}] directory,
critcl does it for them.

[para] In mode "compile & run" the generated header files, and their
companion headers, if any, are placed in the subdirectory [file foo]
of the [sectref {Result Cache}]. As this location is implicitly added
to the include search path any other package importing this API and
and build in mode "compile & run" as well will find the these headers.

[para] For mode "generate package" the application was extended with a
new option [option -includedir] which specifies the location to place
the generated headers in (again in subdirectory [file foo] of that
path). This path is also be added to the include search paths,
ensuring that a package importing an API will find it if the package
exporting that API used the same setting for [option -includedir].

[para] For mode "generate TEA" the static scanner was extended to
recognize [cmd {critcl::api header}] as a source of companion files.
It further uses data about [cmd {critcl::api import}] commands to put
proper support for [option --with-[var foo]-include] options into the
generate [file configure(.in)] so that a user may specify custom
locations for the headers of any imported API.
