
The package provides a single command for the management of global
settings, i.e. configuration options which are independent of any
[file .critcl] file. 

[para] It is expected that this command is irrelevant to anybody just
wishing to write a [file .critcl] file. It is a management command
which is only useful to the [manpage {CriTcl Application}] or similar
tools.

[list_begin definitions]

[call [cmd ::critcl::config] [arg option] [opt [arg val]]]

This command sets and returns critcl's global configuration options. These are

[list_begin options]
[opt_def force bool]

This flag tells the package whether it should force the building of C
files despite having a cached shared library (when [const true], or
not. The default is [const off].

[opt_def lines bool]

This flag tells the package whether to embed #line directives into the
generated C code (when [const true]) or not. By default this is [const on].

[include rq_cline.inc]

[para] Developers of higher-level packages generating their own C
code, either directly, or indirectly, by using critcl commands, should
also read section [sectref {Advanced: Location management}] to see how
critcl helps them in generating their directives.

Examples of such packages come with critcl itself, see the packages
[package critcl::iassoc] and [package critcl::class].

[opt_def I path]

A single global include path to use for all files. Not set by default.

[opt_def combine enum]
[list_begin definitions]
[def [const dynamic]]

Object files have the suffix [const _pic].

[def [const static]]

Object files have the suffix [const _stub].

[def [const standalone]]

Object files have no suffix, and the generated C files are compiled
without using Tcl/Tk stubs. The result are object files usable for
static linking into a [term {big shell}].

[list_end]

The default is [const dynamic].


[opt_def language string]
[opt_def keepsrc bool]

This flag tells the package whether to keep the generated [file .c]
files after it has build their [file .o] files (when [const true]), or
not. The default is [const off].

[opt_def outdir path]

The path where to place a generated shared library. Not set by
default, causing placement into the [sectref {Result Cache}].

[list_end]
[list_end]
