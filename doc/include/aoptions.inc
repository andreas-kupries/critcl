
The following options are understood

[list_begin options]
[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -v]
[opt_def --version]

These options cause critcl to print its version to [const stdout] and
then exit.

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -I path]

This option specifies an additional global include path to use during
compilation of [file .critcl] files. All values are used if this is
specified multiple times.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -L path]

This option specifies an additional global library searh path to use
during linking of [file .critcl] files. All values are used if this is
specified multiple times.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -cache path]

This option specifies the path to the directory to use as the result
cache. If not specified it defaults to [file ~/.critcl/<platform>],
or, when generating a package (see option [option -pkg] below), to
[file ~/.critcl/<pid>.<epoch>],

When specified multiple times the last value is used.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -clean]

When specified the result cache is emptied, i.e. all files and
directories found inside are deleted) before compilation begins.

[para] This option is irrelevant when generating a package (see option
[option -pkg] below) because this mode starts out with a unique and
empty result cache.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -config path]

This option specifies the path to a custom configuration file,
allowing the user to use their own target specifications. If not
specified a hardwired default configuration embedded in the system
core is used instead.

When specified multiple times the last value is used.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -debug mode]

This option activates compilation with debugging. It accepts the modes
below.

When specified multiple times the combination of all modes is used.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[list_begin definitions]
[def [const memory]]

This mode activates memory debugging of allocations made through the
Tcl core.

[def [const symbols]]

This mode activates building of all [file .c] files with debugging
symbols.

[def [const all]]

This mode activates both [const memory] and [const symbols].

[list_end]

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -disable name]

This option sets the value of the custom build configuration option
[arg name] to [const false]. It is equivalent to "-with-[arg name] 0".

[para] The information is validated only if one of the [file .critcl]
input files actually defines and uses a custom build configuration
option with that [arg name].

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -enable name]

This option sets the value of the custom build configuration option
[arg name] to [const true]. It is equivalent to "-with-[arg name] 1".

[para] The information is validated only if one of the [file .critcl]
input files actually defines and uses a custom build configuration
option with that [arg name].

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -force]

When specified compilation is always done, even if a shared library
for the file exists already. This effect can be had through cleaning
the cache (see above) as well, except that it is lazy in the
destruction of files and will not destroy files unrelated to the ones
we are building.

[para] This option is irrelevant when generating a package (see option
[option -pkg] below) because this mode starts out with a unique and
empty result cache.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -help]

This option will cause the system to print a short help about command
line syntax and options and then exit the application.

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -keep]

This option will cause the system to keep the [file .c] files
generated by a run in the result cache.

When generating a package (see option [option -pkg] below) this also
prevents the deletion of the unique result cache used by the run.

This option is intended for the debugging of [cmd critcl] itself,
where it may be necessary to inspect the generated C code.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -libdir path]

This option specifies the path under which the packages generated via
option [option -pkg] are saved. It also specifies a path to search
libraries in, like for [option -L].

When specified multiple times the last value is used.

When not specified at all the default, [file lib], is used. Note how
this is a relative path, placing the result into the current working
directory.

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -includedir path]

This option specifies the path under which any generated package
headers are saved. It also specifies a path to search include files
in, like for [option -I].

When specified multiple times the last value is used as destination,
however all previous values are kept on the include search path.

When not specified at all the default, [file include], is used. Note
how this is a relative path, placing the result into the current
working directory.

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -pkg]

The default mode of the application is to build the [file .critcl]
files listed on the command line and save the results in the result
cache. Essentially pre-filling the cache with important packages,
cutting down on the time needed to use these packages.

[para]

This option activates one of the other modes, package generation.

In this mode the input files are processed first as usual, however
after that they are bundled into a single library and additional files
are generated to make this library usable as a regular Tcl package.

[para]

The option [option -tea], see below, invokes the last mode, TEA
generation. If both options, i.e. [option -pkg] and [option -tea] are
specified the last one specified wins.

[para]

In this mode the options [option -clean] and [option -force] are
irrelevant and ignored. In contrast, the option [option -libdir] is
relevant in both this and [option -tea] mode.

[para]

When this option is specified the basename of the first file argument
after the options is used as the name of the package to generate. If
the extension of that file indicates a shared library ([file .so],
[file .sl], [file .dylib], and [file .dll]) it is also removed from
the set of input files. A [file .tcl] file is kept as part of the
input. A single file without extension is assumed to actually have a
[file .tcl] extension. A file without extension, but other input files
following is treated like the name of a shared library proper, and
removed from the set of input files.

[para] Examples:

[example {
	... -pkg ... foo

	=> Package name is: foo
	=> Input file is:   foo.tcl
}]

[example {
	... -pkg ... foo bar.tcl

	=> Package name is: foo
	=> Input file is:   bar.tcl
}]

[example {
	... -pkg ... foo.tcl

	=> Package name is: foo
	=> Input file is:   foo.tcl
}]

[example {
	... -pkg ... foo.so bar.tcl

	=> Package name is: foo
	=> Input file is:   bar.tcl
}]

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -show]

This option, when specified, will cause the system to print the
configuration of the chosen target to [const stdout] and then exit.

The choice of target can be influenced through the option
[option -target] (see below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -showall]

This option, when specified, will cause the system to print the whole
chosen configuration file to [const stdout] and then exit.

The choice of configuration file can be influenced through the option
[option -config] (see above).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -target name]

This option overrides the default choice of build target with
the user's choice.

When specified multiple times the last value is used.

The named target must exist in the chosen configuration file.

Use option [option -targets] (see below) to get a list of the
acceptable targets.

The choice of configuration file can be influenced through the option
[option -config] (see above).

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] below).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -targets]

This option, when specified, will cause the system to print the list
of all known targets from the chosen configuration file to
[const stdout] and then exit.

The choice of configuration file can be influenced through the option
[option -config] (see above).

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -tea]

Similar to option [option -pkg], except that the invoked mode does not
generate binaries, but a directory hierarchy containing the
[file .critcl] file, its companion files, and a TEA-lookalike build
system with most of the needed support code (incliding copies of the
critcl packages).

[para]

If both options, i.e. [option -pkg] and [option -tea] are specified
the last one specified wins.

[para]

In this mode the options [option -I], [option -L], [option -clean],
[option -force], [option -cache], [option -includedir], [option -enable],
[option -disable], and [option -with-[var FOO]] are irrelevant and
ignored. In contrast, the option [option -libdir] is relevant in both
this and [option -pkg] mode.

[para]

When this option is specified the basename of the first file argument
after the options is used as the name of the package to generate. If
the extension of that file indicates a shared library ([file .so],
[file .sl], [file .dylib], and [file .dll]) it is also removed from
the set of input files. A [file .tcl] file is kept as part of the
input. A single file without extension is assumed to actually have a
[file .tcl] extension. A file without extension, but other input files
following is treated like the name of a shared library proper, and
removed from the set of input files.

[para] Examples:

[example {
	... -tea ... foo

	=> Package name is: foo
	=> Input file is:   foo.tcl
}]

[example {
	... -tea ... foo bar.tcl

	=> Package name is: foo
	=> Input file is:   bar.tcl
}]

[example {
	... -tea ... foo.tcl

	=> Package name is: foo
	=> Input file is:   foo.tcl
}]

[example {
	... -tea ... foo.so bar.tcl

	=> Package name is: foo
	=> Input file is:   bar.tcl
}]

[comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
[opt_def -with-[var name] value]

This option sets the value of the custom build configuration option
[arg name] to [arg value].

[para] The information is validated only if one of the [file .critcl]
input files actually defines and uses a custom build configuration
option with that [arg name].

[para] This option is irrelevant when generating a TEA package (see
option [option -tea] above).

[list_end]
