[comment {-*- mode: tcl ; fill-column: 90 -*-}]
[section {Changes for version 3.1.18.2}][comment {to be 3.2}]

[strong {Not Released Yet}]

[list_begin enumerated]

[enum] [strong {Bug Fix}] Issue
       [uri https://github.com/andreas-kupries/critcl/issues/125 #125].
       Added missing method [method create] in object creation example
       of installer documentation.

[enum] [strong Feature]. Extended [cmd cproc] argument type processing. Now able to
       auto-create restricted scalar types. I.e. types derived from [term int], etc. and
       limited in the range of allowed values.

[para] Further able to auto-create restricted list types, i.e. types derived from
       [term list] and either limited in length, or in the type of the elements, or both.

[enum] [strong {Bug Fix / Enhancement}] Issue
       [uri https://github.com/andreas-kupries/critcl/issues/118 #118].  Modified
       [cmd critcl::cproc] to accept C syntax for arguments, i.e.  Trailing comma on
       argument names, leading comma on type names, and lone comma characters.

[enum] [strong {Bug Fix}]
       Issue [uri https://github.com/andreas-kupries/critcl/issues/115 #115].

[para] [strong BREAKING] Distributions using [syscmd build.tcl] for installation of critcl
       in their packaging scripts have to be updated to the changed command signature of
       [syscmd build.tcl] install, etc. See the details below.

[para] Redone the argument handling for [method install], [method uninstall], and

       [method targets]. The destination argument is gone.  All commands now take options
       similar to what is known from GNU [syscmd configure], i.e.

[list_begin options]
[opt_def --prefix path]
[opt_def --exec-prefix path]
[opt_def --bin-dir path]
[opt_def --lib-dir path]
[opt_def --include-dir path]
[list_end]

[para] They now also respect the environment variable [var DESTDIR], and the associated
       option [option --dest-dir].

[para] The [option --prefix] defaults to the topdir from the bin directory holding the

       [syscmd tclsh] running [syscmd build.tcl].  As Tcl command: [example {
    file dirname [file dirname [info nameofexecutable]]
}]

[para] Added a command [method dirs] doing the same argument handling, for debugging.

[enum] [strong {Performance Fix}] for [term {compile & run}] mode.
       Issue [uri https://github.com/andreas-kupries/critcl/issues/112 #112].

[para] Moved the command activating more precise code location tracking out of package
       [package critcl] into package [package critcl::app].

[para] Because generating packages ahead of time can bear the performance penalty invoked
       by this [strong global] setting.

[para] Arbitrary libraries and applications using critcl dynamically
       ([term {compile & run}]) on the other hand likely cannot, and should not.

[enum] [strong Fix]
       Issue [uri https://github.com/andreas-kupries/critcl/issues/109 #109].

       Ignore [syscmd clang] suffices when computing the target identifier from a
       configuration identifier.

[enum] [strong Feature]. Bumped package [package critcl::cutil] to version 0.2.1.
       Simplified the implementation of macro [cmd ASSERT] by taking the underlying
       [fun Tcl_Panic]'s printf ability into account and added a new macro [cmd ASSERT_VA]
       exporting the same ability to the user.

[list_end]
