[section {Changes for version 3.2.1 (to come)}]
[list_begin enumerated]

[enum] Modified packages to accept operation under Tcl 9.
       Bumped package versions appropriately.
       Bumped copyrights.

[enum] Set different minimum Tcl requirements for the 8.x and 9.x series.

[enum] Made [file ~]-handling portable across the 8.x and 9 boundary
       (via [const HOME] environment variable).

[enum] Bumped embedded [package tclconfig] to version 2.67. Patch supplied by Paul Obermeier.

[enum] [strong {Bug Fix}] [uri https://github.com/andreas-kupries/critcl/issues/127 #127]
[enum] [strong {Bug Fix}] [uri https://github.com/andreas-kupries/critcl/issues/128 #128]
[enum] [strong {Bug Fix}] [uri https://github.com/andreas-kupries/critcl/issues/129 #129]

Fixed various typos in the documentation.

[enum] Reworked internals of [package critcl::cutil]'s tracer to support operation in a
       multi-threaded environment. This new mode is also default. The old single-threaded
       mode can be (re)activated by defining [const CRITCL_TRACE_NOTHREADS].

[para] Package bumped to version 0.4.

[list_end]
