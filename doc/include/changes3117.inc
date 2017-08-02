[section {Changes for version 3.1.17}]
[list_begin enumerated]

[enum] Bugfix: Fixed issue #75.
       Extended [cmd critcl::include] to now take multiple paths.

[enum] Added new compatibility package [package lmap84].

[enum] Fixed typos in various documentation files.

[enum] Fixed bug introduced by commit 86f415dd30 (3.1.16 release). The
       separation of [cmd critcl::ccode] into user and work layers
       means that location retrieval has to go one more level up to
       find the user location.

[enum] New supporting package [package critcl::cutil]. Provides common
       C level facilities useful to packages (assertions, tracing,
       memory allocation shorthands).

[enum] Extended the support package [package critcl::literals] with
       limited configurability. It is now able to generate code for
       C-level access to the pool without Tcl types (Mode [const c]).
       The previously existing functionality is accesssible under mode
       [const tcl], which also is the default. Both modes can be used
       together.

[enum] Extended the support package [package critcl::emap] with
       limited configurability. It is now able to generate code for
       C-level access to the mapping without Tcl types
       (Mode [const c]). The previously existing functionality is
       accessible under mode [const tcl], which also is the
       default. Both modes can be used together.

[list_end]
