[section {Changes for version 3.1.16}]
[list_begin enumerated]

[enum] New feature. Extended [cmd critcl::cproc]'s argument handling
       to allow arbitrary mixing of required and optional arguments.

[enum] New feature.
       [emph {Potential Incompatibility}].
[para] Extended [cmd critcl::cproc]'s argument handling to treat an
       argument [const args] as variadic if it is the last argument of
       the procedure.

[enum] New feature. Added two introspection commands,
       [cmd critcl::has-argtype] and [cmd critcl::has-resulttype].
       These enable a user to test if a specific (named) type
       conversion is implemented or not.

[enum] Bugfix: Fixed problem with the implementation of issue #54 (See
       3.1.14). Always create the secondary log file. Otherwise
       end-of-log handling may break, unconditionally assuming its
       existence.

[enum] Bugfix: Fixed problem with the internal change to the hook
       [const HandleDeclAfterBuild]. Corrected the forgotten
       [cmd critcl::cconst].

[enum] Debugging aid: Added comment holding the name of the result
       type when emitting result conversions.

[list_end]
