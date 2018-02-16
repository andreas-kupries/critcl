[subsection {Constant values}]

While one might believe that there is no need for commands which
returns constant values that is not true. Commands reporting on
compile-time configuration, like version numbers, available features,
etc. are at least one use case for such commands.

[para] The reason for creating critcl commands to support them ?
Convenience to the user, yes, but further than that, the ability to
optimize the internals, i.e. the generated code.

[para] A [cmd cproc] would be easy to write, but incurs overhead due
to a superfluous work function. A [cmd ccommand] has no overhead,
except that of the user having to write the argument checking and
result conversion.

[para] Using [cmd critcl::cconst] is both convenient and without code
overhead.

Our example is a function found in package [package tcl-linenoise],
that is, if [cmd cconst] had existed at the time of writing. It
returns a configuration value reporting to the policy layer if an
extended mode for hidden input is available from the bound
[package linenoise], or not.

[example {
    critcl::cconst linenoise::hidden_extended boolean 1
}]
