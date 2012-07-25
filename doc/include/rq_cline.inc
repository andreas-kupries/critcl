
[para] Side note: This facility requires the use of a tclsh supporting
the builtin [cmd {info frame}] command. If critcl is run by a tclsh
not supporting this no [term #line] directives will be emitted. The
command is supported by Tcl 8.5 and higher. It is also supported by
Tcl 8.4 provided that it was compiled with the define
[option -DTCL_TIP280]. An example of such is ActiveState's ActiveTcl.
