
The package provides five commands to declare various types of C code
fragments. These are:

[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::ccode] [arg text]]

This command compiles the C code in [arg text] and makes the
contained definitions (variables, functions, macros, etc.) available
to all C code fragments specified after it. It itself can assume to
have access to all definitions which were specified before it.

See section [sectref {Runtime Behaviour}] for more details.

[para]
The result of the command is the empty string.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::ccommand] [arg tclname] [arg cfunname]]

This command creates a new Tcl command named [arg tclname] which is
implemented by the C function [arg cfunname].

It is expected that [arg cfunname] has the proper signature for a Tcl
command function, and was declared already.

[para] The result of [cmd ::critcl::ccommand] itself is the empty
string.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::ccommand] [arg tclname] [arg arguments] [opt "[arg option] [arg value]..."] [arg body]]

This form of [cmd critcl::ccommand] creates a new Tcl command named
[arg tclname] which is implemented by the C code in [arg body].

[para] The command wraps the [arg body] in an invisible C function,
compiles it and makes the resulting definition available to all C code
fragments declared later on. It itself can assume to have access to
all definitions which came before it.

See section [sectref {Runtime Behaviour}] for more details.

[para] The result of [cmd critcl::ccommand] itself is the empty
string.

[para] The list of [arg arguments] contain the names for the four
parameters required by a Tcl command function. Superfluous list
elements (i.e. beyond the fourth) are ignored. Missing elements
(parameters), and empty parameter names are handled by replacing them
with standard names. These are, in order of usage

[list_begin enumerated]
[enum] [const clientdata]
[enum] [const interp]
[enum] [const objc]
[enum] [const objv]
[list_end]

The only options accepted by this command are:

[list_begin options]
[opt_def -clientdata [arg c-expression]]

The value of this option is the text of a single C expression. The
value of expression is used in the generated C statement registering
[arg tclname] to initialize the client data of the new Tcl command.

If not specified the expression defaults to [const NULL], i.e. no
client data.

[opt_def -delproc [arg c-expression]]

The value of this option is the text of a single C expression. The
value of this expression has to be a function pointer of type
"Tcl_CmdDeleteProc", which is used in the generated C statement
registering [arg tclname] to initialize a deletion function for the
new Tcl command, i.e. a function which is run by Tcl when the Tcl
command is deleted again.

If not specified the expression defaults to [const NULL], i.e. no
deletion function.

[list_end]

A ccommand is, in comparison to functions defined via
[cmd critcl::cproc], more lower level.

Its advantage is that the developer [emph can] do their own argument
processing, enabling things like variable number of arguments,
options, etc., i.e. much higher flexibility.

Their disadvantage is that you [emph have] to do your own argument
processing. Where a [cmd critcl::cproc] generates the code to convert
from Tcl values to C values and back a [cmd critcl::ccommand] forces
the writer to do all of this on their own. I.e. the cost of the
aforementioned flexibility is a higher complexity seen by the user.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cdata] [arg tclname] [arg data]]

This command a new Tcl command named [arg tclname] which returns
[arg data] as a ByteArray result.

[para] The result of [cmd critcl::cdata] itself is the empty string.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cdefines] [arg definitions] [opt [arg namespace]]]

This command creates Tcl variables in the specified [arg namespace]
which are linked to the [term {C enum values}] and [term #define]s
named as [term {glob patterns}] in the list of [arg definitions]. Each
variable has the same name as the definition which gave rise to it,
and its value is the value of the corresponding enum value or
#define. The namespace defaults to the global namespace, i.e. "::", if
it wasn't specified explicitly.

[para] Please note that this command is only for the lifting of
[emph existing] C definitions into Tcl. The command [emph {does not}]
create the definitions in C. It actually goes so far to check for the
presence of the named definitions and not performing the mapping for
any which do not exist. Which is sensible, given that non-existing
defines have no value which could be used in the mapping.

[para] As these checks are run at the time the embedded C code of a
[file .critcl] file is actually compiled they have access to and check
[emph all] C fragments defined with [cmd critcl::ccode], plus all the
headers it has access to via [cmd critcl::cheaders], for that file.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cproc] [arg name] [arg arguments] [arg resulttype] [arg body]]

This command creates a new Tcl command named [arg tclname] which is
implemented by the C code in [arg body].

In contrast to the low-level [cmd critcl::ccommand] here the arguments
and result are typed and critcl generates the code converting from
Tcl_Obj's to C data types, and vice versa.

The command creates two invisible C functions, one wrapping the
[arg body], the other a shim containing the necessary conversions,
compiles them and makes the resulting definitions available to all C
code fragments declared later on. It itself can assume to have access
to all definitions which came before it.

See section [sectref {Runtime Behaviour}] for more details.

[para] The result of [cmd critcl::cproc] itself is the empty string.

The following values are allowed for the [arg resulttype]:

[example {
          Types
Name      C        Tcl      Notes
-------   -------- -------- -------------------
Tcl_Obj*  Tcl_Obj* Tcl_Obj* Default, pass-through
char*     char*    String   Static string
vstring   char*    String   String is volatile.
string    char*    String   Dynamically allocated (Tcl_Alloc)
dstring   char*    String   Ditto.
-------   -------- -------- -------------------
double    double   Double
float     float    Double
-------   -------- -------- -------------------
bool      int      Int
boolean   int      Int
int       int      Int
long      long     Long
-------   -------- -------- -------------------
ok        int      ---      Result code, pass-through
void      void     ---      No result.
-------   -------- -------- -------------------
}]

[para] Please note that it is possible to extend the above with custom
types if these types are not enough. See section
[sectref {Advanced: Extending cproc}] for details.

[para] The [arg arguments] parameter has the overall syntax of a Tcl
dictionary value, except that keys (argument names) and values
(argument types) are specified in reverse order. Consider the
example [example {
int x int y
}] where [arg x] and [arg y] are the arguments/keys, both
mapped to type/value [const int].

The following values are allowed as argument-types:

[example {
          Types
Name      C        Tcl      Notes
-------   -------- -------- -------------------
Tcl_Obj*  Tcl_Obj* Tcl_Obj* Pass-through
bytearray char*    ByteArray
char*     char*    String
double*   double*           Pointer into ByteArray representation ?!
double    double   Double
float*    float*            Pointer into ByteArray representation ?!
float     float    Double
int*      int*              Pointer into ByteArray representation ?!
bool      int      Boolean
boolean   int      Boolean
int       int      Int
long      long     Long
rawchar*  char*    ByteArray
void*     void*             Default
-------   -------- -------- -------------------
}]

[para] [emph Note] that optional arguments are not possible. This
restriction is inherited from C.

[para] Further note that the type of the first argument is allowed to
be [const Tcl_Interp*]. In that case the argument in question is
[emph not] counted as an argument of the new Tcl command.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cproc] [arg name] [arg arguments] [arg resulttype]]

This variant of [cmd critcl::cproc] assumes that the functionality to
connect is implemented by the C function [arg name] which has the
signature described by the [arg arguments] and [arg resulttype].

[para] It creates only the shim performing the conversions required by
arguments and result.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cinit] [arg text] [arg externals]]

This command compiles the C code in [arg text] and [arg externals].

[para] Both have access to all definitions created by the previously
listed commands, regardless of their and its placement in the
[file .critcl] file. See section [sectref {Runtime Behaviour}] for
more details.

[para] The C code in [arg text] is put into the body of the
initialization function of the shared library backing the
[file .critcl] file, and is executed when this library is loaded into
the interpreter.

[para] The code in [arg externals] on the other hand is placed outside
and just before the initialization function, making this is a good
place for any external symbols required by initialization function
which should not be accessible by any other parts of the C code.

[para]
The result of the command is the empty string.

[comment ---------------------------------------------------------------------]
[list_end]
