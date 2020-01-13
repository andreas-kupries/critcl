[comment {
	 Standard argument types for use with
	 cproc and cclass methods.
}]
[list_begin definitions]
[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]

[def Tcl_Interp*]

[strong Attention]: This is a [strong special] argument type. It can
[strong only] be used by the [strong first] argument of a function.
Any other argument using it will cause critcl to throw an error.

[para] When used, the argument will contain a reference to the current
interpreter that the function body may use. Furthermore the argument
will [strong not] be an argument of the Tcl command for the function.

[para] This is useful when the function has to do more than simply
returning a value. Examples would be setting up error messages on
failure, or querying the interpreter for variables and other data.

[def Tcl_Obj*]
[def object]

The function takes an argument of type [type Tcl_Obj*].
No argument checking is done.
The Tcl level word is passed to the argument as-is.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def pstring]

The function takes an argument of type [type critcl_pstring]
containing the original [type Tcl_Obj*] reference of the Tcl argument,
plus the length of the string and a pointer to the character array.

[example {
typedef struct critcl_pstring {
    Tcl_Obj* o;
    char*    s;
    int      len;
} critcl_pstring;
}]

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def list]

The function takes an argument of type [type critcl_list] containing
the original [type Tcl_Obj*] reference of the Tcl argument, plus the
length of the Tcl list and a pointer to the array of the list
elements.

[example {
typedef struct critcl_list {
    Tcl_Obj*  o;
    Tcl_Obj** v;
    int       c;
} critcl_list;
}]

The Tcl argument must be convertible to [type List], an error is
thrown otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def bytearray]
[def rawchar*]
[def rawchar]

The function takes an argument of type [type char*].

The Tcl argument must be convertible to [type ByteArray], an error is
thrown otherwise.

[emph Note] that the length of the [type ByteArray] is [emph not]
passed to the function, making this type not very usable.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def bytes]

This is the [emph new] and usable [type ByteArray] type.

[para] The function takes an argument of type [type critcl_bytes]
containing the original [type Tcl_Obj*] reference of the Tcl argument,
plus the length of the byte array and a pointer to the byte data.

[example {
typedef struct critcl_bytes {
    Tcl_Obj*             o;
    const unsigned char* s;
    int                len;
} critcl_list;
}]

The Tcl argument must be convertible to [type ByteArray], an error is
thrown otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def char*]

The function takes an argument of type [type char*].
The string representation of the Tcl argument is passed in.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def double]

The function takes an argument of type [type double].

The Tcl argument must be convertible to [type Double], an error is
thrown otherwise.

[def {double > 0}]
[def {double >= 0}]
[def {double < 0}]
[def {double <= 0}]
[def {double > 1}]
[def {double >= 1}]
[def {double < 1}]
[def {double <= 1}]

These are variants of [term double] above, restricting the argument
value to the shown relation. An error is thrown for Tcl arguments
outside of the specified range.

[emph Note:] This is not a general range specification syntax. Only
the listed types exist.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def float]

The function takes an argument of type [type float].

The Tcl argument must be convertible to [type Double], an error is
thrown otherwise.

[def {float > 0}]
[def {float >= 0}]
[def {float < 0}]
[def {float <= 0}]
[def {float > 1}]
[def {float >= 1}]
[def {float < 1}]
[def {float <= 1}]

These are variants of [term float] above, restricting the argument
value to the shown relation. An error is thrown for Tcl arguments
outside of the specified range.

[emph Note:] This is not a general range specification syntax. Only
the listed types exist.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def boolean]
[def bool]

The function takes an argument of type [type int].

The Tcl argument must be convertible to [type Boolean], an error is
thrown otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def int]

The function takes an argument of type [type int].

The Tcl argument must be convertible to [type Int], an error is thrown
otherwise.

[def {int > 0}]
[def {int >= 0}]
[def {int < 0}]
[def {int <= 0}]
[def {int > 1}]
[def {int >= 1}]
[def {int < 1}]
[def {int <= 1}]

These are variants of [term int] above, restricting the argument value
to the shown relation. An error is thrown for Tcl arguments outside of
the specified range.

[emph Note:] This is not a general range specification syntax. Only
the listed types exist.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def long]

The function takes an argument of type [type {long int}].

The Tcl argument must be convertible to [type Long], an error is
thrown otherwise.

[def {long > 0}]
[def {long >= 0}]
[def {long < 0}]
[def {long <= 0}]
[def {long > 1}]
[def {long >= 1}]
[def {long < 1}]
[def {long <= 1}]

These are variants of [term long] above, restricting the argument
value to the shown relation. An error is thrown for Tcl arguments
outside of the specified range.

[emph Note:] This is not a general range specification syntax. Only
the listed types exist.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def wideint]

The function takes an argument of type [type Tcl_WideInt].

The Tcl argument must be convertible to [type WideInt], an error is
thrown otherwise.

[def {wideint > 0}]
[def {wideint >= 0}]
[def {wideint < 0}]
[def {wideint <= 0}]
[def {wideint > 1}]
[def {wideint >= 1}]
[def {wideint < 1}]
[def {wideint <= 1}]

These are variants of [term wideint] above, restricting the argument
value to the shown relation. An error is thrown for Tcl arguments
outside of the specified range.

[emph Note:] This is not a general range specification syntax. Only
the listed types exist.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def void*]

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def double*]
[def float*]
[def int*]

The function takes an argument of the same-named C type.

The Tcl argument must be convertible to ByteArray, an error is thrown
otherwise.

The bytes in the ByteArray are then re-interpreted as the raw
representation of a single C pointer of the given type which is then
passed as argument to the function.

In other words, this is for Tcl values somehow holding raw C pointers,
i.e. memory addresses.

[para] [emph Attention]: These types are considered [emph DEPRECATED].
It is planned to remove their documentation in release 3.2, and their
implementation in release 3.3.  Their deprecation can be undone if
good use cases are shown.

[list_end]
