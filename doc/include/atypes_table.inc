[comment {
  Table of the standard argument types, for a quick overview
  of the names, associated C types, behaviour and limits.
}]
[example_begin]
Critcl type      | C type         | Tcl type  | Notes
---------------- | -------------- | --------- | ------------------------------
Tcl_Interp*      | Tcl_Interp*    | n/a       | [strong Special], only first
---------------- | -------------- | --------- | ------------------------------
Tcl_Obj*         | Tcl_Obj*       | Any       | [strong Read-only]
object           |                |           | Alias of [type Tcl_Obj*] above
list             | critcl_list    | List      | [strong Read-only]
---------------- | -------------- | --------- | ------------------------------
char*            | const char*    | Any       | [strong Read-only], [strong {string rep}]
pstring          | critcl_pstring | Any       | [strong Read-only]
bytes            | critcl_bytes   | ByteArray | [strong Read-only]
---------------- | -------------- | --------- | ------------------------------
int              | int            | Int       |
long             | long           | Long      |
wideint          | Tcl_WideInt    | WideInt   |
double           | double         | Double    |
float            | float          | Double    |
---------------- | -------------- | --------- | ------------------------------
X > 0            |                |           | For X in [type int] ... [type float] above.
X >= 0           |                |           | C types as per the base type X.
X < 0            |                |           | Allowed argument values are
X <= 0           |                |           | restricted as per the shown
X > 1            |                |           | relation
X >= 1           |                |           |
X < 1            |                |           | This is not a general mechanism
X <= 1           |                |           | open to other values. Only 0/1.
---------------- | -------------- | --------- | ------------------------------
boolean          | int            | Boolean   |
bool             |                |           | Alias of [type boolean] above
---------------- | -------------- | --------- | ------------------------------
channel          | Tcl_Channel    | String    | Assumed to be registered
unshared-channel | Tcl_Channel    | String    | As above, limited to current interpreter
take-channel     | Tcl_Channel    | String    | As above, C code takes ownership
---------------- | -------------- | --------- | ------------------------------
bytearray        |                |           | [strong DEPRECATED]
rawchar          |                |           | [strong DEPRECATED]
rawchar*         |                |           | [strong DEPRECATED]
double*          |                |           | [strong DEPRECATED]
float*           |                |           | [strong DEPRECATED]
int*             |                |           | [strong DEPRECATED]
void*            |                |           | [strong DEPRECATED]
[example_end]
