[comment {
  Table of the standard argument types, for a quick overview
  of the names, associated C types, behaviour and limits.
}]
[example_begin]
Critcl type | C type         | Tcl type  | Notes
----------- | -------------- | --------- | ------------------------------
Tcl_Interp* | Tcl_Interp*    | n/a       | [strong special], only 1st
----------- | -------------- | --------- | ------------------------------
Tcl_Obj*    | Tcl_Obj*       | Any       | [strong read-only]
object      |                |           | Alias of [type Tcl_Obj*] above
list        | critcl_list    | List      | [strong read-only]
----------- | -------------- | --------- | ------------------------------
char*       | const char*    | Any       | [strong read-only], [strong {string rep}]
pstring     | critcl_pstring | Any       | [strong read-only]
bytes       | critcl_bytes   | ByteArray | [strong read-only]
----------- | -------------- | --------- | ------------------------------
int         | int            | Int       | 
long        | long           | Long      | 
wideint     | Tcl_WideInt    | WideInt   | 
double      | double         | Double    | 
float       | float          | Double    | 
----------- | -------------- | --------- | ------------------------------
X > 0       |                |           | For X in [type int] ... [type float]
X >= 0      |                |           | above. C types as per the
X < 0       |                |           | base type X. Allowed arg
X <= 0      |                |           | values are restricted as
X > 1       |                |           | per the indicated relation
X >= 1      |                |           | 
X < 1       |                |           | 
X <= 1      |                |           | 
----------- | -------------- | --------- | ------------------------------
boolean     | int            | Boolean   | 
bool        |                |           | Alias of [type boolean] above
----------- | -------------- | --------- | ------------------------------
bytearray   |                |           | [strong deprecated]
double*     |                |           | [strong deprecated]
float*      |                |           | [strong deprecated]
int*        |                |           | [strong deprecated]
rawchar     |                |           | [strong deprecated]
rawchar*    |                |           | [strong deprecated]
void*       |                |           | [strong deprecated]
[example_end]
