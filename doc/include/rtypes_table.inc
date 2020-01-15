[comment {
  Table of the standard result types, for a quick overview
  of the names, associated C types, behaviour and limits.
}]
[example_begin]
Critcl type   | C type         | Tcl type  | Notes
------------- | -------------- | --------- | ------------------------------
void          | n/a            | n/a       | Always OK. Body sets result
ok            | int            | n/a       | Result code. Body sets result
------------- | -------------- | --------- | ------------------------------
int           | int            | Int       |
boolean       |                |           | Alias of [type int] above
bool          |                |           | Alias of [type int] above
long          | long           | Long      |
wideint       | Tcl_WideInt    | WideInt   |
double        | double         | Double    |
float         | float          | Double    |
------------- | -------------- | --------- | ------------------------------
char*         | char*          | String    | [strong {Makes copy}]
vstring       |                |           | Alias of [type char*] above
const char*   | const char*    |           | Behavior of [type char*] above
------------- | -------------- | --------- | ------------------------------
string        |                | String    | Freeable string set directly
              |                |           | [strong {No copy is made}]
dstring       |                |           | Alias of [type string] above
------------- | -------------- | --------- | ------------------------------
              |                |           | For all below: Null is error
              |                |           |    Body has to set a message
Tcl_Obj*      | Tcl_Obj*       | Any       | [strong {refcount --}
object        |                |           | Alias of [type Tcl_Obj*] above
Tcl_Obj*0     |                | Any       | [strong {refcount unchanged}]
object0       |                |           | Alias of [type Tcl_Obj*0] above
------------- | -------------- | --------- | ------------------------------
known-channel | Tcl_Channel    | String    | Assumes to be registered
new-channel   | Tcl_Channel    | String    | New channel, is registered
[example_end]
