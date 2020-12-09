## -*- tcl -*-
# # ## ### ##### ######## ############# #####################
# Pragmas for MetaData Scanner.
# n/a

# CriTcl Utility Commands. Generator for Tcl_ObjTypes handling
# conversion from and to a C type (opaque pointer, or structure).
# Note, internally the second case is reduced to the first.

package provide critcl::objtype 1

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl    8.4    ; # Min supported version.
package require critcl 3.1    ; # Need 'meta?' to get the package name.
package require critcl::util  ; # Use the package's Get/Put commands.

package require critcl::cutil ; # We are using the C level TRACE macros.
critcl::cutil::tracer on      ; # Set the necessary includes up.

namespace eval ::critcl::objtype {}

# # ## ### ##### ######## ############# #####################
## API: Generate declarations and implementation for a custom Tcl_ObjType

# Arguments:
# - Name of the Tcl_ObjType.
# - Script specifying the setup. Two possibilities:
#
#   1. Values are opaque handles. They come with 4 functions to:
#      a. Aquire a reference
#      b. Release a reference
#      c. ToString
#      d. FromString
#
#      Items (a) and (b) mean that whatever is behind a handle is
#      reference counted and the functions of the Tcl_ObjType
#      simply copy the handles around and manage references
#      properly.
#
#      And items (c) and (d) mean that the conversions from and to
#      string representations are delegated to the handle
#      functionality.
#
#      Specification commands:
#
#		handle typename ?spec?
#
#		spec :: dict ('ref'     -> ... aquire a reference
#                         'unref'   -> ... release a reference
#                         '2string' -> ... convert value to string
#                         '2value'  -> ... convert string to value)
#           where ... <=> name of function to
#
#           Missing function names (including a missing spec)
#           causes the generator derive the function names from
#           the name of the Tcl_ObjType.
##
#   2. Values are exposed structures. The obj type declares the
#      fields (name and types (*)) and the generator creates all
#      the functions needed to allocate and copy things around.
#
#      (*) Type names have to have associated critcl arg __and__
#          result types.  The associated C conversion fragments
#          are used in the conversions from and to the string
#          representations.
#
#	   ATTENTION: For the moment let us support only simple
#	   fields, IOW directly copy-able without need for additional
#	   management (memory (de)allocation, reference counts).
#
#      When that works we can look into ways of making that work
#      too. Especially if the underlying type is something made by
#      this generator.
#
#      May need a way to access/declare copy constructors.  This
#      may need extension of the core critcl facilities around argument
#      and result types.
#
#      Specification commands:
#
#		structure spec
#
#		spec :: dict (field_name -> field_type)
#
#		Note: Order is important and taken into account.

proc ::critcl::objtype::structure {name spec args} {
    variable state
    Init $name

    set format  tagged-dict
    set traced 0
    set tagged  1
    set format  dict
    set formats {dict list}

    while {[string match -* [set option [lindex $args 0]]]} {
	set v [lindex $args 1]
	switch -exact -- $option {
	    -format {
		if {$v ni $formats} {
		    set formats [linsert [join $formats {, }] end-1 or]
		    return -code error "Illegal format $v, expected one of $formats"
		}
		set format $v
	    }
	    -trace {
		if {![string is bool -strict $v]} {
		    return -code error "Expected boolean, got $v"
		}
		set traced $v
	    }
	    -tagged {
		if {![string is bool -strict $v]} {
		    return -code error "Expected boolean, got $v"
		}
		set tagged [expr {!!$v}] ;#normalization of the boolean to 0/1
	    }
	    default {
		return -code error "Unknown option $option, expected one of -format, trace, or -tagged"
	    }
	}
	set args [lrange $args 2 end]
    }

    dict set state intrep    ${name}*
    dict set state mode      structure
    dict set state structure $spec
    dict set state format    $format
    dict set state tagged    $tagged
    dict set state traced    $traced

    Complete
    return
}

proc ::critcl::objtype::handle {name args} {
    variable state
    Init $name

    set ctype    $name
    set traced   0
    set ref      {}
    set unref    {}
    set 2value   {}
    set 2string  {}
    set refcount {}

    while {[string match -* [set option [lindex $args 0]]]} {
	set v [lindex $args 1]
	switch -exact -- $option {
	    -trace {
		if {![string is bool -strict $v]} {
		    return -code error "Expected boolean, got $v"
		}
		set traced [expr {!!$v}]
	    }
	    -type {
		if {$v eq {}} {
		    return -code error "Expected non-empty string"
		}
		set ctype $v
	    }
	    -ref -
	    -unref -
	    -refcount -
	    -2value -
	    -2string {
		if {$v eq {}} {
		    return -code error "Expected non-empty string"
		}
		# Chopping leading `-` maps option to variable name.
		set [string range $option 1 end] $v
	    }
	    default {
		return -code error "Unknown option $option, expected one of -trace, -type, -ref, -unref, -refcount, -2string, or -2value"
	    }
	}
	set args [lrange $args 2 end]
    }
    foreach k {
	ref unref refcount 2string 2value
    } {
	if {[set $k] ne {}} continue
	set $k ${ctype}_$k
    }

    dict set state intrep   $ctype
    dict set state mode     handle
    dict set state ref      $ref
    dict set state unref    $unref
    dict set state refcount $refcount
    dict set state 2value   ${2value}
    dict set state 2string  ${2string}
    dict set state traced   $traced

    Complete
    return
}

proc ::critcl::objtype::Init {name} {
    variable state
    catch { unset state }

    #puts "=== |$name|"
    # Pull the package we are working on out of the system.

    set package  [critcl::meta? name]
    set qpackage [expr {[string match ::* $package]
			? "$package"
			: "::$package"}]

    # Compute the various C identifiers to use.
    lassign [uplevel 2 [list ::critcl::name2c $name]]      ns  cns  cname
    lassign [uplevel 2 [list ::critcl::name2c $qpackage]]  pns pcns cpackage

    #puts "%%% Pkg  |$package|"
    #puts "%%% pNS  |$pns|"
    #puts "%%% pCNS |$pcns|"
    #puts "%%% cPkg |$cpackage|"

    #puts "%%% Class |$name|"
    #puts "%%% NS    |$ns|"
    #puts "%%% CNS   |$cns|"
    #puts "%%% CCN   |$cname|"

    set stema ${pcns}${cpackage}_$cns$cname
    set stemb [Camelize ${pcns}${cpackage}]_[Camelize $cns$cname]

    dict set state package     $package
    dict set state name        $name
    dict set state stem        $stema
    dict set state objtypevar  ${stema}_ObjType

    dict set state api_public   0
    dict set state api_new      ${stemb}NewObj
    dict set state api_from     ${stemb}FromObj
    dict set state code_support {}

    dict set state trace TRACE_TAG_OFF
    return
}

proc ::critcl::objtype::Camelize {s} {
    set r {}
    foreach e [split $s _] { lappend r [string totitle $e] }
    return [join $r {}]
}

proc ::critcl::objtype::Complete {} {
    variable state

    GenerateCode
    Emit

    unset state
    return
}

proc ::critcl::objtype::GenerateCode {} {
    variable state

    set stem   [dict get $state stem]
    set traced [dict get $state traced]

    # Hook function names
    Set fun_destructor ${stem}_ReleaseIntRep
    Set fun_copy       ${stem}_DuplicateIntRep
    Set fun_stringify  ${stem}_StringOfIntRep
    Set fun_from_any   ${stem}_IntRepFromAny
    Set trace          [expr {$traced ? "TRACE_TAG_ON" : "TRACE_TAG_OFF"}]

    GC/[dict get $state mode]

    dict unset state mode
    dict unset state traced
    return
}

proc ::critcl::objtype::GC/handle {} {
    # TODO: objtrack integration.
    return
}

proc ::critcl::objtype::GC/structure {} {
    variable state
    # TODO: objtrack integration.

    # Create structure support code allowing for use as a handle.

    set name   [dict get $state name]
    set stem   [dict get $state stem]

    set format [dict get $state format]    ; dict unset state format
    set tagged [dict get $state tagged]    ; dict unset state tagged
    set spec   [dict get $state structure] ; dict unset state structure

    # Generate the code for a (semi)opaque type usable by the
    # low-level parts of the generator (handle type).

    dict set state ref      ${stem}_ref
    dict set state unref    ${stem}_unref
    dict set state 2string  ${stem}_to_string
    dict set state 2value   ${stem}_to_value
    dict set state refcount ${stem}_refcount

    foreach {
	ftype
	fname
    } $spec {
	lassign [critcl::argtype-def $ftype] _ _ val _ _ str
	if {$str eq {}} {
	    return -code error "Bad type $ftype: No conversion to string available"
	}

	lappend fields "$ftype $fname;"
	lappend strc   $fname $str
	lappend valc   $fname $val
    }

    dict set state fields [join $fields "\n\t    "]

    # Wrap the field conversions as per the chosen string
    # representation (list|dict tagged|untagged)

    dict set state c2string [GC/structure/2string $format $tagged $name $strc]
    dict set state c2value  [GC/structure/2value  $format $tagged $name $valc]

    Set code_support [join [list [critcl::at::here!]\n {
	typedef struct @intrep@_s {
	    unsigned int refCount;
	    @fields@
	} @intrep@_s;

	static int @stem@_refcount (@intrep@ value)
	{
	    return value->refCount;
	}

	static void @stem@_ref (@intrep@ value)
	{
	    value->refCount ++;
	}

	static void @stem@_unref (@intrep@ value)
	{
	    if (value->refCount > 1) { value->refCount --; return; }
	    ckfree ((char*) value);
	}

	static void @stem@_to_string (@intrep@ value, Tcl_DString* ds)
	{
	    @c2string@
	}

	static int @stem@_to_value_do (Tcl_Interp* interp, Tcl_Obj* obj, @intrep@ value)
	{
	    @c2value@
	    return TCL_OK;
	}

	static @intrep@ @stem@_to_value (Tcl_Interp* interp, Tcl_Obj* obj)
	{
	    @intrep@ value = (@intrep@) ckalloc (sizeof (@intrep@_s));
	    int res = @stem@_to_value_do (interp, obj, value);
	    if (res == TCL_OK) { return value; }
	    ckfree ((char*) value);
	    return NULL;
	}
    }] {}]

    GC/handle
    return
}

proc ::critcl::objtype::GC/structure/2value {format tagged name valc} {
    return [join [GC/structure/2value/$format$tagged $name $valc] "\n\t    "]
}

proc ::critcl::objtype::GC/structure/2value/list1 {name valc} {
    set tovalue {}
    set n       [expr {1 + ([llength $valc] >> 1)}]
    set index   1

    GC/structure/2value/list-setup

    lappend tovalue "if (strcmp (Tcl_GetString (lv\[0\]), \"$name\") != 0) \{"
    lappend tovalue "    Tcl_AppendResult (interp,"
    lappend tovalue "        \"Bad tag, expected '$name'\","
    lappend tovalue "        NULL);"
    lappend tovalue "    return TCL_ERROR;"
    lappend tovalue "\}"

    GC/structure/2value/list-fields $valc
    return $tovalue
}

proc ::critcl::objtype::GC/structure/2value/list0 {name valc} {
    set tovalue {}
    set n       [expr {[llength $valc] >> 1}]
    set index   0

    GC/structure/2value/list-setup
    GC/structure/2value/list-fields $valc
    return $tovalue
}

proc ::critcl::objtype::GC/structure/2value/list-setup {} {
    upvar 1 tovalue tovalue n n
    lappend tovalue "int       lc;"
    lappend tovalue "Tcl_Obj** lv;"
    lappend tovalue "int res = Tcl_ListObjGetElements (interp, obj, &lc, &lv);"
    lappend tovalue "if (res != TCL_OK) \{ return TCL_ERROR; \}"
    lappend tovalue "if (lc != $n) \{"
    lappend tovalue "    Tcl_AppendResult (interp,"
    lappend tovalue "        \"Bad number of elements, expected $n\","
    lappend tovalue "        NULL);"
    lappend tovalue "    return TCL_ERROR;"
    lappend tovalue "\}"
    return
}

proc ::critcl::objtype::GC/structure/2value/list-fields {valc} {
    upvar 1 tovalue tovalue index index
    foreach {fname ftov} $valc {
	set map [list @@ "lv\[$index\]" @A value->$fname]
	lappend tovalue "/* == $fname ====================================== */"
	lappend tovalue [string map $map $ftov]
	incr index
    }
    lappend tovalue "/* == ====================================== */"
    return
}

proc ::critcl::objtype::GC/structure/2value/dict1 {name valc} {
    set tovalue {}
    set n       [expr {[llength $valc] >> 1}]

    lappend tovalue "int size, res;"

    GC/structure/2value/dict-setup 1

    lappend tovalue "Tcl_Obj* inner;"
    lappend tovalue "Tcl_Obj* key = Tcl_NewStringObj (\"$name\", -1);"
    lappend tovalue "res = Tcl_DictObjGet (interp, obj, key, &inner);"
    lappend tovalue "Tcl_DecrRefCount (key);"
    lappend tovalue "if (res != TCL_OK) \{ return res; \}"
    lappend tovalue "obj = inner;"

    GC/structure/2value/dict-setup $n
    GC/structure/2value/dict-fields $name $valc

    return $tovalue
}

proc ::critcl::objtype::GC/structure/2value/dict0 {name valc} {
    set tovalue {}
    set n       [expr {[llength $valc] >> 1}]

    lappend tovalue "int size, res;"

    GC/structure/2value/dict-setup $n

    lappend tovalue "Tcl_Obj* key;"

    GC/structure/2value/dict-fields $name $valc

    return $tovalue
}

proc ::critcl::objtype::GC/structure/2value/dict-setup {n} {
    upvar 1 tovalue tovalue
    lappend tovalue "res = Tcl_DictObjSize (interp, obj, &size);"
    lappend tovalue "if (res != TCL_OK) \{ return res; \}"
    lappend tovalue "if (size != $n) \{"
    lappend tovalue "    Tcl_AppendResult (interp,"
    lappend tovalue "        \"Bad number of entries, expected $n\","
    lappend tovalue "        NULL);"
    lappend tovalue "    return TCL_ERROR;"
    lappend tovalue "\}"
    return
}

proc ::critcl::objtype::GC/structure/2value/dict-fields {name valc} {
    upvar 1 tovalue tovalue n n

    lappend tovalue "Tcl_Obj* field;"
    foreach {fname ftov} $valc {
	# TODO: Use a `critcl::literals::def` for the keys to
	# eradicate the Tcl_Obj* churn we have here.

	lappend tovalue "/* == $fname ====================================== */"
	lappend tovalue "key = Tcl_NewStringObj (\"$fname\", -1);"
	lappend tovalue "res = Tcl_DictObjGet (interp, obj, key, &field);"
	lappend tovalue "Tcl_DecrRefCount (key);"
	lappend tovalue "if (res != TCL_OK) \{ return TCL_ERROR; \}"
	set map [list @@ field @A value->$fname]
	lappend tovalue [string map $map $ftov]
	incr index
    }
    lappend tovalue "/* == ====================================== */"
    return
}

proc ::critcl::objtype::GC/structure/2string {format tagged name strc} {
    set tostring {}
    if {$tagged} {
	switch -exact -- $format {
	    dict {
		lappend tostring "Tcl_DStringAppendElement (ds, \"$name\");"
		lappend tostring "Tcl_DStringStartSublist (ds);"
	    }
	    list {
		lappend tostring "Tcl_DStringAppendElement (ds, \"$name\");"
	    }
	}
    }
    switch -exact -- $format {
	dict {
	    foreach {fname ftos} $strc {
		set map [list @DS ds @A value->$fname]
		lappend tostring "Tcl_DStringAppendElement (ds, \"$fname\");"
		lappend tostring [string map $map "{ $ftos }"]
	    }
	}
	list {
	    foreach {fname ftos} $strc {
		set map [list @DS ds @A value->$fname]
		lappend tostring [string map $map "{ $ftos }"]
	    }
	}
    }
    if {$tagged && ($format eq "dict")} {
	lappend tostring "Tcl_DStringEndSublist (ds);"
    }

    return [join $tostring "\n\t    "]
}

proc ::critcl::objtype::Set {key value} {
    variable state
    dict set state $key $value
    return
}

proc ::critcl::objtype::Emit {} {
    variable state

    set name     [dict get $state name]
    set stem     [dict get $state stem]
    set intrep   [dict get $state intrep]
    set hdr      ${stem}_objtype.h
    set header   [file join [critcl::cache] $hdr]

    file mkdir [critcl::cache]
    set template [Template objtype.h]
    #puts T=[string length $template]
    set map [MakeMap]
    critcl::util::Put $header [string map $map $template]

    critcl::ccode "#include <$hdr>"

    if {[dict get $state api_public]} {
	set n [dict get $state api_new]
	set f [dict get $state api_from]
	set t [dict get $state intrep]

	critcl::api function Tcl_Obj* $n [list $t value]
	critcl::api function int      $f [list Tcl_Interp* interp Tcl_Obj* obj ${t}* value]
    }

    critcl::argtype $name \n[critcl::at::here!][string map $map {
	if (@api_from@ (interp, @@, &@A) != TCL_OK) return TCL_ERROR;
    }] $intrep $intrep

    critcl::resulttype $name \n[critcl::at::here!][string map $map {
	/* @api_new@ result is 0-refcount */
	{ Tcl_Obj* ro = @api_new@ (rv);
	if (ro == NULL) { return TCL_ERROR; }
	Tcl_SetObjResult (interp, ro);
	return TCL_OK; }
    }] $intrep

    return
}

proc ::critcl::objtype::MakeMap {} {
    variable state

    # First set of substitutions.
    set premap {}
    dict for {k v} $state {
	lappend premap @${k}@ $v
    }

    # Resolve the substitutions used in the fragments of code to
    # generate the final map.
    set map {}
    foreach {k v} $premap {
	lappend map $k [string map $premap $v]
    }

    return $map
}

proc ::critcl::objtype::Template {path} {
    variable selfdir
    set path $selfdir/$path
    #puts T=$path
    return [critcl::util::Get $path]
}

# # ## ### ##### ######## ############# #####################
## State

namespace eval ::critcl::objtype {
    variable selfdir [file dirname [file normalize [info script]]]
}

# # ## ### ##### ######## ############# #####################
## Export API

namespace eval ::critcl::objtype {
    namespace export structure handle
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## Ready
return
