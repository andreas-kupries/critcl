## -*- tcl -*-
# # ## ### ##### ######## ############# #####################
# Pragmas for MetaData Scanner.
# n/a

# CriTcl Utility Commands. Specification of a command representing a
# class made easy, with code for object command and method dispatch
# generated.

package provide critcl::class 1

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl    8.4   ; # Min supported version.
package require critcl 3.1   ; # Need 'meta?' to get the package name.
package require critcl::util ; # Use the package's Get/Put commands.

namespace eval ::critcl::class       {}

# # ## ### ##### ######## ############# #####################
## API: Generate the declaration and implementation files for the class.

proc ::critcl::class::def {classname script} {
    # Arguments:
    # - name of the Tcl command representing the class.
    #   May contain namespace qualifiers. Represented by a ccommand.
    # - script specifying the state structure and methods.

    #puts "=== |$classname|"
    #puts "--- $script"

    # Pull the package we are working on out of the 

    set package [critcl::meta? name]
    lassign [uplevel 1 [list ::critcl::name2c $classname]] ns cns cclassname

    #puts "%%% Pkg |$package|"
    #puts "%%% NS  |$ns|"
    #puts "%%% CNS |$cns|"
    #puts "%%% CCN |$cclassname|"

    spec::Begin
    spec::Process $script
    set data [spec::Done]

    #puts "@@@ <<$data>>"

    set stem      ${package}_$cns$cclassname
    set classtype ${stem}_CLASS

    dict set data package   $package
    dict set data class     $classname
    dict set data stem      $stem
    dict set data classtype $classtype

    if {[dict exists $data include]} {
	set lines {}
	foreach header [dict get $data include] {
	    lappend lines "#include <$header>"
	}
	dict set data includes [join $lines \n]
    } else {
	dict set data includes {/* No inclusions */}
    }

    if {[dict exists $data instancetype]} {
	set itype [dict get $data instancetype]
	dict set data ivardecl    "    $itype instance"
	dict set data ivarrelease ""
	dict set data ivarerror   ""
	dict set data itypedecl   "/* External type for instance state: $itype */"
    } elseif {[dict exists $data field]} {
	# Convert fields into instance type declaration and name.
	set itype [dict get $data stem]_INSTANCE
	set decl {}
	lappend decl "typedef struct ${itype}__ \{"
	lappend decl "    Tcl_Command cmd;"
	foreach fname [dict get $data field names] {
	    set ctype   [dict get $data field def $fname ctype]
	    set comment [dict get $data field def $fname comment]

	    set field "    $ctype $fname;"
	    if {$comment ne {}} {
		append field " /* $comment */"
	    }
	    lappend decl $field
	}
	lappend decl "\} ${itype}__;"
	lappend decl "typedef struct ${itype}__* $itype;"

	dict set data instancetype $itype
	dict set data ivardecl    "    $itype instance = ($itype) ckalloc (sizeof (${itype}__))"
	dict set data ivarerror   "error:\n    ckfree ((char*) instance);\n    return NULL;"
	dict set data ivarrelease "    ckfree ((char*) instance)"
	dict set data itypedecl   [join $decl \n]

	dict lappend data postconstructor "instance->cmd = cmd;"

	if {[dict exists $data field def class]} {
	    dict append data ivardecl ";\n    instance->class = class"
	}
    } else {
	return -code error "Unable to generate code, must have either external type, or set of fields"
    }

    dict set data ctypedecl {}
    if {[dict exists $data classvar]} {
	# Convert class variables  into class type field declarations.

	set decl {}
	lappend decl "/* # # ## ### ##### ######## User: Class variables */"

	foreach fname [dict get $data classvar names] {
	    set ctype   [dict get $data classvar def $fname ctype]
	    set comment [dict get $data classvar def $fname comment]

	    set field "$ctype $fname;"
	    if {$comment ne {}} {
		append field " /* $comment */"
	    }
	    lappend decl $field
	}

	lappend decl "/* # # ## ### ##### ######## */"

	dict set data ctypedecl "    [join $decl "\n    "]\n"
    }

    # Process the method declarations. Ensure that the names are
    # listed in alphabetical order, to be nice.

    if {[dict exists $data method names]} {
	set map [list @stem@ $stem @instancetype@ $itype]
	foreach name [lsort -dict [dict get $data method names]] {
	    set mname                  [dict get $data method def $name enum]
	    set mcase [string map $map [dict get $data method def $name case]]
	    set mcode [string map $map [dict get $data method def $name code]]

	    lappend mnames \"$name\",
	    lappend menums $mname
	    lappend mcases $mcase
	    lappend mcodes $mcode
	}

	dict unset data method
    } else {
	set mnames {}
	set menums {M_EMPTY}
	set mcases {}
	set mcodes {}
    }

    dict set data method_names           "    [join $mnames "\n    "]"
    dict set data method_enumeration     "    [join $menums ",\n    "]"
    dict set data method_dispatch        "\t[join $mcases \n\t]"
    dict set data method_implementations   [join $mcodes \n]

    # Process the class method declarations. Ensure that the names are
    # listed in alphabetical order, to be nice.

    set cmnames {}
    set cmenums {}
    set cmcases {}
    set cmcodes {}

    if {[dict exists $data classmethod names]} {
	set map [list @stem@ $stem @classtype@ $classtype]
	foreach name [lsort -dict [dict get $data classmethod names]] {
	    set cmname                  [dict get $data classmethod def $name enum]
	    set cmcase [string map $map [dict get $data classmethod def $name case]]
	    set cmcode [string map $map [dict get $data classmethod def $name code]]

	    lappend cmnames \"$name\",
	    lappend cmenums $cmname
	    lappend cmcases $cmcase
	    lappend cmcodes $cmcode
	}

	dict unset data classmethod
    }

    dict set data class_method_names           "\n    [join $cmnames "\n    "]"
    dict set data class_method_enumeration     ",\n    [join $cmenums ",\n    "]"
    dict set data class_method_dispatch        "\t[join $cmcases \n\t]"
    dict set data class_method_implementations   [join $cmcodes \n]

    # Process the class constructor fragments, if any.
    if {[dict exists $data classconstructor]} {
	set cc [dict get $data classconstructor]
	dict set data classconstructor "\{\n[join $cc "\n\}\n    \{\n"]\n\}"
    }

    # Process the class destructor fragments, if any.
    if {[dict exists $data classdestructor]} {
	set cc [dict get $data classdestructor]
	dict set data classdestructor "\{\n[join $cc "\n\}\n    \{\n"]\n\}"
    }

    # Process the constructor fragments, if any.
    if {[dict exists $data constructor]} {
	set cc [dict get $data constructor]
	dict set data constructor "\{\n[join $cc "\n\}\n    \{\n"]\n\}"
    }

    # Process the constructor fragments, if any.
    if {[dict exists $data postconstructor]} {
	set cc [dict get $data postconstructor]
	dict set data postconstructor "\{\n[join $cc "\n\}\n    \{\n"]\n\}"
    }

    # Process the destructor fragments, if any.
    if {[dict exists $data destructor]} {
	set cc [dict get $data destructor]
	dict set data destructor "\{\n[join $cc "\n\}\n    \{\n"]\n\}"
    }

    # Process the support code fragments, if any.
    if {[dict exists $data support]} {
	set support [dict get $data support]
	dict set data support [join $support \n]
    }

    # Ensure the existence of keys required by the template and
    # optional in the class declaration.
    foreach k {
	classconstructor
	constructor
	postconstructor
	destructor
	support
    } {
	if {[dict exists $data $k]} continue
	dict set data $k {}
    }

    #array set CLASS $data
    #parray CLASS
    #unset CLASS

    set map [MakeMap $data]
    MakeSupport $data $map

    uplevel 1 [list critcl::ccommand $classname ${stem}_ClassCommand]
    return
}

proc ::critcl::class::MakeMap {data} {
    # First set of substitutions.
    set premap {}
    dict for {k v} $data {
	lappend premap @${k}@ $v
    }

    # Resolve the substitutions used in pieces of code to generate the
    # final map.
    set map {}
    foreach {k v} $premap {
	lappend map $k [string map $premap $v]
    }

    return $map
}

proc ::critcl::class::MakeSupport {data map} {
    set hdr      [dict get $data stem]_class.h
    set header   [file join [critcl::cache] $hdr]
    set template [Template class.h]

    #puts T=[string length $template]

    file mkdir [critcl::cache]

    critcl::util::Put $header [string map $map $template]

    critcl::ccode "#include <$hdr>"
    return
}

proc ::critcl::class::Template {path} {
    variable selfdir
    set path $selfdir/$path
    #puts T=$path
    return [critcl::util::Get $path]
}

# # ## ### ##### ######## ############# #####################
##
# Internal: Namespace holding the specification commands and related
# state. Treat like a sub-package, with a proper API.
##
# # ## ### ##### ######## ############# #####################

namespace eval ::critcl::class::spec {}

proc ::critcl::class::spec::Begin {} {
    variable state {}
    dict set state method      names {}
    dict set state classmethod names {}
    return
}

proc ::critcl::class::spec::Done {} {
    variable state
    set result $state
    unset state
    return $result
}

proc ::critcl::class::spec::Process {script} {
    eval $script
    return
}

# # ## ### ##### ######## ############# #####################

proc ::critcl::class::spec::include {header} {
    # Name of API to include in the generated code.
    variable state
    dict lappend state include $header
    return
}

proc ::critcl::class::spec::type {name} {

    # Declaration of the C type to use for the object state.  This
    # type is expected to be declared externally. It allows us to use
    # a 3rd party structure directly. Cannot be specified if fields
    # and/or class variables for our own structures have been declared
    # already.

    variable state

    if {
	[dict exists $state field] ||
	[dict exists $state classvar]
    } {
	return -code error \
	    "external instance type is in conflict with fields or class variables already declared."
    }

    dict set state instancetype $name
    return
}


proc ::critcl::class::spec::field {ctype name {comment {}}} {
    # Declaration of a field in the C structure for instances. Cannot
    # be specified if a an external "type" has been specified already.

    variable state

    if {[dict exists $state instancetype]} {
	return -code error \
	    "Field is in conflict with external instance type already declared."
    }

    if {[dict exists $state field def $name]} {
	return -code error "Duplicate definition of field \"$name\""
    }

    dict update state field f {
	dict lappend f names $name
    }
    dict set state field def $name ctype   $ctype
    dict set state field def $name comment $comment

    if {[llength [dict get $state field names]] == 1} {
	# Generate a destroy method. We can do that, because we know
	# that the instance structure will have a field named 'cmd'.
	# Declared here.
	dict set state field def cmd {}
	mdef destroy {
	    if (objc != 2) {
		Tcl_WrongNumArgs (interp, 2, objv, NULL);
		return TCL_ERROR;
	    }
	    Tcl_DeleteCommandFromToken(interp, instance->cmd);
	    return TCL_OK;
	}
    }
    return
}

proc ::critcl::class::spec::classvar {ctype name {comment {}}} {

    # Declaration of a field in the C structure for classes. Cannot be
    # specified if a an external "type" has been specified already.

    variable state

    if {[dict exists $state instancetype]} {
	return -code error \
	    "Class variable is in conflict with external instance type already declared."
    }

    if {[dict exists $state classvar def $name]} {
	return -code error "Duplicate definition of class variable \"$name\""
    }

    dict update state classvar c {
	dict lappend c names $name
    }
    dict set state classvar def $name ctype   $ctype
    dict set state classvar def $name comment $comment

    if {[llength [dict get $state classvar names]] == 1} {
	# On declaration of the first class variable we
	# we declare a field which provides instances with
	# a reference to their class.
	field @classtype@ class {Reference to class (variables)}
    }
    return
}

proc ::critcl::class::spec::introspect-methods {} {
    classvar Tcl_Obj* methods {Cache for list of method names}
    classconstructor {
	class->methods = ComputeMethodList (@stem@_methodnames);
	Tcl_IncrRefCount (class->methods);
    }
    classdestructor {
	Tcl_DecrRefCount (class->methods);
	class->methods = NULL;
    }
    support {
#ifndef HAVE_COMPUTE_METHOD_LIST
#define HAVE_COMPUTE_METHOD_LIST
static Tcl_Obj*
ComputeMethodList (CONST char** table)
{
    int n, i;
    char** item;
    Tcl_Obj** lv;
    Tcl_Obj* result;

    item = (char**) table;
    n = 0;
    while (*item) {
	n ++;
	item ++;
    }

    lv = (Tcl_Obj**) ckalloc (n * sizeof (Tcl_Obj*));
    i = 0;
    while (table [i]) {
	lv [i] = Tcl_NewStringObj (table [i], -1);
	i ++;
    }

    result = Tcl_NewListObj (n, lv);
    ckfree ((char*) lv);

    return result;
}
#endif /* HAVE_COMPUTE_METHOD_LIST */
    }
    mdef methods { /* <instance> methods */
	if (objc != 2) {
	    Tcl_WrongNumArgs (interp, 0, objv, NULL);
	    return TCL_ERROR;
	}
	Tcl_SetObjResult (interp, instance->class->methods);
	return TCL_OK;
    }
    classmethod methods { /* <class> methods */
	if (objc != 2) {
	    Tcl_WrongNumArgs (interp, 0, objv, NULL);
	    return TCL_ERROR;
	}
	Tcl_SetObjResult (interp, class->methods);
	return TCL_OK;
    }
    return
}

proc ::critcl::class::spec::classconstructor {code} {
    variable state
    set loc {};#[critcl::LinePragma -2 [critcl::This]]
    dict lappend state classconstructor $loc[string trim $code \n]
    return
}

proc ::critcl::class::spec::classdestructor {code} {
    variable state
    set loc {};#[critcl::LinePragma -2 [critcl::This]]
    dict lappend state classdestructor $loc[string trim $code \n]
    return
}

proc ::critcl::class::spec::constructor {code {postcode {}}} {
    variable state
    set loc {};#[critcl::LinePragma -2 [critcl::This]]
    dict lappend state constructor        $loc[string trim $code     \n]
    dict lappend state postconstructor    $loc[string trim $postcode \n]
    return
}

proc ::critcl::class::spec::destructor {code} {
    variable state
    set loc {};#[critcl::LinePragma -2 [critcl::This]]
    dict lappend state destructor $loc[string trim $code \n]
    return
}

proc ::critcl::class::spec::support {code} {
    variable state
    set loc {};#[critcl::LinePragma -2 [critcl::This]]
    dict lappend state support $loc[string trim $code \n]
    return
}

proc ::critcl::class::spec::mdef {name args} {
    variable state

    if {[dict exists $state method def $name]} {
	return -code error "Duplicate definition of method \"$name\""
    }

    set loc {};#[critcl::LinePragma -2 [critcl::This]]

    # Compute a C enum identifier from the method name. To avoid
    # trouble we have to remove any non-alphabetic characters. We
    # further then have to add a serial number to distinguish methods
    # which would, despite having different names, transform to the
    # same C enum identifier.

    regsub -all -- {[^a-zA-Z0-9_]} $name _ mname
    regsub -all -- {_+} $mname _ mname
    set serial [llength [dict get $state method names]]
    set mname @stem@_M_${serial}_[string toupper $mname]

    set case  "case $mname: return @function@ (instance, interp, objc, objv); break;"

    if {[llength $args] >= 2} {
	if {[lindex $args 0] ne "as"} {
	    return -code error "wrong#args"
	}

	# Method is external function.

	set function [lindex $args 1]
	set code     "/* $name : External function @function@ */"

	if {[llength $args] > 2} {
	    set details "objv, [join [lrange $args 2 end] ", "]"
	    set case [string map [list objv $details] $case]
	}

    } elseif {[llength $args] != 1} {
	return -code error "wrong#args"
    } else {
	set function ${mname}_Cmd
	set body [lindex $args 0]
	set code $loc[string map [list @body@ $body] [string trim {
	    static int
	    @function@ (@instancetype@ instance,
			Tcl_Interp*            interp,
			int                    objc,
			Tcl_Obj* CONST*        objv)
	    {
		@body@
	    }
	}]]
    }    

    set map [list @function@ $function]

    dict update state method m {
	dict lappend m names $name
    }
    dict set state method def $name enum $mname
    dict set state method def $name case [string map $map $case]
    dict set state method def $name code [string map $map $code]
    return
}

proc ::critcl::class::spec::classmethod {name args} {
    variable state

    if {[dict exists $state classmethod def $name]} {
	return -code error "Duplicate definition of class method \"$name\""
    }

    set loc {};#[critcl::LinePragma -2 [critcl::This]]

    # Compute a C enum identifier from the classmethod name. To avoid
    # trouble we have to remove any non-alphabetic characters. We
    # further then have to add a serial number to distinguish
    # classmethods which would, despite having different names,
    # transform to the same C enum identifier.

    regsub -all -- {[^a-zA-Z0-9_]} $name _ mname
    regsub -all -- {_+} $mname _ mname
    set serial [llength [dict get $state classmethod names]]
    set mname @stem@_CM_${serial}_[string toupper $mname]

    set case  "case $mname: return @function@ (class, interp, objc, objv); break;"

    if {[llength $args] >= 2} {
	if {[lindex $args 0] ne "as"} {
	    return -code error "wrong#args"
	}

	# Classmethod is external function.

	set function [lindex $args 1]
	set code     "/* $name : External function @function@ */"

	if {[llength $args] > 2} {
	    set details "objv, [join [lrange $args 2 end] ", "]"
	    set case [string map [list objv $details] $case]
	}

    } elseif {[llength $args] != 1} {
	return -code error "wrong#args"
    } else {
	set function ${mname}_Cmd
	set body [lindex $args 0]
	set code $loc[string map [list @body@ $body] [string trim {
	    static int
	    @function@ (@classtype@ class,
			Tcl_Interp*            interp,
			int                    objc,
			Tcl_Obj* CONST*        objv)
	    {
		@body@
	    }
	}]]
    }    

    set map [list @function@ $function]

    dict update state classmethod m {
	dict lappend m names $name
    }
    dict set state classmethod def $name enum $mname
    dict set state classmethod def $name case [string map $map $case]
    dict set state classmethod def $name code [string map $map $code]
    return
}

# # ## ### ##### ######## ############# #####################
## State

namespace eval ::critcl::class {
    variable selfdir [file dirname [file normalize [info script]]]
}

# # ## ### ##### ######## ############# #####################
## Export API

namespace eval ::critcl::class {
    namespace export def
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## Ready
return
