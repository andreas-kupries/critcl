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

    set stem ${package}_$cns$cclassname

    dict set data package   $package
    dict set data class     $classname
    dict set data stem      $stem
    dict set data classtype [dict get $data stem]_CLASS

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
	dict set data ivardecl    "$itype instance"
	dict set data ivarrelease ""
	dict set data itypedecl   "/* External type for instance state: $itype */"
    } elseif {[dict exists $data instancefields]} {
	# Convert fields into instance type declaration and name.
	set itype [dict get $data stem]_INSTANCE
	set decl {}
	lappend decl "typedef struct ${itype}__ \{"
	lappend decl "    Tcl_Command cmd;"
	foreach field [dict get $data instancefields] {
	    foreach {ctype name comment} $field break
	    set field "    $ctype $name;"
	    if {$comment ne {}} {
		append field " /* $comment */"
	    }
	    lappend decl $field
	}
	lappend decl "\} ${itype}__;"
	lappend decl "typedef struct ${itype}__* $itype;"

	dict set data instancetype $itype
	dict set data ivardecl    "$itype instance = ($itype) ckalloc (sizeof (${itype}__))"
	dict set data ivarrelease "ckfree ((char*) instance)"
	dict set data itypedecl   [join $decl \n]

	dict append data postconstructor "instance->cmd = cmd;"

    } else {
	return -code error "Unable to generate code, must have either external type, or set of fields"
    }

    # Process the method declarations. Ensure that the names are
    # listed in alphabetical order, to be nice.

    if {[dict exists $data method_names]} {
	set map [list @stem@ $stem @instancetype@ $itype]
	foreach name [lsort -dict [dict get $data method_names]] {
	    set mname                  [dict get $data method $name enum]
	    set mcase [string map $map [dict get $data method $name case]]
	    set mcode [string map $map [dict get $data method $name code]]

	    lappend mnames \"$name\",
	    lappend menums $mname
	    lappend mcases $mcase
	    lappend mcodes $mcode
	}

	dict unset data method_names
	dict unset data method
    } else {
	set mnames {}
	set menums {M_EMPTY}
	set mcases {}
	set mcodes {}
    }

    dict set data method_names           \t[join $mnames \n\t]
    dict set data method_enumeration     \t[join $menums ",\n\t"]
    dict set data method_dispatch        \t[join $mcases \n\t]
    dict set data method_implementations   [join $mcodes \n]

    foreach k {
	constructor
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
    # for our own structure have been declared already.

    variable state

    if {[dict exists $state instancefields]} {
	return -code error \
	    "external instance type is in conflict with fields already declared."
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

    dict lappend state instancefields [list $ctype $name $comment]
    return
}

proc ::critcl::class::spec::constructor {code {postcode {}}} {
    variable state
    dict set state constructor        [string trim $code]
    dict set state postconstructor    [string trim $postcode]
    return
}

proc ::critcl::class::spec::support {code} {
    variable state
    dict set state support [string trim $code]
    return
}

proc ::critcl::class::spec::destructor {code} {
    variable state
    dict set state destructor [string trim $code]
    return
}

proc ::critcl::class::spec::mdef {name args} {
    variable state

    set mname M_[string toupper $name]
    set case  "case $mname: @function@ (instance, interp, objc, objv); break;"

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
	set function @stem@_$mname
	set body [lindex $args 0]
	set code [string map [list @body@ $body] [string trim {
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

    dict lappend state method_names $name

    dict set state method $name enum $mname

    set map [list @function@ $function]
    dict set state method $name case [string map $map $case]
    dict set state method $name code [string map $map $code]
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
