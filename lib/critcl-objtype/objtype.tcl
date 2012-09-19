## -*- tcl -*-
# # ## ### ##### ######## ############# #####################
# Pragmas for MetaData Scanner.
# n/a

# CriTcl Utility Commands. Generation of Tcl_ObjType handling conversion
# from and to a C representation.

package provide critcl::objtype 1

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl    8.4   ; # Min supported version.
package require critcl 3.1   ; # Need 'meta?' to get the package name.
package require critcl::util ; # Use the package's Get/Put commands.

namespace eval ::critcl::objtype {}

# # ## ### ##### ######## ############# #####################
## API: Generate the declaration and implementation files for the objtype.

proc ::critcl::objtype::define {name script} {
    variable state
    catch { unset state }

    # Arguments:
    # - name of the Tcl_ObjType.
    # - script specifying the internal representation and various pieces
    #   of code.

    #puts "=== |$name|"
    #puts "--- $script"

    # Pull the package we are working on out of the system.

    set package [critcl::meta? name]
    set qpackage [expr {[string match ::* $package] 
			? "$package"
			: "::$package"}]

    # Compute the various C identifiers to use.
    lassign [uplevel 1 [list ::critcl::name2c $name]]      ns  cns  cname
    lassign [uplevel 1 [list ::critcl::name2c $qpackage]]  pns pcns cpackage

    #puts "%%% Pkg  |$package|"
    #puts "%%% pNS  |$pns|"
    #puts "%%% pCNS |$pcns|"
    #puts "%%% cPkg |$cpackage|"

    #puts "%%% Class |$name|"
    #puts "%%% NS    |$ns|"
    #puts "%%% CNS   |$cns|"
    #puts "%%% CCN   |$cname|"

    set stem  ${pcns}${cpackage}_$cns$cname
    set stemb [Camelize ${pcns}${cpackage}]_[Camelize $cns$cname]

    dict set state package     $package
    dict set state name        $name
    dict set state stem        $stem
    dict set state objtypevar  ${stem}_ObjType

    # Overridable by spec, indirect.
    dict set state free ${stem}_ReleaseIntRep
    dict set state dupl ${stem}_DuplicateIntRep
    dict set state 2str ${stem}_StringOfIntRep
    dict set state from ${stem}_IntRepFromAny

    dict set state havefree 1
    dict set state havedupl 1
    dict set state have2str 1
    dict set state havefrom 1

    # Overridable by spec.
    dict set state api_public 0
    dict set state api_new  ${stemb}NewObj
    dict set state api_from ${stemb}FromObj

    # Check if the 'info frame' information for
    # 'script' passes through properly.
    spec::Process $script

    # state keys
    # - package
    # - name
    # - stem
    # - objtypevar
    # - intrep	    = C name of int.rep type.
    # - constructor | list        | 
    # - get         | C fragments | optional, ...
    # - destructor  |             | optional, nothing
    # - copy        |             | optional, assignment
    # - 2string     |             | optional, error
    # - parse       |             | 
    # - support     |             | optional, nothing

    # Process... (post-processing)
    # Fill defaults for some optional fragments.

    ProcessType

    #Default destructor | Nulled if not defined.
    #Default copy       | Nulled copy defaults to assignment in core itself.

    if {![dict exists $state destructor]} {
	dict set state free NULL
	dict set state havefree 0
    }
    if {![dict exists $state copy]} {
	dict set state dupl NULL
	dict set state havedupl 0
    }

    Default constructor {
	OT_PTR (obj) = (void*) value;
    }
    Default get {
	*value = (@intrep@) OT_PTR (obj);
    }
    Default 2string
    Default parse {
	Tcl_AppendResult (interp, "No string representation available.");
	return TCL_ERROR;
    }
    Default support 

    ProcessFragment constructor "\{\n" " " "\}"
    ProcessFragment destructor  "\{\n" " " "\}"
    ProcessFragment get         "\{\n" " " "\}"
    ProcessFragment copy        "\{\n" " " "\}"
    ProcessFragment 2string     "\{\n" " " "\}"
    ProcessFragment parse       "\{\n" " " "\}"
    ProcessFragment support     "" \n ""

    # Data needed from the specification...
    # - C type of the intrep.
    # - How to create a Tcl_Obj* with intrep.
    # - How to retrieve and return an intrep.
    # - Base names for the create/retrieve functions.
    #   (These are the public functions of the generated code)
    #   (Could be defaulted to objtype name, or full stem).
    # - Can we support generating the necessary stubs decls
    #   for the public functions ?
    # - Code to release the intrep
    # - Code to duplicate the intrep
    # - Code to generate string from intrep
    # - Code to generate intrep from string

    GenerateCode

    unset state
    return
}

proc ::critcl::objtype::Camelize {s} {
    set r {}
    foreach e [split $s _] { lappend r [string totitle $e] }
    return [join $r {}]
}

proc ::critcl::objtype::ProcessType {} {
    variable state
    if {[dict exists $state intrep]} return

    return -code error "Mandatory intrep C type is missing."
}

proc ::critcl::objtype::Default {key args} {
    variable state
    if {[dict exists $state $key]} return
    dict set state $key $args
    return
}

proc ::critcl::objtype::ProcessFragment {key prefix sep suffix} {
    # Process code fragments into a single block, if any.
    # Ensure it exists, even if empty. Required by template.
    # Optional in specification.

    variable state
    if {![dict exists $state $key]} {
	set new {}
    } else {
	set new ${prefix}[join [dict get $state $key] $suffix$sep$prefix]$suffix
    }
    dict set state $key $new
    return
}

proc ::critcl::objtype::GenerateCode {} {
    variable state

    set stem     [dict get $state stem]
    set hdr      ${stem}_objtype.h
    set header   [file join [critcl::cache] $hdr]

    file mkdir [critcl::cache]
    set template [Template objtype.h]
    #puts T=[string length $template]
    critcl::util::Put $header [string map [MakeMap] $template]

    critcl::ccode "#include <$hdr>"

    if {[dict get $state api_public]} {
	set n [dict get $state api_new]
	set f [dict get $state api_from]
	set t [dict get $state intrep]

	critcl::api function Tcl_Obj* $n [list $t value]
	critcl::api function int      $f [list Tcl_Interp* interp Tcl_Obj* obj ${t}* value]
    }
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

proc ::critcl::objtype::Dedent {pfx text} {
    set result {}
    foreach l [split $text \n] {
	lappend result [regsub ^$pfx $l {}]
    }
    join $result \n
}

# # ## ### ##### ######## ############# #####################
##
# Internal: All the helper commands providing access to the system
# state to the specification commands (see next section)
##
# # ## ### ##### ######## ############# #####################

proc ::critcl::objtype::CodeFragment {section code} {
    variable state
    set code [string trim $code \n]
    if {$code ne {}} {
	dict lappend state $section $code
    }
    return
}

proc ::critcl::objtype::Set {key value} {
    variable state
    dict set state $key $value
    return
}

# # ## ### ##### ######## ############# #####################
##
# Internal: Namespace holding the objtype specification commands. The
# associated state resides in the outer namespace, as do all the
# procedures actually accessing that state (see above). Treat it like
# a sub-package, with a proper API.
##
# # ## ### ##### ######## ############# #####################

namespace eval ::critcl::objtype::spec {}

proc ::critcl::objtype::spec::Process {script} {
    # Note how this script is evaluated within the 'spec' namespace,
    # providing it with access to the specification methods.

    # Point the global namespace resolution into the spec namespace,
    # to ensure that the commands are properly found even if the
    # script moved through helper commands and other namespaces.

    # Note that even this will not override the builtin 'variable'
    # command with ours, which is why ours is now called
    # 'insvariable'.

    namespace eval :: [list namespace path [list [namespace current] ::]]

    eval $script

    namespace eval :: {namespace path {}}
    return
}

proc ::critcl::objtype::spec::intrep {name} {
    # Declare C type of the internal representation.
    # Must be specified.
    ::critcl::objtype::Set intrep $name
    return
}

proc ::critcl::objtype::spec::api {name} {
    newobj ${name}NewObj
    getobj ${name}GetFromObj
    return
}

proc ::critcl::objtype::spec::fromobj {name} {
    ::critcl::objtype::Set api_from $name
    return
}

proc ::critcl::objtype::spec::newobj {name} {
    ::critcl::objtype::Set api_new $name
    return
}

proc ::critcl::objtype::spec::stubs {} {
    ::critcl::objtype::Set api_public 1
    return
}

proc ::critcl::objtype::spec::constructor {code} {
    # create internal representation.
    # - argument list ?
    ::critcl::objtype::CodeFragment constructor $code;#[critcl::at::get*]$code
    return
}

proc ::critcl::objtype::spec::get {code} {
    # return internal representation.
    # optional.
    # Default is to return obj->internalRep.otherValuePtr
    # Cast to the representation type.
    # - environment: 
    ::critcl::objtype::CodeFragment get $code;#[critcl::at::get*]$code
    return
}

proc ::critcl::objtype::spec::destructor {code} {
    # release intrep. optional. default: nothing
    ::critcl::objtype::CodeFragment destructor [critcl::at::get*]$code
    return
}

proc ::critcl::objtype::spec::copy {code} {
    # duplicate intrep. optional. default: C assignment.
    ::critcl::objtype::CodeFragment copy $code;#[critcl::at::get*]$code
    return
}

proc ::critcl::objtype::spec::2string {code} {
    # generate string from intrep. optional. default: error.
    ::critcl::objtype::CodeFragment 2string $code;#[critcl::at::get*]$code
    return
}

proc ::critcl::objtype::spec::parse {code} {
    # parse string, generate intrep.
    ::critcl::objtype::CodeFragment parse $code;#[critcl::at::get*]$code
    return
}

proc ::critcl::objtype::spec::support {code} {
    # additional support code. optional. default: nothing
    ::critcl::objtype::CodeFragment support $code;#[critcl::at::get*]$code
    return
}

# # ## ### ##### ######## ############# #####################
## State

namespace eval ::critcl::objtype {
    variable selfdir [file dirname [file normalize [info script]]]
}

# # ## ### ##### ######## ############# #####################
## Export API

namespace eval ::critcl::objtype {
    namespace export define
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## Ready
return
