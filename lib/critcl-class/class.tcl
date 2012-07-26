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

namespace eval ::critcl::class {}

# # ## ### ##### ######## ############# #####################
## API: Generate the declaration and implementation files for the class.

proc ::critcl::class::define {classname script} {
    variable state

    catch { unset state }

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

    set stem ${package}_$cns$cclassname

    dict set state package     $package
    dict set state class       $classname
    dict set state stem        $stem
    dict set state classtype   ${stem}_CLASS
    dict set state method      names {}
    dict set state classmethod names {}

    # Check if the 'info frame' information for 'script' passes through properly.
    spec::Process $script

    #puts "@@@ <<$state>>"

    ProcessIncludes
    ProcessExternalType
    ProcessInstanceVariables
    ProcessClassVariables

    ProcessMethods method
    ProcessMethods classmethod

    ProcessFragment classconstructor "\{\n" " " "\}"
    ProcessFragment classdestructor  "\{\n" " " "\}"
    ProcessFragment constructor      "\{\n" " " "\}"
    ProcessFragment postconstructor  "\{\n" " " "\}"
    ProcessFragment destructor       "\{\n" " " "\}"
    ProcessFragment support          "" \n ""

    GenerateCode

    unset state
    return
}

proc ::critcl::class::ProcessIncludes {} {
    variable state
    if {[dict exists $state include]} {
	ProcessFragment include "#include <" "\n" ">"
	dict set state includes [dict get $state include]
	dict unset state include
    } else {
	dict set state includes {/* No inclusions */}
    }
    return
}

proc ::critcl::class::ProcessExternalType {} {
    variable state
    if {![dict exists $state instancetype]} return

    # Handle external C type for instances.
    set itype [dict get $state instancetype]
    dict set state ivardecl    "    $itype instance"
    dict set state ivarrelease ""
    dict set state ivarerror   ""
    dict set state itypedecl   "/* External type for instance state: $itype */"

    # For ProcessMethods
    dict set state method typedef $itype
    return
}

proc ::critcl::class::ProcessInstanceVariables {} {
    variable state

    if {![dict exists $state variable]} {
	if {![dict exists $state instancetype]} {
	    # We have neither external type, nor instance variables.
	    # Fake ourselves out, recurse.
	    dict set state variable names {}
	    ProcessInstanceVariables itype
	    return
	}

	# For ProcessMethods
	dict set state method menum   M_EMPTY
	dict set state method typekey @instancetype@
	dict set state method prefix  {}
	dict set state method startn  {}
	dict set state method starte  {}
	return
    }

    # Convert the set of instance variables (which can be empty) into
    # a C instance structure type declaration, plus variable name.

    set itype [dict get $state stem]_INSTANCE

    set decl {}
    lappend decl "typedef struct ${itype}__ \{"
    lappend decl "    Tcl_Command cmd;"

    foreach fname [dict get $state variable names] {
	set ctype   [dict get $state variable def $fname ctype]
	set vloc    [dict get $state variable def $fname loc]
	set comment [dict get $state variable def $fname comment]

	set field "$vloc    $ctype $fname;"
	if {$comment ne {}} {
	    append field " /* $comment */"
	}
	lappend decl $field
    }

    lappend decl "\} ${itype}__;"
    lappend decl "typedef struct ${itype}__* $itype;"

    dict set state instancetype $itype
    dict set state ivardecl    "    $itype instance = ($itype) ckalloc (sizeof (${itype}__))"
    dict set state ivarerror   "error:\n    ckfree ((char*) instance);\n    return NULL;"
    dict set state ivarrelease "    ckfree ((char*) instance)"
    dict set state itypedecl   [join $decl \n]

    # Extend the constructor
    dict lappend state postconstructor "\tinstance->cmd = cmd;\n    "

    # And handle a reference to a class structure.
    if {[dict exists $state variable def class]} {
	dict append state ivardecl ";\n    instance->class = class"
    }

    # For ProcessMethods
    dict set state method typedef $itype
    dict set state method menum   M_EMPTY
    dict set state method typekey @instancetype@
    dict set state method prefix  {}
    dict set state method startn  {}
    dict set state method starte  {}
    return
}

proc ::critcl::class::ProcessClassVariables {} {
    variable state

    # For ProcessMethods
    dict set state classmethod typedef [dict get $state classtype]
    dict set state classmethod menum   {}
    dict set state classmethod typekey @classtype@
    dict set state classmethod prefix  class_
    dict set state classmethod startn  "\n"
    dict set state classmethod starte  ",\n"
    dict set state ctypedecl {}

    if {![dict exists $state classvariable]} return

    # Convert class variables  into class type field declarations.

    set decl {}
    lappend decl "/* # # ## ### ##### ######## User: Class variables */"

    foreach fname [dict get $state classvariable names] {
	set ctype   [dict get $state classvariable def $fname ctype]
	set vloc    [dict get $state classvariable def $fname loc]
	set comment [dict get $state classvariable def $fname comment]

	set field "$vloc$ctype $fname;"
	if {$comment ne {}} {
	    append field " /* $comment */"
	}
	lappend decl $field
    }

    lappend decl "/* # # ## ### ##### ######## */"

    dict set state ctypedecl "    [join $decl "\n    "]\n"
    return
}

proc ::critcl::class::Max {v s} {
    upvar 1 $v max
    set l [string length $s]
    if {$l < $max} return
    set max $l
    return
}

proc ::critcl::class::ProcessMethods {key} {
    variable state
    # Process method declarations. Ensure that the names are listed in
    # alphabetical order, to be nice.

    # From Process(Instance|Class)Variables
    set pfx  [dict get $state $key prefix]
    set stn  [dict get $state $key startn]
    set ste  [dict get $state $key starte]

    if {[dict exists $state $key names] &&
	[llength [dict get $state $key names]]} {
	set map [list @stem@ [dict get $state stem] \
		     [dict get $state $key typekey] \
		     [dict get $state $key typedef]]

	set maxe 0
	set maxn 0
	foreach name [lsort -dict [dict get $state $key names]] {
	    Max maxn $name
	    Max maxe [dict get $state $key def $name enum]
	}
	incr maxn 3

	foreach name [lsort -dict [dict get $state $key names]] {
	    set enum                    [dict get $state $key def $name enum]
	    set case   [string map $map [dict get $state $key def $name case]]
	    set code   [string map $map [dict get $state $key def $name code]]
	    set syntax [string map $map [dict get $state $key def $name syntax]]

	    lappend names "[format %-${maxn}s \"$name\",] $syntax"
	    lappend enums "[format %-${maxe}s $enum] $syntax"
	    regexp {(:.*)$} $case tail
	    set case "case [format %-${maxe}s $enum]$tail"
	    lappend cases $case
	    lappend codes $code
	}

	dict set state ${pfx}method_names           "${stn}    [join $names  "\n    "]"
	dict set state ${pfx}method_enumeration     "${ste}    [join $enums ",\n    "]"
	dict set state ${pfx}method_dispatch        "${stn}\t[join $cases \n\t]"
	dict set state ${pfx}method_implementations [join $codes \n\n]
    } else {
	set enums [dict get $state $key menum]
	if {[llength $enums]} {
	    set enums "${ste}    [join $enums ",\n    "]"
	}

	dict set state ${pfx}method_names           {}
	dict set state ${pfx}method_enumeration     $enums
	dict set state ${pfx}method_dispatch        {}
	dict set state ${pfx}method_implementations {}
    }


    dict unset state $key
    return
}

proc ::critcl::class::ProcessFragment {key prefix sep suffix} {
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

proc ::critcl::class::GenerateCode {} {
    variable state

    set stem     [dict get $state stem]
    set class    [dict get $state class]
    set hdr      ${stem}_class.h
    set header   [file join [critcl::cache] $hdr]

    file mkdir [critcl::cache]
    set template [Template class.h]
    #puts T=[string length $template]
    critcl::util::Put $header [string map [MakeMap] $template]

    critcl::ccode "#include <$hdr>"
    uplevel 2 [list critcl::ccommand $class ${stem}_ClassCommand]
    return
}

proc ::critcl::class::MakeMap {} {
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

proc ::critcl::class::Template {path} {
    variable selfdir
    set path $selfdir/$path
    #puts T=$path
    return [critcl::util::Get $path]
}

proc ::critcl::class::Dedent {pfx text} {
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

proc ::critcl::class::Include {header} {
    # Name of an API to include in the generated code.
    variable state
    dict lappend state include $header
    return
}

proc ::critcl::class::ExternalType {name} {
    # Declaration of the C type to use for the object state.  This
    # type is expected to be declared externally. It allows us to use
    # a 3rd party structure directly. Cannot be specified if instance
    # and/or class variables for our own structures have been declared
    # already.

    variable state

    if {[dict exists $state variable]} {
	return -code error "Invalid external instance type. Instance variables already declared."
    }
    if {[dict exists $state classvariable]} {
	return -code error "Invalid external instance type. Class variables already declared."
    }

    dict set state instancetype $name
    return
}

proc ::critcl::class::Variable {ctype name comment vloc} {
    # Declaration of an instance variable. In other words, a field in
    # the C structure for instances. Cannot be specified if an
    # external "type" has been specified already.

    variable state

    if {[dict exists $state instancetype]} {
	return -code error \
	    "Invalid instance variable. External instance type already declared."
    }

    if {[dict exists $state variable def $name]} {
	return -code error "Duplicate definition of instance variable \"$name\""
    }

    dict update state variable f {
	dict lappend f names $name
    }
    dict set state variable def $name ctype   $ctype
    dict set state variable def $name loc     $vloc
    dict set state variable def $name comment [string trim $comment]

    if {[llength [dict get $state variable names]] == 1} {
	# Generate a destroy method. We can do that, because we know
	# that the instance structure will have a field named 'cmd'.
	# Declared here.
	dict set state variable def cmd {}
	critcl::at::here ; MethodExplicit destroy proc {} ok {
	    Tcl_DeleteCommandFromToken(interp, instance->cmd);
	    return TCL_OK;
	}
    }
    return
}

proc ::critcl::class::ClassVariable {ctype name comment vloc} {
    # Declaration of a class variable. In other words, a field in the
    # C structure for the class. Cannot be specified if a an external
    # "type" has been specified already.

    variable state

    if {[dict exists $state instancetype]} {
	return -code error \
	    "Invalid class variable. External instance type already declared."
    }

    if {[dict exists $state classvariable def $name]} {
	return -code error "Duplicate definition of class variable \"$name\""
    }

    dict update state classvariable c {
	dict lappend c names $name
    }
    dict set state classvariable def $name ctype   $ctype
    dict set state classvariable def $name loc     $vloc
    dict set state classvariable def $name comment [string trim $comment]

    if {[llength [dict get $state classvariable names]] == 1} {
	# On declaration of the first class variable we declare an
	# instance variable which provides instances with a reference
	# to their class (structure).
	Variable @classtype@ class {Reference to class (variables)} [critcl::at::here!]
    }
    return
}

proc ::critcl::class::Constructor {code} {
    CodeFragment constructor $code
    return
}

proc ::critcl::class::PostConstructor {code} {
    CodeFragment postconstructor $code
    return
}

proc ::critcl::class::Destructor {code} {
    CodeFragment destructor $code
    return
}

proc ::critcl::class::ClassConstructor {code} {
    CodeFragment classconstructor $code
    return
}

proc ::critcl::class::ClassDestructor {code} {
    CodeFragment classdestructor $code
    return
 }

proc ::critcl::class::Support {code} {
    CodeFragment support $code
    return
}

proc ::critcl::class::MethodExternal {name function details} {
    MethodCheck method instance $name

    set map {}
    if {[llength $details]} {
	set  details [join $details {, }]
	lappend map objv "objv, $details"
	set details " ($details)"
    }

    MethodDef method instance $name [MethodEnum method $name] {} $function $map \
	"/* $name : External function @function@$details */"
    return
}

proc ::critcl::class::MethodExplicit {name mtype arguments args} {
    # mtype in {proc, command}
    MethodCheck method instance $name

    set enum     [MethodEnum method $name]
    set function ${enum}_Cmd
    set cdimport "[critcl::at::here!]    @instancetype@ instance = (@instancetype@) clientdata;"

    if {$mtype eq "proc"} {
	# Method is cproc.
	# |args| == 2, args => rtype, body
	# arguments is (argtype argname...)
	# (See critcl::cproc for full details)

	lassign $args rtype body

	set body   [critcl::at::get][string trimright $body]
	set syntax "/* Syntax: <class> $name [critcl::argnames $arguments] */"
	set body   "\n    $syntax\n$cdimport\n    $body"

	critcl::divert CMETHOD
	critcl::cproc $function $arguments $rtype $body \
	    -cname 1 -pass-cdata 1
	set code [critcl::divertend]

    } else {
	# Method is ccommand.
	# |args| == 1, args => body
	lassign $args body

	if {$arguments ne {}} {set arguments " $arguments"}
	set body   [critcl::at::get][string trimright $body]
	set syntax "/* Syntax: <class> $name$arguments */"
	set body   "\n    $syntax\n$cdimport\n    $body"

	critcl::divert CMETHOD
	critcl::ccommand $function {} $body \
	    -cname 1
	set code [critcl::divertend]
    }

    MethodDef method instance $name $enum $syntax $function {} $code
    return
}

proc ::critcl::class::ClassMethodExternal {name function details} {
    MethodCheck classmethod class $name

    set map {}
    if {[llength $details]} {
	lappend map objv "objv, [join $details {, }]"
    }

    MethodDef classmethod class $name [MethodEnum method $name] {} $function $map \
	"/* $name : External function @function@ */"
    return
}

proc ::critcl::class::ClassMethodExplicit {name mtype arguments args} {
    # mtype in {proc, command}
    MethodCheck classmethod class $name

    set enum     [MethodEnum method $name]
    set function ${enum}_Cmd
    set cdimport "[critcl::at::here!]    @classtype@ class = (@classtype@) clientdata;"

    if {$mtype eq "proc"} {
	# Method is cproc.
	# |args| == 2, args => rtype, body
	# arguments is (argtype argname...)
	# (See critcl::cproc for full details)

	lassign $args rtype body

	set body   [critcl::at::get][string trimright $body]
	set syntax "/* Syntax: <class> $name [critcl::argnames $arguments] */"
	set body   "\n    $syntax\n$cdimport\n    $body"

	critcl::divert CMETHOD
	critcl::cproc $function $arguments $rtype $body \
	    -cname 1 -pass-cdata 1
	set code [critcl::divertend]

    } else {
	# Method is ccommand.
	# |args| == 1, args => body
	lassign $args body

	if {$arguments ne {}} {set arguments " $arguments"}
	set body   [critcl::at::get][string trimright $body]
	set syntax "/* Syntax: <class> $name$arguments */"
	set body   "\n    $syntax\n$cdimport\n    $body"

	critcl::divert CMETHOD
	critcl::ccommand $function {} $body \
	    -cname 1
	set code [critcl::divertend]
    }

    MethodDef classmethod class $name $enum $syntax $function {} $code
    return
}

proc ::critcl::class::MethodCheck {section label name} {
    variable state
    if {[dict exists $state $section def $name]} {
	return -code error "Duplicate definition of $label method \"$name\""
    }
    return
}

proc ::critcl::class::MethodEnum {section name} {
    variable state
    # Compute a C enum identifier from the (class) method name.

    # To avoid trouble we have to remove any non-alphabetic
    # characters. A serial number is required to distinguish methods
    # which would, despite having different names, transform to the
    # same C enum identifier.

    regsub -all -- {[^a-zA-Z0-9_]} $name _ name
    regsub -all -- {_+} $name _ name

    set serial [llength [dict get $state $section names]]

    return @stem@_M_${serial}_[string toupper $name]
}

proc ::critcl::class::MethodDef {section var name enum syntax function xmap code} {
    variable state

    set case  "case $enum: return @function@ ($var, interp, objc, objv); break;"
    set case [string map $xmap $case]

    set map [list @function@ $function]

    dict update state $section m {
	dict lappend m names $name
    }
    dict set state $section def $name enum $enum
    dict set state $section def $name case   [string map $map $case]
    dict set state $section def $name code   [string map $map $code]
    dict set state $section def $name syntax [string map $map $syntax]
    return
}

proc ::critcl::class::CodeFragment {section code} {
    variable state
    set code [string trim $code \n]
    if {$code ne {}} {
	dict lappend state $section $code
    }
    return
}

# # ## ### ##### ######## ############# #####################
##
# Internal: Namespace holding the class specification commands. The
# associated state resides in the outer namespace, as do all the
# procedures actually accessing that state (see above). Treat it like
# a sub-package, with a proper API.
##
# # ## ### ##### ######## ############# #####################

namespace eval ::critcl::class::spec {}

proc ::critcl::class::spec::Process {script} {
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

proc ::critcl::class::spec::include {header} {
    ::critcl::class::Include $header
}

proc ::critcl::class::spec::type {name} {
    ::critcl::class::ExternalType $name
}

proc ::critcl::class::spec::insvariable {ctype name {comment {}} {constructor {}} {destructor {}}} {
    ::critcl::at::caller
    set vloc [critcl::at::get*]
    ::critcl::at::incrt $comment     ; set cloc [::critcl::at::get*]
    ::critcl::at::incrt $constructor ; set dloc [::critcl::at::get]


    ::critcl::class::Variable $ctype $name $comment $vloc

    if {$constructor ne {}} {
	::critcl::class::Constructor $cloc$constructor
    }
    if {$destructor ne {}} {
	::critcl::class::Destructor $dloc$destructor
    }

    return
}

proc ::critcl::class::spec::constructor {code {postcode {}}} {
    ::critcl::at::caller      ; set cloc [::critcl::at::get*]
    ::critcl::at::incrt $code ; set ploc [::critcl::at::get]

    if {$code ne {}} {
	::critcl::class::Constructor $cloc$code
    }
    if {$postcode ne {}} {
	::critcl::class::PostConstructor $ploc$postcode
    }
    return
}

proc ::critcl::class::spec::destructor {code} {
    ::critcl::class::Destructor [::critcl::at::caller!]$code
    return
}

proc ::critcl::class::spec::method {name op detail args} {
    # Syntax
    # (1) method <name> as      <function>  ...
    # (2) method <name> proc    <arguments> <rtype> <body>
    # (3) method <name> command <arguments> <body>
    #            name   op      detail      args__________

    if {$op eq "as"} {
	# instance method is external C function matching an ObjCmd in
	# signature, possibly with additional parameters at the end.
	#
	# detail = name of that function
	# args = values for the additional parameters, if any.

	::critcl::class::MethodExternal $name $detail $args
	return
    }

    # method is fully defined here.
    # op = proc|cmd|command

    # op == proc
    # detail  = argument syntax per cproc.
    # args[0] = r(esult)type
    # args[1] = body

    # op == command
    # detail  = argument syntax. not used in code, purely descriptive.
    # args[0] = body

    switch -exact -- $op {
	proc {
	    if {[llength $args] != 2} {
		return -code error "wrong#args"
	    }
	}
	cmd - command {
	    set op command
	    if {[llength $args] != 1} {
		return -code error "wrong#args"
	    }
	}
	default {
	    return -code error "Illegal method type \"$op\", expected one of cmd, command, or proc"
	}
    }

    ::critcl::at::caller
    ::critcl::at::incrt $detail
    ::critcl::class::MethodExplicit $name [string trim $detail] {*}$args
    return
}

proc ::critcl::class::spec::classvariable {ctype name {comment {}} {constructor {}} {destructor {}}} {
    ::critcl::at::caller
    set vloc [critcl::at::get*]
    ::critcl::at::incrt $comment     ; set cloc [::critcl::at::get*]
    ::critcl::at::incrt $constructor ; set dloc [::critcl::at::get]

    ::critcl::class::ClassVariable $ctype $name $comment $vloc

    if {$constructor ne {}} {
	::critcl::class::ClassConstructor $cloc$constructor
    }
    if {$destructor ne {}} {
	::critcl::class::ClassDestructor $dloc$destructor
    }
    return
}

proc ::critcl::class::spec::classconstructor {code} {
    ::critcl::class::ClassConstructor [::critcl::at::caller!]$code
    return
}

proc ::critcl::class::spec::classdestructor {code} {
    ::critcl::class::ClassDestructor [::critcl::at::caller!]$code
    return
}

proc ::critcl::class::spec::classmethod {name op detail args} {
    # Syntax
    # (1) classmethod <name> as      <function>  ...
    # (2) classmethod <name> proc    <arguments> <rtype> <body>
    # (3) classmethod <name> command <arguments> <body>
    #                 name   op      detail      args__________

    if {$op eq "as"} {
	# class method is external C function matching an ObjCmd in
	# signature, possibly with additional parameters at the end.
	#
	# detail = name of that function
	# args = values for the additional parameters, if any.

	::critcl::class::ClassMethodExternal $name $detail $args
	return
    }

    # class method is fully defined here.
    # op = proc|cmd|command

    # op == proc
    # detail  = argument syntax per cproc.
    # args[0] = r(esult)type
    # args[1] = body

    # op == command
    # detail  = argument syntax. not used in code, purely descriptive.
    # args[0] = body

    switch -exact -- $op {
	proc {
	    if {[llength $args] != 2} {
		return -code error "wrong#args"
	    }
	}
	cmd - command {
	    set op command
	    if {[llength $args] != 1} {
		return -code error "wrong#args"
	    }
	}
	default {
	    return -code error "Illegal method type \"$op\", expected one of cmd, command, or proc"
	}
    }

    ::critcl::at::caller
    ::critcl::at::incrt $detail
    ::critcl::class::ClassMethodExplicit $name $op [string trim $detail] {*}$args
    return
}

proc ::critcl::class::spec::support {code} {
    ::critcl::class::Support [::critcl::at::caller!]$code
    return
}

proc ::critcl::class::spec::method_introspection {} {
    ::critcl::class::spec::classvariable Tcl_Obj* methods {
	Cache for the list of method names.
    } {
	class->methods = ComputeMethodList (@stem@_methodnames);
	Tcl_IncrRefCount (class->methods);
    } {
	Tcl_DecrRefCount (class->methods);
	class->methods = NULL;
    }

    ::critcl::class::spec::support {
#ifndef CRITCL_CLASS__HAVE_COMPUTE_METHOD_LIST
#define CRITCL_CLASS__HAVE_COMPUTE_METHOD_LIST
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
#endif /* CRITCL_CLASS__HAVE_COMPUTE_METHOD_LIST */
    }

    ::critcl::class::spec::method methods proc {} ok {
	Tcl_SetObjResult (interp, instance->class->methods);
	return TCL_OK;
    }

    ::critcl::class::spec::classmethod methods proc {} ok {
	Tcl_SetObjResult (interp, class->methods);
	return TCL_OK;
    }
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
    namespace export define
    catch { namespace ensemble create } ; # 8.5+
}

# # ## ### ##### ######## ############# #####################
## Ready
return
