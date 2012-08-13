# -*- tcl -*- (critcl actually, Tcl + embedded C)
# sr.tcl --
#
#	Object-based rand number generators. The low-level math, i.e.
#	the rnmath functions, is provided by package 'rnmath'.
#
# Concept pulled out of and derived from tcllib/modules/simulation/random.tcl
# Copyright (c) 2007 by Arjen Markus <arjenmarkus@users.sourceforge.net>
#
# Critcl code generation and setup
# Copyright (c) 2011 by Andreas Kupries <andreas_kupris@users.sourceforge.net>
#
# Example of how to IMPORT a C-level stubs API through critcl v3.

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.4
package require critcl 3

critcl::buildrequirement {
    package require stubs::gen ; # Generator/iterator framework ...
}

namespace eval ::random {}

# # ## ### ##### ######## ############# #####################
## Configuration

critcl::license \
    {Arjen Markus, Andreas Kupries} \
    {BSD licensed.}

critcl::summary {Random number generator objects for Tcl.}

critcl::description {
    This package implements random number generator objects for
    Tcl. It uses the functions provided by package 'rnmath' for
    the actual math.
}

critcl::subject {random number generator}
# plus the distributions, see inside of 'genclass'

# # ## ### ##### ######## ############# #####################
## Dependencies.

# C random number generator functions.
set ::random::T [critcl::api import rnmath 1]

if {$::random::T eq {}} {
    critcl::error "Unable to work without programmatic access to the rnmath API, the expected rnmath.decls was not found."
}

# # ## ### ##### ######## ############# #####################
## Code generation commands converting a simple RNMATH declaration into
## the necessary C code for class and object commands.

proc ::random::wrap {} {
    variable T
    variable PS {}
    # Iterate over the slots in the stubs table and generator wrap code for each
    # rnmath_ function.
    ::stubs::gen::forall $T rnmath [namespace current]::Make 0

    # Finalize the parameter union ...
    set PS "typedef union RNMATHparam \{\n${PS}\} RNMATHparam;"

    # The union is placed into a generated header, getting around
    # ordering problems, namely that this is finalized after a number
    # of pieces needing the type are declared. The first such piece
    # has a #include to the generated header. Whic h works because the
    # build happens after everything is generated.

    # critcl::stash ? command
    file mkdir     [critcl::cache]
    set    c [open [critcl::cache]/sr_param.h w]
    puts  $c $PS
    close $c
    #critcl::ccode $PS
    return
}

proc ::random::Make {name decl index} {
    # Handle a single slot.

    variable PS ; # The code for the parameter union (RNMATHparam) is accumulated here.

    lassign $decl ftype fname farguments
    # ASSERT ftype == void
    if {$ftype ne "void"} return

    # ASSERT fname match rnmath_*
    if {![string match rnmath_* $fname]} return

    # Extract generator name from function name.
    regsub {^rnmath_} $fname {} rname

    # Split arguments into arguments and results. The latter are
    # recognized through their pointer types (Ending in '*').
    set arguments {}
    set rtypes    {}
    foreach a $farguments {
	lassign $a atype aname aflag
	# ASSERT aflag == {}
	if {[string index $atype end] eq "*"} {
	    lappend rtypes [string range $atype 0 end-1] $aname
	} else {
	    lappend arguments $atype $aname
	}
    }

    # Generate a structure to hold the function arguments.
    # This is added to PS and will become a union of all
    # parameter structures.
    append PS "    struct \{\n"
    foreach {atype aname} $arguments {
	append PS "        $atype $aname;\n"
    }
    append PS "    \} $rname;\n"

    # Invoke the actual code generator.
    critcl::msg -nonewline " ($rname)"
    genclass $rtypes $rname $arguments
    return
}

proc ::random::genclass {rtypes name arguments} {
    # Extend the meta data. Same as used by 'rnmath', to put them
    # together, near each other.
    critcl::subject "$name probability distribution"
    critcl::subject "probability distribution $name"
    critcl::subject "distribution $name"

    set ingest ""
    foreach {t a} $arguments {
	append  ingest        "\t    rnmathp->$name.$a = _$a;\n"
	lappend theparameters "rnmathp->${name}.$a"
    }

    set argnames     [critcl::argnames $arguments]
    set thearguments [join [critcl::argcsignature $arguments] {, }]
    set argvars      [indent "\t    " [join [critcl::argvardecls   $arguments] \n]]\n
    set argcheck     [indent "\t  "   [join [critcl::argconversion $arguments] \n]]\n

    if {[llength $rtypes] == 2} {
	# Single-value result. Variables for each, and construction of a list.

	lassign $rtypes t r

	append  resultvars     "\t    $t _$r;\n"
	append  resultvars     "\t    Tcl_Obj* _lv;\n"

	append  resultget      "\t    _lv = Tcl_New[cap $t]Obj (_$r);\n"
	append  thearguments  ", $t* $r"
	lappend theparameters "&_$r"
	set resultset "_lv"
    } else {
	# Multi-value result. Variables for each, and construction of a list.
	set lc 0
	foreach {t r} $rtypes {
	    append resultvars     "\t    $t _$r;\n"
	    append resultget      "\t    _lv\[$lc\] = Tcl_New[cap $t]Obj (_$r);\n"
	    append thearguments  ", $t* $r"
	    lappend theparameters "&_$r"
	    incr lc
	}
	append resultvars "\t    Tcl_Obj* _lv\[$lc\];\n"
	set resultset "Tcl_NewListObj ($lc,_lv)"
    }

    set theparameters [join $theparameters {, }]

    # Low-level math function generating the numbers. Imported from rnmath stubs.

    # Instance command for the generators. Invokes the math function
    # with the parameters it got through its client data.

    critcl::ccode [subst -nocommand {
	static int
	r_${name}_objcmd (ClientData cd, Tcl_Interp* interp, int objc, Tcl_Obj *CONST objv[])
	{
	    RNMATHparam* rnmathp = (RNMATHparam*) cd;
$resultvars
	    if (objc > 1) {
		Tcl_WrongNumArgs (interp, 1, objv, "");
		return TCL_ERROR;
	    }

	    rnmath_$name ($theparameters);
$resultget
	    Tcl_SetObjResult (interp, $resultset);
	    return TCL_OK;
	}
    }]

    # Class command for generators of this type. Creates instance
    # commands with proper client data.
    set np   [llength $argnames]
    set nmin [expr {$np + 1}]
    set nmax [expr {$np + 2}]

    critcl::ccommand ::random::$name {cd interp oc ov} [subst -nocommands {
	RNMATHparam* rnmathp;
	char* name;
$argvars
	if (oc == $nmin) {
	    name = NULL;
	} else if (oc == $nmax) {
	    name = Tcl_GetString (ov [1]);
	    ov++;
	} else {
	    Tcl_WrongNumArgs (interp, 1, ov, "?name? $argnames");
	    return TCL_ERROR;
	}

$argcheck
	rnmathp = RNMATHnewCmd (interp, "$name", name, r_${name}_objcmd);

	if (!rnmathp) {
	    return TCL_ERROR;
	}

$ingest
	return TCL_OK;
    }]

    return
}

proc ::random::cap {name} {
    return [string toupper [string index $name 0]][string range $name 1 end]
}

proc ::random::indent {prefix text} {
    return ${prefix}[join [split $text \n] \n$prefix]
}

# # ## ### ##### ######## ############# #####################
## Intro and shared/common/fixed code.

critcl::ccode {
    /* -*- c -*- */
    /* .................................................. */
    /* Global generator management, per interp */

#include <sr_param.h> /* Generated, see random::wrap */
#define PREFIX "random"

    typedef struct RNMATHglobal {
	long int counter;
	char buf [sizeof (PREFIX) + 40 + 40];
	/* 40 - generator type string, 40 - space for long integer */
    } RNMATHglobal;

    /* Union parameter structure for all generators. */
    /* So that we have one structure for all, and a single destructor function */
    /* We can't get around the need for multiple constructors for the different */
    /* generators */

    static void
    RNMATHglobalFree (ClientData cd, Tcl_Interp* interp)
    {
	ckfree((char*) cd);
    }

    static char*
    AutoName (Tcl_Interp* interp, char* rtype)
    {
#define KEY "package/random"

	Tcl_InterpDeleteProc* proc = RNMATHglobalFree;
	RNMATHglobal*            rnmathglobal;

	rnmathglobal = Tcl_GetAssocData (interp, KEY, &proc);
	if (rnmathglobal == NULL) {
	    rnmathglobal = (RNMATHglobal*) ckalloc (sizeof (RNMATHglobal));
	    rnmathglobal->counter = 0;

	    Tcl_SetAssocData (interp, KEY, proc,
			      (ClientData) rnmathglobal);
	}
	    
	rnmathglobal->counter ++;
	sprintf (rnmathglobal->buf, PREFIX "%s%d", rtype, rnmathglobal->counter);
	return rnmathglobal->buf;

#undef  KEY
    }

    static void
    RNMATHdeleteCmd (ClientData clientData)
    {
	/* Release the generator parameters */
	ckfree ((char*) clientData);
    }

    static RNMATHparam*
    RNMATHnewCmd (Tcl_Interp* interp, char* rtype, char* name, Tcl_ObjCmdProc p)
    {
	Tcl_Obj*    fqn;
	Tcl_CmdInfo ci;
	RNMATHparam*   rnmathp;

	if (!name) {
	    name = AutoName (interp, rtype);
	}

	if (!Tcl_StringMatch (name, "::*")) {
	    /* Relative name. Prefix with current namespace */

	    Tcl_Eval (interp, "namespace current");
	    fqn = Tcl_GetObjResult (interp);
	    fqn = Tcl_DuplicateObj (fqn);
	    Tcl_IncrRefCount (fqn);

	    if (!Tcl_StringMatch (Tcl_GetString (fqn), "::")) {
		Tcl_AppendToObj (fqn, "::", -1);
	    }
	    Tcl_AppendToObj (fqn, name, -1);
	} else {
	    fqn = Tcl_NewStringObj (name, -1);
	    Tcl_IncrRefCount (fqn);
	}
	Tcl_ResetResult (interp);

	if (Tcl_GetCommandInfo (interp,
				Tcl_GetString (fqn),
				&ci)) {
	    Tcl_Obj* err;

	    err = Tcl_NewObj ();
	    Tcl_AppendToObj    (err, "command \"", -1);
	    Tcl_AppendObjToObj (err, fqn);
	    Tcl_AppendToObj    (err, "\" already exists, unable to create generator", -1);
	    
	    Tcl_DecrRefCount (fqn);
	    Tcl_SetObjResult (interp, err);
	    return NULL;
	}

	rnmathp = (RNMATHparam*) ckalloc (sizeof (RNMATHparam));

	Tcl_CreateObjCommand (interp, Tcl_GetString (fqn),
			      p, (ClientData) rnmathp,
			      RNMATHdeleteCmd);

	Tcl_SetObjResult (interp, fqn);
	Tcl_DecrRefCount (fqn);

	return rnmathp;
    }

    static double
    RANDOM (void)
    {
	return random () / 2147483647.0;
    }
}

# # ## ### ##### ######## ############# #####################
## Genereate generators from the imported API.

::random::wrap

# # ## ### ##### ######## ############# #####################
## Finalization; drop helper commands, and provide the package.

unset  ::random::T
rename ::random::cap      {}
rename ::random::indent   {}
rename ::random::genclass {}
rename ::random::wrap     {}

package provide random 1
return
