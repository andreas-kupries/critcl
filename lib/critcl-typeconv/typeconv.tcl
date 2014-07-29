# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of type conversions used by "cproc" and
# equivalent commands in other packages (see critcl::class for
# examples).

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.
# - provide better access to the functionality by other package (ex:
#   critcl::class).

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require critcl::at     ;# Management of #line pragmas in C code.
package require debug          ;# debug narrative

package provide critcl::typeconv 4

namespace eval ::critcl::typeconv {}

debug level  critcl/typeconv
debug prefix critcl/typeconv {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.
## - Define an argument type with conversion
## - Define support code for an argument type.
## - Define a result type with conversion
## - Retrieve argument type information: var type
## - Retrieve argument type information: parameter type
## - Retrieve argument type information: conversion C code
## - Retrieve argument type information: supporting C code
## - Retrieve result type information: C type
## - Retrieve result type information: conversion C code

proc ::critcl::typeconv::arg-def {name conversion {ctypevar {}} {ctypearg {}}} {
    debug.critcl/typeconv {}
    variable atypevar
    variable atypearg
    variable aconv
    variable asupport

    # ctypevar - C type of C variable holding the argument.
    # ctypearg - C type of formal C function argument.

    if {[info exists aconv($name)]} {
	Error "Illegal duplicate definition of '$name'." \
	    ARGUMENT DUPLICATE $name
    }

    set csupport {}

    # Handle aliases by copying the original definition.
    if {$conversion eq "="} {
	# "ctypevar" here == name of the definition to make a copy of.
	CheckArgument $ctypevar

	set conversion $aconv($ctypevar) 
	set ctypearg   $atypearg($ctypevar)
	set ctypevar   $atypevar($ctypevar)
	set csupport   $asupport($ctypevar)
    } else {
	# XXX Use of new critcl::at package.
	lassign [at header $conversion] leadoffset conversion
	set conversion "\t\{\n[at caller! $leadoffset]\t[string trim $conversion] \}"

	if {$ctypevar eq {}} {
	    set ctypevar $name
	}
	if {$ctypearg eq {}} {
	    set ctypearg $name
	}
    }

    set aconv($name)    $conversion
    set atypevar($name) $ctypevar
    set atypearg($name) $ctypearg
    set asupport($name) $csupport
    return
}

proc ::critcl::typeconv::arg-set-support {name code} {
    debug.critcl/typeconv {}
    variable aconv
    variable asupport

    CheckArgument $name
    if {$asupport($name) ne {}} {
	Error "Illegal duplicate support-code of '$name'." \
	    ARGUMENT SUPPORT-DUPLICATE $name
    }

    lappend lines "#ifndef CRITCL_$name"
    lappend lines "#define CRITCL_$name"
    lappend lines $code
    lappend lines "#endif"

    set asupport($name) [join $lines \n]\n
    return
}

proc ::critcl::typeconv::result-def {name conversion {ctype {}}} {
    debug.critcl/typeconv {}
    variable rtype
    variable rconv

    if {[info exists rconv($name)]} {
	Error "Illegal duplicate definition of '$name'." \
	    RESULT DUPLICATE $name
    }

    # Handle aliases by copying the original definition.
    if {$conversion eq "="} {
	# "ctype" here == name of the definition to make a copy of.
	CheckResult $ctype

	set conversion $rconv($ctype) 
	set ctype      $rtype($ctype)
    } else {
	# XXX use critcl::at
	lassign [at header $conversion] leadoffset conversion
	set conversion [at caller! $leadoffset]\t[string trimright $conversion]

	if {$ctype eq {}} {
	    set ctype $name
	}
    }

    set rconv($name) $conversion
    set rtype($name) $ctype
    return
}

proc ::critcl::typeconv::arg-get-var-type {type} {
    debug.critcl/typeconv {}
    variable atypevar
    CheckArgument $type
    return $atypevar($type)
}

proc ::critcl::typeconv::arg-get-arg-type {type} {
    debug.critcl/typeconv {}
    variable atypearg
    CheckArgument $type
    return $atypearg($type)
}

proc ::critcl::typeconv::arg-get-conv {type} {
    debug.critcl/typeconv {}
    variable aconv
    CheckArgument $type
    return $aconv($type)
}

proc ::critcl::typeconv::arg-get-support {type} {
    debug.critcl/typeconv {}
    variable asupport
    CheckArgument $type
    return $asupport($type)
}

proc ::critcl::typeconv::result-get-type {type} {
    debug.critcl/typeconv {}
    variable rtype
    CheckResult $type
    return $rtype($type)
}

proc ::critcl::typeconv::result-get-code {type} {
    debug.critcl/typeconv {}
    variable rconv
    CheckResult $type
    return $rconv($type)
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::typeconv {
    # Make relevant #line management commands available.
    namespace import ::critcl::at

    # Two databases, for argument- and result-types.

    # Conversion maps, Tcl types for procedure arguments and results
    # to C types and code fragments for the conversion between the
    # realms.

    # I. arguments
    #
    # The code fragments stored in "aconv" have the following
    # environment (placeholders, variables):
    # 'ip' - C variable, Tcl_Interp* of the interpreter providing the arguments.
    # '@@' - Tcl_Obj* valued expression returning the Tcl argument value.
    # '@A' - Name of the C-level argument variable.

    variable  atypevar ; array set atypevar {}
    variable  atypearg ; array set atypearg {}
    variable  aconv    ; array set aconv    {}
    variable  asupport ; array set asupport {}

    # atypevar :: name -> C type of C variable holding the argument.
    # atypearg :: name -> C type of formal C function argument.
    # aconv    :: name -> C code fragment converting from Tcl to C.
    # asupport :: name -> C code fragment supporting the conversion code.
    #
    # where name == Symbolic (cri)tcl name of the argument type.

    # Mapping from cproc result to C result type of the function.
    # This is also the C type of the helper variable holding the result.
    # NOTE: 'void' is special, as it has no result, nor result variable.

    # The code fragments stored in "rconv" have the following
    # environment (placeholders, variables):
    # 'rv' - variable capturing the return value of the C function.
    # 'ip' - variable containing pointer to the interp to set the result into.

    variable  rtype ; array set rtype {}
    variable  rconv ; array set rconv {}

    # rtype :: name -> C type of C variable holding the result.
    # rconv :: name -> C code fragment converting from Tcl to C.
    #
    # where name == Symbolic (cri)tcl name of the return type.
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::typeconv::CheckResult {type} {
    debug.critcl/typeconv {}
    variable rconv
    if {[info exists rconv($type)]} return
    Error "Unknown result type '$type'" \
	RESULT UNKNOWN $type
}

proc ::critcl::typeconv::CheckArgument {type} {
    debug.critcl/typeconv {}
    variable aconv
    if {[info exists aconv($type)]} return
    Error "Unknown argument type '$type'" \
	ARGUMENT UNKNOWN $type
}

proc ::critcl::typeconv::Error {msg args} {
    debug.critcl/typeconv {}
    set code [linsert $args 0 CRITCL TYPECONV]
    return -code error -errorcode $code $msg
}

# # ## ### ##### ######## ############# #####################
## Initialization

apply {{} {
    # Define all the standard types provided by critcl.

    arg-def int {
	if (Tcl_GetIntFromObj(interp, @@, &@A) != TCL_OK) return TCL_ERROR;
    }
    arg-def boolean {
	if (Tcl_GetBooleanFromObj(interp, @@, &@A) != TCL_OK) return TCL_ERROR;
    } int int

    arg-def bool = boolean

    arg-def long {
	if (Tcl_GetLongFromObj(interp, @@, &@A) != TCL_OK) return TCL_ERROR;
    }

    arg-def double {
	if (Tcl_GetDoubleFromObj(interp, @@, &@A) != TCL_OK) return TCL_ERROR;
    }
    arg-def float {
	double t;
	if (Tcl_GetDoubleFromObj(interp, @@, &t) != TCL_OK) return TCL_ERROR;
	@A = (float) t;
    }

    arg-def char* {
	@A = Tcl_GetString(@@);
    }

    arg-def pstring {
	@A.s = Tcl_GetStringFromObj(@@, &(@A.len));
	@A.o = @@;
    } critcl_pstring critcl_pstring

    arg-set-support pstring {
	typedef struct critcl_pstring {
	    Tcl_Obj* o;
	    char*    s;
	    int      len;
	} critcl_pstring;
    }

    arg-def list {
	if (Tcl_ListObjGetElements (interp, @@, &(@A.c), &(@A.v)) != TCL_OK) return TCL_ERROR;
	@A.o = @@;
    } critcl_list critcl_list

    arg-set-support list {
	typedef struct critcl_list {
	    Tcl_Obj*  o;
	    Tcl_Obj** v;
	    int       c;
	} critcl_list;
    }

    arg-def Tcl_Obj* {
	@A = @@;
    }

    arg-def object = Tcl_Obj*

    ## The next set of argument types looks to be very broken. We are
    ## keeping them for now, but declare them as DEPRECATED. Their
    ## documentation will be removed in version 3.2, and their
    ## implementation in 3.3 as well, fully exterminating them.

    arg-def int* {
	/* Raw pointer in binary Tcl value */
	@A = (int*) Tcl_GetByteArrayFromObj(@@, NULL);
	Tcl_InvalidateStringRep(@@);
    }
    arg-def float* {
	/* Raw pointer in binary Tcl value */
	@A = (float*) Tcl_GetByteArrayFromObj(@@, NULL);
    }
    arg-def double* {
	/* Raw pointer in binary Tcl value */
	@A = (double*) Tcl_GetByteArrayFromObj(@@, NULL);
    }
    arg-def bytearray {
	/* Raw binary string. Length information is _NOT_ propagated */
	@A = (char*) Tcl_GetByteArrayFromObj(@@, NULL);
	Tcl_InvalidateStringRep(@@);
    } char* char*

    arg-def rawchar  = bytearray
    arg-def rawchar* = bytearray

    # Declare the standard result types for cproc.
    # System still has special case code for:
    # - void (no rv result variable).

    result-def void {
	return TCL_OK;
    }

    result-def ok {
	return rv;
    } int

    result-def int {
	Tcl_SetObjResult(interp, Tcl_NewIntObj(rv));
	return TCL_OK;
    }

    result-def boolean = int
    result-def bool    = int

    result-def long {
	Tcl_SetObjResult(interp, Tcl_NewLongObj(rv));
	return TCL_OK;
    }

    result-def double {
	Tcl_SetObjResult(interp, Tcl_NewDoubleObj(rv));
	return TCL_OK;
    }
    result-def float {
	Tcl_SetObjResult(interp, Tcl_NewDoubleObj(rv));
	return TCL_OK;
    }

    # Static and volatile strings. Duplicate.

    result-def char* {
	Tcl_SetObjResult(interp, Tcl_NewStringObj(rv,-1));
	return TCL_OK;
    }

    result-def {const char*} {
	Tcl_SetObjResult(interp, Tcl_NewStringObj(rv,-1));
	return TCL_OK;
    }

    result-def vstring = char*

    # Dynamic strings, allocated via Tcl_Alloc.
    #
    # We are avoiding the Tcl_Obj* API here, as its use requires an
    # additional duplicate of the string, churning memory and
    # requiring more copying.
    #   Tcl_SetObjResult(interp, Tcl_NewStringObj(rv,-1));
    #   Tcl_Free (rv);

    result-def string {
	Tcl_SetResult (interp, rv, TCL_DYNAMIC);
	return TCL_OK;
    } char*

    result-def dstring = string

    result-def Tcl_Obj* {
	if (rv == NULL) { return TCL_ERROR; }
	Tcl_SetObjResult(interp, rv);
	Tcl_DecrRefCount(rv);
	return TCL_OK;
    }

    result-def object = Tcl_Obj*

    # Done ...
    return
} ::critcl::typeconv}

# # ## ### ##### ######## ############# #####################
## Ready
return
