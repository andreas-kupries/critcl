## -*- tcl -*-
# # ## ### ##### ######## ############# #####################
# Pragmas for MetaData Scanner.
# n/a

# CriTcl Utility Package for emap en- and decoder.
# Based on i-assoc.

package provide critcl::emap 1

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl    8.4     ; # Min supported version.
package require critcl 3.1.11  ; # make, include -- dict portability
package require critcl::iassoc

namespace eval ::critcl::emap {}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Embed C Code

proc critcl::emap::def {name dict} {
    # dict: Tcl symbolic name -> (C int value (1))
    #
    # (Ad 1) Can be numeric, or symbolic, as long as it is a C int
    #        expression in the end.

    # For the C level opt array we want the elements sorted alphabetically.
    set symbols [lsort -dict [dict keys $dict]]
    set i 0
    foreach s $symbols {
	set id($s) $i
	incr i
    }
    set last $i
 
    # Standard code using linear search to convert from C back to Tcl
    # (As it doesn't know order of values).

    dict for {sym value} $dict {
	set map [list @ID@ $id($sym) @SYM@ $sym @VALUE@ $value]

	append init \n[critcl::at::here!][string map $map {
	    data->c     [@ID@] = "@SYM@";
	    data->value [@ID@] = @VALUE@;
	    data->tcl   [@ID@] = Tcl_NewStringObj ("@SYM@", -1);
	    Tcl_IncrRefCount (data->tcl [@ID@]);
	}]

	append final \n[critcl::at::here!][string map $map {
	    Tcl_DecrRefCount (data->tcl [@ID@]);
	}]
    }
    append init \n "    data->c \[$last\] = NULL;"

    lappend map @NAME@  $name
    lappend map @UNAME@ [string toupper $name]
    lappend map @LAST@  $last

    # I. Generate a header file for inclusion by other parts of the
    #    package, i.e. csources. Include the header here as well, for
    #    the following blocks of code.
    #
    #    Declaration of the en- and decoder functions.

    critcl::include [critcl::make ${name}.h \n[critcl::at::here!][string map $map {
	#ifndef @NAME@_HEADER
	#define @NAME@_HEADER

	/* Encode a Tcl string into the corresponding state code */
	extern int
	@NAME@_encode (Tcl_Interp* interp,
		       Tcl_Obj*    state,
		       int*        result);

	/* Decode a state into the corresponding Tcl string */
	extern Tcl_Obj*
	@NAME@_decode (Tcl_Interp* interp,
		       int         state);

	#endif
    }]]

    # II: Generate the interp association holding the various
    #     conversion maps.

    critcl::iassoc def ${name}_iassoc {} \n[critcl::at::here!][string map $map {
	const char*    c     [@LAST@+1]; /* State name, C string */
	Tcl_Obj*       tcl   [@LAST@];   /* State name, Tcl_Obj*, sharable */
	int            value [@LAST@];   /* State code */
    }] $init $final

    # III: Generate encoder function: Conversion of Tcl state string
    #      into corresponding state code.

    critcl::ccode \n[critcl::at::here!][string map $map {
	int
	@NAME@_encode (Tcl_Interp* interp,
		       Tcl_Obj*    state,
		       int*        result)
	{
	    @NAME@_iassoc_data context = @NAME@_iassoc (interp);
	    int id;

	    if (Tcl_GetIndexFromObj (interp, state, context->c, "@NAME@", 0,
				     &id) != TCL_OK) {
		Tcl_SetErrorCode (interp, "@UNAME@", "STATE", NULL);
		return TCL_ERROR;
	    }

	    *result = context->value [id];
	    return TCL_OK;
	}
    }]

    # IV: Generate decoder function: Convert state code into the
    #     corresponding Tcl state string (First mapping wins).

    critcl::ccode \n[critcl::at::here!][string map $map {
	Tcl_Obj*
	@NAME@_decode (Tcl_Interp* interp, int state)
	{
	    char buf [20];
	    int i;
	    @NAME@_iassoc_data context = @NAME@_iassoc (interp);

	    for (i = 0; i < @LAST@; i++)  {
	        if (context->mask[i] != state) continue;
		return context->tcl [i];
	    }

	    sprintf (buf, "%d", state);
	    Tcl_AppendResult (interp, "Invalid @NAME@ state code ", buf, NULL);
	    Tcl_SetErrorCode (interp, "@UNAME@", "STATE", NULL);
	    return NULL;
	}
    }]

    # V. Define convenient argument- and result-type definitions
    #    wrapping the de- and encoder functions for use by cprocs.

    critcl::argtype $name \n[critcl::at::here!][string map $map {
	if (@NAME@_encode (interp, @@, &@A) != TCL_OK) return TCL_ERROR;
    }] int int

    critcl::resulttype $name \n[critcl::at::here!][string map $map {
	/* @NAME@_decode result is 0-refcount */
	Tcl_Obj* ro = @NAME@_decode (interp, rv);
	if (ro == NULL) { return TCL_ERROR; }
	Tcl_SetObjResult (interp, ro);
	return TCL_OK;
    }] int
}

# # ## ### ##### ######## ############# #####################
## Export API

namespace eval ::critcl::emap {
    namespace export def
    catch { namespace ensemble create }
}

namespace eval ::critcl {
    namespace export emap
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## Ready
return
