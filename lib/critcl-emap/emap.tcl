## -*- tcl -*-
# # ## ### ##### ######## ############# #####################
# Pragmas for MetaData Scanner.
# n/a

# CriTcl Utility Package for emap en- and decoder.
# Based on i-assoc.
#
# Copyright (c) 2014-2015 Andreas Kupries <andreas_kupries@users.sourceforge.net>

package provide critcl::emap 1

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl    8.4     ; # Min supported version.
package require critcl 3.1.11  ; # make, include -- dict portability
package require critcl::iassoc

namespace eval ::critcl::emap {}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Embed C Code

proc critcl::emap::def {name dict args} {
    # dict: Tcl symbolic name -> (C int value (1))
    #
    # (Ad 1) Can be numeric, or symbolic, as long as it is a C int
    #        expression in the end.

    # args = options. Currently only -nocase for case-insensitive strings on encoding.

    set nocase 0
    foreach o $args {
	switch -glob -- $o {
	    -nocase -
	    -nocas -
	    -noca -
	    -noc -
	    -no -
	    -n { set nocase 1 }
	    -* -
	    default {
		return -code error -errorcode {CRITCL EMAP INVALID-OPTION} \
		    "Expected option -nocase, got \"$o\""
	    }
	}
    }

    # For the C level opt array we want the elements sorted alphabetically.
    set symbols [lsort -dict [dict keys $dict]]
    set i 0
    foreach s $symbols {
	set id($s) $i
	incr i
    }
    set last $i

    set allint 1
    set min {}
    set max {}

    dict for {sym value} $dict {
	# Manage a direct mapping table from stati to strings, if we
	# can see the numeric value of all stati.
	if {$allint && [string is integer -strict $value]} {
	    if {($min eq {}) || ($value < $min)} { set min $value }
	    if {($max eq {}) || ($value > $max)} { set max $value }
	    lappend direct($value) $sym
	} else {
	    set allint 0
	}

	if {$nocase} { set sym [string tolower $sym] }
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

    if {$nocase} {
	lappend line ""
	lappend line "/* -nocase :: Due to the duplication the state string is not shared,"
	lappend line " * allowing us to convert in place. As the string may change"
	lappend line " * length (shrinking) we have to reset the length after"
	lappend line " * conversion."
	lappend line " */"
	lappend line "state = Tcl_DuplicateObj (state);"
	lappend line "Tcl_SetObjLength(state, Tcl_UtfToLower (Tcl_GetString (state)));"
	lappend map @NOCASE_BEGIN@ [join $line "\n\t    "]
	lappend map @NOCASE_END@   "Tcl_DecrRefCount (state);\n"
    } else {
	lappend map @NOCASE_BEGIN@ ""
	lappend map @NOCASE_END@   ""
    }

    critcl::ccode \n[critcl::at::here!][string map $map {
	int
	@NAME@_encode (Tcl_Interp* interp,
		       Tcl_Obj*    state,
		       int*        result)
	{
	    @NAME@_iassoc_data context = @NAME@_iassoc (interp);
	    int id, res;
	    @NOCASE_BEGIN@
	    res = Tcl_GetIndexFromObj (interp, state, context->c, "@NAME@", 0, &id);
	    @NOCASE_END@
	    if (res != TCL_OK) {
		Tcl_SetErrorCode (interp, "@UNAME@", "STATE", NULL);
		return TCL_ERROR;
	    }

	    *result = context->value [id];
	    return TCL_OK;
	}
    }]

    # IV: Generate decoder function: Convert state code into the
    #     corresponding Tcl state string (First mapping wins).

    if {$allint &&
	($min ne {}) && ($max ne {}) &&
	(($max-$min) < 50)} {
	# Decoder based on a direct mapping table. We can do this
	# because all the values are pure integers, i.e. we know them
	# in detail, and the table is not too big.

	if {$min == 0} {
	    set offset ""
	} elseif {$min < 0} {
	    set offset "+[expr {0-$min}]"
	} else {
	    # Note: The 0+... ensures that we get a decimal number.
	    set offset "-[expr {0+$min}]"
	}

	set table {}
	set hasholes 0
	set n [string length $max]
	for {set i $min} {$i <= $max} {incr i} {
	    if {[info exists direct($i)]} {
		set sym [lindex $direct($i) 0]
		set code $id($sym)
		lappend table "$code,\t/* [format %${n}d $i] <=> \"$sym\" */"
	    } else {
		lappend table "-1,"
		set hasholes 1
	    }
	}

	lappend map @DIRECT@ "\n\t\t    [join $table "\n\t\t    "]"
	lappend map @SIZE@   [llength $table]
	lappend map @MIN@    $min
	lappend map @MAX@    $max
	lappend map @OFFSET@ $offset
	lappend map @HOLCHK@ [expr {$hasholes
				    ? "if (i < 0) goto error;"
				    : ""}]

	critcl::ccode \n[critcl::at::here!][string map $map {
	    Tcl_Obj*
	    @NAME@_decode (Tcl_Interp* interp, int state)
	    {
		static const direct [@SIZE@] = {@DIRECT@
		};

		char buf [20];
		int i;
		@NAME@_iassoc_data context = @NAME@_iassoc (interp);

		/* Check limits first */
		if (state < @MIN@) goto error;
		if (state > @MAX@) goto error;

		/* Map to string index, check if it was a hole (if necessary) */
		i = direct [state@OFFSET@];
		@HOLCHK@

		/* Return the chosen string */
		return context->tcl [i];

	      error:
		sprintf (buf, "%d", state);
		Tcl_AppendResult (interp, "Invalid @NAME@ state code ", buf, NULL);
		Tcl_SetErrorCode (interp, "@UNAME@", "STATE", NULL);
		return NULL;
	    }
	}]

    } else {
	# Decoder based on linear search. Because we either
	# - see some symbolic values (= do not know actual value)
	# - the direct mapping table would be too large (> 50 entries).

	critcl::ccode \n[critcl::at::here!][string map $map {
	    Tcl_Obj*
	    @NAME@_decode (Tcl_Interp* interp, int state)
	    {
		char buf [20];
		int i;
		@NAME@_iassoc_data context = @NAME@_iassoc (interp);

		for (i = 0; i < @LAST@; i++)  {
		   if (context->value[i] != state) continue;
		   return context->tcl [i];
		}

		sprintf (buf, "%d", state);
		Tcl_AppendResult (interp, "Invalid @NAME@ state code ", buf, NULL);
		Tcl_SetErrorCode (interp, "@UNAME@", "STATE", NULL);
		return NULL;
	    }
	}]
    }

    # V. Define convenient argument- and result-type definitions
    #    wrapping the de- and encoder functions for use by cprocs.

    critcl::argtype $name \n[critcl::at::here!][string map $map {
	if (@NAME@_encode (interp, @@, &@A) != TCL_OK) return TCL_ERROR;
    }] int int

    critcl::resulttype $name \n[critcl::at::here!][string map $map {
	/* @NAME@_decode result is 0-refcount */
	{ Tcl_Obj* ro = @NAME@_decode (interp, rv);
	if (ro == NULL) { return TCL_ERROR; }
	Tcl_SetObjResult (interp, ro);
	return TCL_OK; }
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
