## -*- tcl -*-
# # ## ### ##### ######## ############# #####################
# Pragmas for MetaData Scanner.
# n/a

# CriTcl Utility Package for Shared Tcl_Obj* literals of a package.
# Based i-assocs.

package provide critcl::literals 1

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl    8.4     ; # Min supported version.
package require critcl 3.1.11  ; # make, include -- dict portability
package require critcl::iassoc

namespace eval ::critcl::literals {}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Embed C Code

proc critcl::literals::def {name dict} {
    # dict: C symbolic name -> string

    set codes [dict keys $dict]

    dict for {sym string} $dict {
	set map [list @SYM@ $sym @STR@ $string]

	append init \n[critcl::at::here!][string map $map {
	    data->literal [@SYM@] = Tcl_NewStringObj ("@STR@", -1);
	    Tcl_IncrRefCount (data->literal [@SYM@]);
	}]
	append final \n[critcl::at::here!][string map $map {
	    Tcl_DecrRefCount (data->literal [@SYM@]);
	}]
    }

    lappend map @NAME@  $name
    lappend map @CODES@ [join $codes {, }]

    # I. Generate a header file for inclusion by other parts of the
    #    package, i.e. csources. Include the header here as well, for
    #    the following blocks of code.
    #
    #    Declarations of an enum of the symbolic names, plus the
    #    accessor function.

    critcl::include [critcl::make ${name}.h \n[critcl::at::here!][string map $map {
	/* Symbolic names for the literals */
	typedef enum @NAME@_names {
	    @CODES@
	    , @NAME@_name_LAST
	} @NAME@_names;

	/* AccessorRetrieval function for the literals */
	extern Tcl_Obj*
	@NAME@ (Tcl_Interp*  interp,
		@NAME@_names literal);
    }]]
    ## future, maybe ? access macros, one per code ?

    # II: Generate the interp association to hold the array of literals.

    critcl::iassoc def ${name}_iassoc {} \n[critcl::at::here!][string map $map {
	/* Array of the string literals, indexed by the symbolic names */
	Tcl_Obj* literal [@NAME@_name_LAST];
    }] $init $final

    # III: Generate the accessor implementation.

    critcl::ccode [critcl::at::here!][string map $map {
	Tcl_Obj*
	@NAME@ (Tcl_Interp*  interp,
		@NAME@_names literal)
	{
	    if ((literal < 0) || (literal >= @NAME@_name_LAST)) {
		Tcl_Panic ("Bad @NAME@ literal");
	    }
	    return @NAME@_iassoc (interp)->literal [literal];
	}
    }]
}

# # ## ### ##### ######## ############# #####################
## Export API

namespace eval ::critcl::literals {
    namespace export def
    catch { namespace ensemble create }
}

namespace eval ::critcl {
    namespace export literals
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## Ready
return
