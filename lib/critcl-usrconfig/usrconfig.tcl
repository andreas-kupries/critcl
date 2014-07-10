# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of user configuration options.
# Handled on a per-file basis.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4            ;# Minimal supported Tcl runtime.
package require critcl::uuid       ;# Digesting, change detection.

package provide  critcl::usrconfig 1
namespace eval ::critcl::usrconfig {
    namespace export c_define c_set c_query \
        default
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

## - Declare a user option.
## - Set the value for a user option.
## - Query the value of a user option.
## - Determine the default for a type.

proc ::critcl::usrconfig::c_define {ref oname odesc otype {odefault {}}} {
    # The is command ignores the description of the user's flag. This
    # argument is only used by the static code scanner supporting
    # TEA. See ::critcl::scan::userconfig.

    # When declared without a default we determine one of our
    # own. Boolean flags default to true, whereas enum flags, which
    # are the rest, default to their first value.

    if {[llength [info level 0]] < 6} {
	set odefault [default $otype]
    }

    # We check the default against the type too, before saving
    # everything.
    Validate $oname $otype $odefault

    uuid-add $ref .uc-def [list $oname $otype $odefault]

    dict set config $ref $oname type    $otype
    dict set config $ref $oname default $odefault
    return
}

proc ::critcl::usrconfig::c_set {ref oname value} {
    # NOTE: We can set any user flag we choose, even if not declared
    # yet. Validation of the value happens on query, at which time the
    # flag must be declared.
    variable config
    dict set config $ref $oname value $value
    return
}

proc ::critcl::usrconfig::c_query {ref oname} {
    variable config

    # Prefer cached data.
    # This is known as declared, with defaults merged, and validated.
    if {[dict exists $config $ref $oname =]} {
	return [dict get $config $ref $oname =]
    }

    # Reject the use of undeclared user flags.
    if {![dict exists $config $ref $oname type]} {
	return -code error \
	    -errorcode {CRITCL USRCONFIG INVALID FLAG} \
	    "Unknown user flag \"$oname\""
    }

    # Check if a value was supplied by the calling application. If it
    # was not, we fall back to the declared default.

    if {[dict exists $config $ref $oname value]} {
	set value [dict get $config $ref $oname value]
    } else {
	set value [dict get $config $ref $oname default]
    }

    # Check the value against the flag's type.
    Validate $oname \
	[dict get $config $ref $oname type] \
	$value

    # And fill the cache before returning the requested information,
    # for future calls. See beginning of the procedure for where the
    # cache is queried.
    dict set config $ref $oname = $value
    return $value
}

proc ::critcl::usrconfig::clear {ref} {
    variable config
    dict unset config $ref
    return
}

proc ::critcl::usrconfig::default {otype} {
    switch -exact -- $otype {
	bool {
	    return 1
	}
	default {
	    return [lindex $otype 0]
	}
    }
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::usrconfig {
    # Per-file (ref) database of option information.
    # <key> -> <oname> -> "value"   -> 
    #                  -> "type"    ->
    #                  -> "default" ->
    #                  -> "="       ->
    variable config {}

    namespace import ::critcl::uuid::add ; rename add uuid-add
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::usrconfig::Validate {oname otype value} {
    switch -exact -- $otype {
	bool {
	    if {![string is bool -strict $value]} {
		return -code error \
		    -errorcode {CRITCL USRCONFIG INVALID BOOL} \
		    "Expected boolean for user flag \"$oname\", got \"$value\""
	    }
	}
	default {
	    if {[lsearch -exact $otype $value] < 0} {
		set choices [linsert [join $otype {, }] end-1 or]
		return -code error \
		    -errorcode {CRITCL USRCONFIG INVALID ENUM} \
		    "Expected one of $choices for user flag \"$oname\", got \"$value\""
	    }
	}
    }
}

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
