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

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require critcl::cdefs  ;# General collected C definitions.
package require critcl::uuid   ;# Digesting, change detection.
package require debug          ;# debug narrative

package provide critcl::usrconfig 4

namespace eval ::critcl::usrconfig {
    namespace export c_define c_set c_query \
        default
    namespace ensemble create
}

debug level  critcl/usrconfig
debug prefix critcl/usrconfig {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## Link the lifetime of custom configuration flags to the general C
## definitions for a context.

cdefs on-clear ::critcl::usrconfig::clear

# # ## ### ##### ######## ############# #####################
## API commands.

## - Declare a user option.
## - Set the value for a user option.
## - Query the value of a user option.
## - Determine the default for a type.

proc ::critcl::usrconfig::c_define {context oname odesc otype {odefault {}}} {
    debug.critcl/usrconfig {}
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

    cdefs initialize $file
    uuid add $context .uc-def [list $oname $otype $odefault]

    dict set config $context $oname type    $otype
    dict set config $context $oname default $odefault
    return
}

proc ::critcl::usrconfig::c_set {context oname value} {
    debug.critcl/usrconfig {}
    # NOTE: We can set any user flag we choose, even if not declared
    # yet. Validation of the value happens on query, at which time the
    # flag must be declared.
    variable config
    dict set config $context $oname value $value
    return
}

proc ::critcl::usrconfig::c_query {context oname} {
    debug.critcl/usrconfig {}
    variable config

    # Prefer cached data.
    # This is known as declared, with defaults merged, and validated.
    if {[dict exists $config $context $oname =]} {
	return [dict get $config $context $oname =]
    }

    # Reject the use of undeclared user flags.
    if {![dict exists $config $context $oname type]} {
	set choices [linsert [join [lsort -dict [dict keys [dict get $config $context]]] {, }] end-1 or]
	Error "Unknown user flag \"$oname\", expected one of $choices" \
	    INVALID FLAG $oname
    }

    # Check if a value was supplied by the calling application. If it
    # was not, we fall back to the declared default.

    if {[dict exists $config $context $oname value]} {
	set value [dict get $config $context $oname value]
    } else {
	set value [dict get $config $context $oname default]
    }

    # Check the value against the flag's type.
    Validate $oname \
	[dict get $config $context $oname type] \
	$value

    # And fill the cache before returning the requested information,
    # for future calls. See beginning of the procedure for where the
    # cache is queried.
    dict set config $context $oname = $value
    return $value
}

proc ::critcl::usrconfig::clear {context} {
    debug.critcl/usrconfig {}
    variable config
    dict unset config $context
    return
}

proc ::critcl::usrconfig::default {otype} {
    debug.critcl/usrconfig {}
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
    # Per-file (context) database of option information.
    # <key> -> <oname> -> "value"   -> 
    #                  -> "type"    ->
    #                  -> "default" ->
    #                  -> "="       ->
    variable config {}

    namespace import ::critcl::cdefs
    namespace import ::critcl::uuid
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::usrconfig::Validate {oname otype value} {
    debug.critcl/usrconfig {}
    switch -exact -- $otype {
	bool {
	    if {![string is bool -strict $value]} {
		Error "Expected boolean for user flag \"$oname\", got \"$value\"" \
		    INVALID BOOL $value $oname
	    }
	}
	default {
	    if {[lsearch -exact $otype $value] < 0} {
		set choices [linsert [join $otype {, }] end-1 or]
		Error "Expected one of $choices for user flag \"$oname\", got \"$value\"" \
		    INVALID ENUM $value $oname
	    }
	}
    }
}

proc ::critcl::usrconfig::Error {msg args} {
    debug.critcl/usrconfig {}
    set code [linsert $args 0 CRITCL USRCONFIG]
    return -code error -errorcode $code $msg
}

# # ## ### ##### ######## ############# #####################
## Ready
return
