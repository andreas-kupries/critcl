# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of package meta data pulled out of .critcl
# files. Handled on a per-file basis.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require critcl::common ;# General utility commands.
package require debug          ;# debug narrative

package provide critcl::meta 4

namespace eval ::critcl::meta {
    namespace export getall get gets \
	require license description summary subject \
	general extend assign clear
    namespace ensemble create
}

debug level  critcl/meta
debug prefix critcl/meta {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::meta::getall {context} {
    debug.critcl/meta {}
    variable system
    variable user

    if {![dict exists $user $context]} {
	set result {}
    } else {
	set result [dict get $user $context]
    }

    # Merge the package information (= system meta data) with the
    # user's meta data. The system information overrides anything the
    # user may have declared for the reserved keys (name, version,
    # platform, as::author, as::build::date, license, description,
    # summary, require). Note that for the internal bracketing code
    # the system information may not exist, hence the catch. Might be
    # better to indicate the bracket somehow and make it properly
    # conditional.

    #puts %$file

    catch {
	set result [dict merge $result [dict get $system $context]]
    }

    return $result
}

proc ::critcl::meta::get {context key} {
    debug.critcl/meta {}
    variable system
    if {[dict exists $system $context $key]} {
	return [dict get $system $context $key]
    }

    variable user
    if {[dict exists $user $context $key]} {
	return [dict get $user $context $key]
    }

    Error "Unknown meta data key \"$key\"" INVALID $key
}

proc ::critcl::meta::gets {context key} {
    debug.critcl/meta {}
    variable system
    if {[dict exists $system $context $key]} {
	return [dict get $system $context $key]
    }

    Error "Unknown system meta data key \"$key\"" INVALID-SYSTEM $key
}

proc ::critcl::meta::require {context words} {
    debug.critcl/meta {}
    extend $context require $words
    return
}

proc ::critcl::meta::license {context who words} {
    debug.critcl/meta {}
    set who [string trim $who]
    if {$who ne ""} {
	append license "This software is copyrighted by $who.\n"
    }

    append license [common license-text $words]

    assign $context license [common text2words   $license]
    assign $context author  [common text2authors $who]
    return
}

proc ::critcl::meta::description {context text} {
    debug.critcl/meta {}
    assign $context description [common text2words $text]
    return
}

proc ::critcl::meta::summary {context text} {
    debug.critcl/meta {}
    assign $context summary [common text2words $text]
    return
}

proc ::critcl::meta::subject {context words} {
    debug.critcl/meta {}
    extend $context subject $words
    return
}

proc ::critcl::meta::general {context key words} {
    debug.critcl/meta {}
    # <=> extend (below), for user, instead of system.
    variable user
    dict update user $context data {
	foreach v $words {
	    dict lappend data $key $v
	}
    }
    return
}

proc ::critcl::meta::extend {context key words} {
    debug.critcl/meta {}
    variable system
    dict update system $context data {
	foreach word $words {
	    dict lappend data $key $word
	}
    }
    return
}

proc ::critcl::meta::assign {context key words} {
    debug.critcl/meta {}
    variable system
    dict set system $context $key $words
    return
}

proc ::critcl::meta::clear {context} {
    debug.critcl/meta {}
    variable system
    variable user
    dict unset system $context
    dict unset user   $context
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::meta {
    # Per-file (context) databases of meta data.
    # (1) system information
    # (2) user information.
    # The separation is made to ensure that a user cannot overwrite
    # system information with bogus values of her choice.

    # <context> -> <key> -> words
    variable system {}
    variable user   {}

    namespace import ::critcl::common
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::meta::Error {msg args} {
    set code [linsert $args 0 CRITCL META]
    return -code error -errorcode $code $msg
}

# # ## ### ##### ######## ############# #####################
## Ready
return
