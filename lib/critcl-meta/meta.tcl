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

package require Tcl 8.4            ;# Minimal supported Tcl runtime.
package require dict84             ;# Forward-compatible dict command.
package require critcl::common     ;# General utility commands.

package provide  critcl::meta 1
namespace eval ::critcl::meta {
    namespace export getall get gets \
	require license description summary subject \
	general extend assign clear
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::meta::getall {ref} {
    variable system
    variable user

    if {![dict exists $user $ref]} {
	set result {}
    } else {
	set result [dict get $user $ref]
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
	set result [dict merge $result [dict get $system $ref]]
    }

    return $result


}

proc ::critcl::meta::get {ref key} {
    variable system
    if {[dict exists $system $ref $key]} {
	return [dict get $system $ref $key]
    }

    variable user
    if {[dict exists $user $ref $key]} {
	return [dict get $user $ref $key]
    }

    Error "Unknown meta data key \"$key\"" INVALID $key
}

proc ::critcl::meta::gets {ref key} {
    variable system
    if {[dict exists $system $ref $key]} {
	return [dict get $system $ref $key]
    }

    Error "Unknown system meta data key \"$key\"" INVALID-SYSTEM $key
}

proc ::critcl::meta::require {ref args} {
    extend $ref require $args
}

proc ::critcl::meta::license {ref who args} {
    set who [string trim $who]
    if {$who ne ""} {
	append license "This software is copyrighted by $who.\n"
    }

    append license [common::license-text $args]

    assign $ref license [common::text2words   $license]
    assign $ref author  [common::text2authors $who]
    return
}

proc ::critcl::meta::description {ref text} {
    assign $ref description [common::text2words $text]
}

proc ::critcl::meta::summary {ref text} {
    assign $ref summary [common::text2words $text]
}

proc ::critcl::meta::subject {ref args} {
    extend $ref subject $args
}

proc ::critcl::meta::general {ref key args} {
    # <=> extend (below), for user, instead of system.
    variable user
    dict update user $ref data {
	foreach v $args {
	    dict lappend data $key $v
	}
    }
    return
}

proc ::critcl::meta::extend {ref key words} {
    variable system
    dict update system $ref data {
	foreach word $words {
	    dict lappend data $key $word
	}
    }
    return
}

proc ::critcl::meta::assign {ref key words} {
    variable system
    dict set system $ref $key $words
    return
}

proc ::critcl::meta::clear {ref} {
    variable system
    variable user
    dict unset system $ref
    dict unset user   $ref
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::meta {
    # Per-file (ref) databases of meta data.
    # (1) system information
    # (2) user information.
    # The separation is made to ensure that a user cannot overwrite
    # system information with bogus values of her choice.

    # <ref> -> <key> -> words
    variable system {}
    variable user   {}
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::meta::Error {msg args} {
    set code [linsert $args 0 CRITCL META]
    return -code error -errorcode $code $msg
}

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
