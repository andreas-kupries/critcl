# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of per-file binary tags (present vs. not).
# Examples: initialized, done, failed, etc.
#
# This database takes over the function of a few separate flag
# variables originally maintained in the critcl core. Beyond the more
# readable API this is also extensible, i.e. able to handle any new
# indicator flags which may crop up in the future.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require debug          ;# debug narrative

package provide critcl::tags 4

namespace eval ::critcl::tags {
    namespace export set unset has
    namespace ensemble create
}

debug level  critcl/tags
debug prefix critcl/tags {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

## - Attach tag to a file, with optional value.
## - Remove tag from a file.
## - Check if a tag is attached to a file.
## - Return tag value.

proc ::critcl::tags::set {context tag {value {}}} {
    debug.critcl/tags {}
    variable config

    dict set config $context $tag $value
    return
}

proc ::critcl::tags::unset {context tag} {
    debug.critcl/tags {}
    variable config

    dict unset config $context $tag
    return
}

proc ::critcl::tags::has {context tag} {
    debug.critcl/tags {}
    variable config

    set result [dict exists $config $context $tag]
    debug.critcl/tags {==> ($result)}
    return $result
}

proc ::critcl::tags::get {context tag} {
    debug.critcl/tags {}
    variable config

    set result [dict get $config $context $tag]
    debug.critcl/tags {==> ($result)}
    return $result
}

# NOTE: No "clear" API. The tag database will contain information
# going beyond the basic lifecycle of a .critcl file. Particular
# examples are the flags "done" and "failed" which must be kept even
# after a .critcl file has been fully processed

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::tags {
    # Per-file (context) database of TAGS information.
    # <key> -> <tag> -> ""
    variable config {}
}

# # ## ### ##### ######## ############# #####################
## Ready
return
