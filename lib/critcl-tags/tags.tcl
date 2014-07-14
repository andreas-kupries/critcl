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

package require Tcl 8.4            ;# Minimal supported Tcl runtime.
package require dict84             ;# Forward-compatible dict command.

package provide  critcl::tags 1
namespace eval ::critcl::tags {
    namespace export set unset has
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

## - Attach tag to a file.
## - Remove tag from a file.
## - Check if a tg is attached to a file.

proc ::critcl::tags::set {ref tag {value {}}} {
    variable config
    dict set config $ref $tag ""
    return
}

proc ::critcl::tags::unset {ref tag} {
    variable config
    dict unset config $ref $tag
    return
}

proc ::critcl::tags::has {ref tag} {
    variable config
    return [dict exists $config $ref $tag]
}

proc ::critcl::tags::get {ref tag} {
    variable config
    return [dict get $config $ref $tag]
}

# NOTE: No "clear" API. The tag database will contain information
# going beyond the basic lifecycle of a .critcl file. Particular
# examples are the flags "done" and "failed" which must be kept even
# after a .critcl file has been fully processed

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::tags {
    # Per-file (ref) database of TAGS information.
    # <key> -> <tag> -> ""
    variable config {}
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

# -- none --

# # ## ### ##### ######## ############# #####################
## Initialization -- MD5.
# Setup is defered to happen only when MD% is actually used.
# This code requires careful attention to handle boot-strapping.

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
