# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of a log file.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require critcl::cache  ;# Access to result cache.
package require critcl::common ;# General utilities.
package require debug          ;# debug narrative

package provide critcl::log 4

namespace eval ::critcl::log {
    namespace export begin line text done fd
    namespace ensemble create

    namespace import ::critcl::cache
    namespace import ::critcl::common
}

debug level  critcl/log
debug prefix critcl/log {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

## - start the logging
## - log a full line, ending any previous string
## - log a string, without ending a line
## - stop logging and save to a session log.

proc ::critcl::log::start {cookie file} {
    debug.critcl/log {}

    variable session $cookie
    variable path    [cache get [pid].log]
    variable fd      [open $path $w]

    puts $fd "\n[common now] - $file"
    return
}

proc ::critcl::log::fd {} {
    variable fd
    debug.critcl/log {==> ($fd)}
    return  $fd
}

proc ::critcl::log::text {text} {
    debug.critcl/log {}
    variable fd
    puts -newline $fd $text
    return
}

proc ::critcl::log::line {text} {
    debug.critcl/log {}
    variable fd
    puts $fd $text
    return
}

proc ::critcl::log::done {} {
    debug.critcl/log {}
    variable session
    variable path
    variable fd

    close $fd
    cache append $session.log [set msgs [common cat $path]]
    file delete -force $path

    unset session path fd

    debug.critcl/log {/done}
    return $msgs
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::log {
    variable path    ;# path of the current log file.
    variable fd      ;# channel handle of the current log.
    variable session ;# session handle
}

# # ## ### ##### ######## ############# #####################
## Ready
return
