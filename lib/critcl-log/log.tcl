# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of a log file.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4            ;# Minimal supported Tcl runtime.
package require critcl::cache      ;# Access to result cache.
package require critcl::common     ;# General utilities.

package provide  critcl::log 1
namespace eval ::critcl::log {
    namespace export begin line text done fd
    catch { namespace ensemble create }

    namespace eval cache  { namespace import ::critcl::cache::*  }
    namespace eval common { namespace import ::critcl::common::* }
}

# # ## ### ##### ######## ############# #####################
## API commands.

## - start the logging
## - log a full line, ending any previous string
## - log a string, without ending a line
## - stop logging and save to a session log.

proc ::critcl::log::start {cookie file} {
    variable session $cookie
    variable path    [cache::get [pid].log]
    variable fd      [open $path $w]

    puts $fd "\n[common::now] - $file"
    return
}

proc ::critcl::log::fd {} {
    variable fd
    return  $fd
}

proc ::critcl::log::text {text} {
    variable fd
    puts -newline $fd $text
    return
}

proc ::critcl::log::line {text} {
    variable fd
    puts $fd $text
    return
}

proc ::critcl::log::done {} {
    variable session
    variable path
    variable fd

    close $fd
    cache::append $session.log [set msgs [common::cat $path]]
    file delete -force $path

    unset session path fd
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
## Internal support commands

# -- none --

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
