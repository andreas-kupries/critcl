# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of the "current" critcl file, i.e. the file
# which is currently processed and accumulating definitions. The
# exposed API allows the core to tinker with the information, enabling
# - redirection of definition to a virtual file.
# - inclusion of files and associating their definition with the includer.
# - ...

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4        ;# Minimal supported Tcl runtime.
package require critcl::data 1 ;# Access to data files (license).

package provide  critcl::common 1
namespace eval ::critcl::common {
    namespace export cat write append \
	text2words text2authors license-text \
	today
    catch { namespace ensemble create }

    namespace import ::critcl::data::file
    rename file datafile
}

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::common::cat {path} {
    # Easier to write our own copy than requiring fileutil and then
    # using fileutil::cat.

    set fd [open $path r]
    set data [read $fd]
    close $fd
    return $data
}

proc ::critcl::common::write {dst contents} {
    file mkdir [file dirname $dst]
    set    chan [open $dst w]
    puts  $chan $contents
    close $chan
    return
}

proc ::critcl::common::append {dst contents} {
    file mkdir [file dirname $dst]
    set    chan [open $dst a]
    puts  $chan $contents
    close $chan
}

proc ::critcl::common::text2words {text} {
    regsub -all {[ \t\n]+} $text { } text
    return [split [string trim $text]]
}

proc ::critcl::common::text2authors {text} {
    regsub -all {[ \t\n]+} $text { } text
    set authors {}
    foreach a [split [string trim $text] ,] {
	lappend authors [string trim $a]
    }
    return $authors
}

proc ::critcl::common::license-text {words} {
    if {[llength $words]} {
	# Use the supplied license details as our suffix.
	return [join $words]
    } else {
	# As no details were supplied we fall back to the critcl
	# license as template for the license of the generated
	# package. Note how we strip the first 2 lines from the
	# file. This removes the author information for critcl itself,
	# allowing us to replace it by the user-supplied author.

	return [join [lrange [split [cat [datafile license.terms]] \
				  \n] \
			  2 end] \
		    \n]
    }
}

proc ::critcl::common::today {} {
    return [clock format [clock seconds] -format {%Y-%m-%d}]
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::common {}

# # ## ### ##### ######## ############# #####################
## Internal support commands

# -- none --

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
