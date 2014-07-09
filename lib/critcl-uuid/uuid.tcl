# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of the uuid for a collection of definition.
# Notes:
# - In mode "pkg" this information is irrelevant.
# - In mode "compile&run" this information is only important if the
#   backend keeps a generated shared library around after the
#   application using it has ended, for future uses.  This applies to
#   backends using an external compiler, to amortize the time spent by
#   it.
#
# Given the above creation of a full MD5-based UUID can be disabled
# by a frontent or backend, forcing the use of a simple, and fast,
# counter.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# Note: The package tracks multiple UUIDs, per file.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4            ;# Minimal supported Tcl runtime.
package require dict84             ;# Forward-compatible dict command.

package provide  critcl::uuid 1
namespace eval ::critcl::uuid {
    namespace export get add serial fast clear
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

## - Retrieve the uuid, based on the currently known data.
## - Extend data going into the final uuid.
## - Retrieve a monotonic serial number.
## - Activate fast (counter mode).

proc ::critcl::uuid::get {ref} {
    variable config
    # See also 'add' for the data going into the uuid.

    # NOTE: While it could be useful to cache the result per ref we
    # currently have only one user (critcl::BaseOf) which caches its
    # own results, so doing it here does not gain us anything. Revisit
    # if that changes.

    return [Hash "$ref [Get $ref]"]
}

proc ::critcl::uuid::add {ref key data} {
    variable config
    # Note how the data is reduced to a hash. The final uuid for the
    # ref is generated from the ref this is stored under and the list
    # of keys and hashes we accumulated for it here.

    set digest [Hash /$data]
    dict lappend config $ref $key $digest
    return $digest
}

proc ::critcl::uuid::serial {ref} {
    variable config
    return [llength [Get $ref]]
}

proc ::critcl::uuid::fast {} {
    # Activate fast UUID generation without MD5.
    variable counter 1
    return
}

proc ::critcl::uuid::clear {ref} {
    variable config
    dict unset config $ref
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::uuid {
    # Counter for fast UUID generation without MD5.
    # The initialization value forces the default of a MD5-based UUID.
    variable counter 0

    # Per-file (ref) database of UUID information.
    variable config {}
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::uuid::Get {ref} {
    variable config
    if {[catch {
	dict get $config $ref
    } data]} {
	set data {}
    }
    return $data
}

proc ::critcl::uuid::Hash {data} {
    variable counter
    if {$counter} {
	return [format %032d [incr counter]]
    }
    SetupMD5
    return [MD5 $data]
}

# # ## ### ##### ######## ############# #####################
## Initialization -- MD5.
# Setup is defered to happen only when MD% is actually used.
# This code requires careful attention to handle boot-strapping.

proc ::critcl::uuid::SetupMD5 {} {
    # I. Locate and activate a suitable implementation.
    # (Ia) Future: Have critcl come with its own implementation,
    #      and compile it in a boot-strapping process.
    #
    # II. Fix API differences between the possible implementations
    #     (Returning binary digest versus hex string).


    # md5 could be a cmd or a pkg, or be in a separate namespace
    if {[catch { md5 "" }]} {
	# Do *not* use "package require md5c" since critcl is not loaded
	# yet, but do look for a compiled one, in case object code already
	# exists.

	if {![catch { md5c "" }]} {
	    interp alias {} md5 {} md5c
	} elseif {[catch {package require Trf 2.0}] || [catch {::md5 -- test}]} {
	    # Else try to load the Tcl version in tcllib
	    catch { package require md5 }
	    if {![catch { md5::md5 "" }]} {
		interp alias {} md5 {} md5::md5
	    } else {
		# Last resort: package require or source Don Libes'
		# md5pure script

		if {[catch { package require md5pure }]} {
		    if {[file exists md5pure.tcl]} {
			source md5pure.tcl
			interp alias {} md5 {} md5pure::md5
		    } else {
			# XXX: Note the assumption here, that the md5
			# XXX: package is found relative to critcl itself,
			# XXX: in the critcl starkit.

			source [file join [file dirname [info script]] ../md5/md5.tcl]
			interp alias {} md5 {} md5::md5
		    }
		} else {
		    interp alias {} md5 {} md5pure::md5
		}
	    }
	}
    }

    # Some md5 implementations return hex, others binary.
    # We canonicalize the API to hex results.
    if {[string length [md5 ""]] == 32} {
	proc ::critcl::uuid::MD5 {s} {
	    return [md5 $s]
	}
    } else {
	proc ::critcl::uuid::MD5 {s} {
	    binary scan [md5 $s] H* md; return $md
	}
    }

    # Rename to empty. Will be compiled out of bytecode.
    proc ::critcl::uuid::SetupMD5 {args} {}
    return
}

# # ## ### ##### ######## ############# #####################
## Ready
return
