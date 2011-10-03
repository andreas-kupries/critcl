[subsection {Using C with Tcl functionality as fallback}]

There is one special case of
[sectref {Having both C and Tcl functionality}]
which deserves its own section.

[para] The possibility of not having the fast C code on some platform,
and using a slower Tcl implementation of the functionality. In other
words, a fallback which keeps the package working in the face of
failure to build the C parts. A more concrete example of this would be
a module implementing the SHA hash, in both C and Tcl, and using the
latter if and only if the C implementation is not available.

[para] There two major possibilities in handling such a situation.

[list_begin enumerated]

[comment {======================================================}]
[enum] Keep all the pieces separated. In that scenario our concrete
example would be spread over three packages. Two low-level packages
[package sha::c] and [package sha::tcl] containing the two
implementations of the algorithm, and, thirdly, a coordinator package
[package sha] which loads either of them, based on availability.

[para] The Tcllib bundle of packages contains a number of packages
structured in this manner, mostly in the [term struct] module.

[para] Writing the C and Tcl parts should be simple by now, with all
the examples we had so far. The only non-trivial part is the
coordinator, and even that if and only if we wish to make it easy to
write a testsuite which can check both branches, C, and Tcl without
gymnastics. So, the most basic coordinator would be

[example {
    set sha::version 1
    if {[catch {
        package require sha::c $sha::version
    }]} {
        package require sha::tcl $sha::version
    }
    package provide sha $sha::version
}]

It tries to load the C implementation first, and falls back to the Tcl
implementation if that fails. The code as is assumes that both
implementations create exactly the same command names, leaving the
caller unaware of the choice of implementations.

[para] A concrete example of this scheme can be found in Tcllib's
[package md5] package. While it actually uses ythe [package Trf] as
its accelerator, and not a critcl-based package the principle is the
same. It also demonstrates the need for additional glue code when the
C implementation doesn't exactly match the signature and semantics of
the Tcl implementation.

[para] This basic coordinator can be easily extended to try more than
two packages to get the needed implementation. for example, the C
implementation may not just exist in a sha::c package, but also
bundled somewhere else. Tcllib, for example, has a tcllibc package
which bundles all the C parts of its packages which have them in a
single binary.

[para] Another direction to take it in is to write code which allows
the loading of multiple implementations at the same time, and then
switching between them at runtime. Doing this requires effort to keep
the implementations out of each others way, i.e. they cannot provide
the same command names anymore, and a more complex coordinator as
well, which is able to map from the public command names to whatever
is provided by the implementation.

[para] The main benefit of this extension is that it makes testing the
two different implementations easier, simply run through the same set
of tests multiple times, each time with different implementation
active. The disadvantage is the additional complexity of the
coordinator's internals. As a larger example of this technique here is
the coordinator [file modules/struct/queue.tcl] handling the C and Tcl
implementations of Tcllib's [package struct::queue] package:

[example {
    # queue.tcl --
    #       Implementation of a queue data structure for Tcl.

    package require Tcl 8.4
    namespace eval ::struct::queue {}

    ## Management of queue implementations.

    # ::struct::queue::LoadAccelerator --
    #       Loads a named implementation, if possible.

    proc ::struct::queue::LoadAccelerator {key} {
        variable accel
        set r 0
        switch -exact -- $key {
            critcl {
                # Critcl implementation of queue requires Tcl 8.4.
                if {![package vsatisfies [package provide Tcl] 8.4]} {return 0}
                if {[catch {package require tcllibc}]} {return 0}
                set r [llength [info commands ::struct::queue_critcl]]
            }
            tcl {
                variable selfdir
                if {
                    [package vsatisfies [package provide Tcl] 8.5] &&
                    ![catch {package require TclOO}]
                } {
                    source [file join $selfdir queue_oo.tcl]
                } else {
                    source [file join $selfdir queue_tcl.tcl]
                }
                set r 1
            }
            default {
                return -code error "invalid accelerator/impl. package $key:\
                    must be one of [join [KnownImplementations] {, }]"
            }
        }
        set accel($key) $r
        return $r
    }

    # ::struct::queue::SwitchTo --
    #       Activates a loaded named implementation.

    proc ::struct::queue::SwitchTo {key} {
        variable accel
        variable loaded

        if {[string equal $key $loaded]} {
            # No change, nothing to do.
            return
        } elseif {![string equal $key ""]} {
            # Validate the target implementation of the switch.

            if {![info exists accel($key)]} {
                return -code error "Unable to activate unknown implementation \"$key\""
            } elseif {![info exists accel($key)] || !$accel($key)} {
                return -code error "Unable to activate missing implementation \"$key\""
            }
        }

        # Deactivate the previous implementation, if there was any.

        if {![string equal $loaded ""]} {
            rename ::struct::queue ::struct::queue_$loaded
        }

        # Activate the new implementation, if there is any.

        if {![string equal $key ""]} {
            rename ::struct::queue_$key ::struct::queue
        }

        # Remember the active implementation, for deactivation by future
        # switches.

        set loaded $key
        return
    }

    # ::struct::queue::Implementations --
    #       Determines which implementations are
    #       present, i.e. loaded.

    proc ::struct::queue::Implementations {} {
        variable accel
        set res {}
        foreach n [array names accel] {
            if {!$accel($n)} continue
            lappend res $n
        }
        return $res
    }

    # ::struct::queue::KnownImplementations --
    #       Determines which implementations are known
    #       as possible implementations.

    proc ::struct::queue::KnownImplementations {} {
        return {critcl tcl}
    }

    proc ::struct::queue::Names {} {
        return {
            critcl {tcllibc based}
            tcl    {pure Tcl}
        }
    }

    ## Initialization: Data structures.

    namespace eval ::struct::queue {
        variable  selfdir [file dirname [info script]]
        variable  accel
        array set accel   {tcl 0 critcl 0}
        variable  loaded  {}
    }

    ## Initialization: Choose an implementation,
    ## most prefered first. Loads only one of the
    ## possible implementations. And activates it.

    namespace eval ::struct::queue {
        variable e
        foreach e [KnownImplementations] {
            if {[LoadAccelerator $e]} {
                SwitchTo $e
                break
            }
        }
        unset e
    }

    ## Ready

    namespace eval ::struct {
        # Export the constructor command.
        namespace export queue
    }

    package provide struct::queue 1.4.2
}]

In this implementation the coordinator renames the commands of the
low-level packages to the public commands, making the future dispatch
as fast as if the commands had these names anyway, but also forcing a
spike of bytecode recompilation if switching is ever done at the
runtime of an application, and not just used for testing, and possibly
disrupting introspection by the commands, especially if they move
between different namespaces.

[para] A different implementation would be to provide the public
commands as procedures which consult a variable to determine which of
the loaded implementations is active, and then call on its
commands. This doesn't disrupt introspection, nor does it trigger
bytecode recompilation on switching. But it takes more time to
dispatch to the actual implementation, in every call of the public API
for the package in question.

[para] A concrete example of this scheme can be found in Tcllib's
[package crc32] package.

[comment {======================================================}]
[enum] Mix the pieces together. Please note that while I am describing
how to make this work I strongly prefer and recommend to use the
previously shown approach using separate files/packages. It is much
easier to understand and maintain. With this warning done, lets go
into the nuts and bolts.

[para] If we care only about mode "compile & run" things are easy:

[example {
    package require critcl

    if {![critcl::compiling]} {
        proc mycommand {...} {
            ...
        }

    } else {
        critcl::cproc mycommand {...} {
            ...
        }
    }
}]

The command [cmd critcl::compiling] tells us whether we have a
compiler available or not, and in the latter case we implement our
command in Tcl.

[para] Now what happens when we invoke mode "generate package" ?

... compiler failure ...
... ok   - C code - everything fine
... fail - no package ? or just no C code ? declare self as tsource, to be used ?
... platform-specific C/Tcl -- uuid.

[list_end]



