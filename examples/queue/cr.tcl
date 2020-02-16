#!/usr/bin/env tclsh
# -*- tcl -*-
# Run the example via mode "compile & run".
# Note: generic code, same in all examples.

cd [file dirname [file normalize [info script]]]
source ../../lib/critcl/critcl.tcl
source ../../lib/critcl-class/class.tcl

puts v=[set v [package present critcl]]
puts [package ifneeded critcl $v]

puts v=[set vc [package present critcl::class]]
puts [package ifneeded critcl::class $vc]

# Show the config
puts ""
puts "target-config:   [critcl::targetconfig]"
puts "target-platform: [critcl::targetplatform]"
puts "target-actual:   [critcl::actualtarget]"
puts "build-platform:  [critcl::buildplatform]"
puts "cache:           [critcl::cache]"
puts ""

# Pull the package, ignoring build and examples ...
foreach f [glob *.tcl] {
    if {[string match build* $f]} continue
    if {[string match cr* $f]} continue
    if {[string match example* $f]} continue

    puts "Reading $f ..."
    source $f
}

proc ex {args} {
    set code [catch {uplevel 1 $args} result]
    set code [string map {0 ok 1 error 2 break 3 continue} $code]
    set max  [expr {80 - [string length $args] - [string length "Example: "]}]
    puts "Example: $args [string repeat _ $max]"
    puts "Code:    (($code))"
    puts "Result:  (($result))"
    puts ""
    return
}

# ... and run the examples.
foreach f [glob -nocomplain example*] {
    puts "Running $f ..."
    source $f
}

exit
