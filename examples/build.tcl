#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}
set selfdir [file dirname [file normalize [info script]]]
set self    [file tail [info script]]

# Run the build code in the sub-directories, passing any arguments.

puts ""
foreach b [glob -directory $selfdir */$self] {
    puts "$b _______________________________________________"
    eval [linsert $argv 0 exec 2>@ stderr >@ stdout [info nameofexecutable] $b]
    puts ""
    puts ""
}
