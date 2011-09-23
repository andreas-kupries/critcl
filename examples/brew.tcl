#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}
set selfdir [file dirname [file normalize [info script]]]

# Run the "brew.tcl" files in the sibling sub-directories, passing any
# arguments.

puts ""
foreach b [glob -directory $selfdir */brew.tcl] {
    puts "$b _______________________________________________"
    eval [linsert $argv 0 exec 2>@ stderr >@ stdout [info nameofexecutable] $b]
    puts ""
    puts ""
}
