#!/usr/bin/env tclsh
# -*- tcl -*-

set selfdir [file dirname [file normalize [info script]]]
set self    [file tail [info script]]

# Perform "compile & run" in the sub-directories.

puts ""
foreach cr [glob -directory $selfdir */$self] {
    puts "$cr _______________________________________________"
    catch {
	eval [list exec 2>@ stderr >@ stdout [info nameofexecutable] $cr]
    }
    puts ""
    puts ""
}
