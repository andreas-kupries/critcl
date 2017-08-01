#!/usr/bin/env tclsh
# -*- tcl -*-

set selfdir [file dirname [file normalize [info script]]]
set self    [file tail [info script]]

set pattern [lindex $argv 0]
if {$pattern eq ""} {
    set pattern *
}
# Perform "compile & run" in the sub-directories.

puts ""
foreach cr [lsort -dict [glob -directory $selfdir $pattern/$self]] {
    puts "$cr _______________________________________________"
    catch {
	eval [list exec 2>@ stderr >@ stdout [info nameofexecutable] $cr]
    }
    puts ""
    puts ""
}
