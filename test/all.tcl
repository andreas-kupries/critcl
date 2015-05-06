# -*- tcl -*-
# Run all .test files in this file's directory, i.e.
# the .test siblings of this file.

foreach t [lsort -dict [glob -directory [file dirname [file normalize [info script]]] *.test]] {
    puts ""
    puts "_ _ __ ___ _____ ________ _____________ _____________________ *** [file tail $t] ***"
    if {[catch {
	source $t
    }]} {
	puts $::errorInfo
    }
}

puts ""
puts "_ _ __ ___ _____ ________ _____________ _____________________"
puts ""
