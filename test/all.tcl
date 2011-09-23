# -*- tcl -*-
# Run all .test files in this file's directory, i.e.
# the .test siblings of this file.

foreach t [glob -directory [file dirname [file normalize [info script]]] \
	       *.test] {
    source $t
}
