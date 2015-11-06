#!/usr/bin/env tclsh

package require Tcl 8.5
package require variadic

foreach a {
    {}
    {6}
    {6 7}
    {6 7 8}
    {6 7 8 9}
    {6 7 8 9 0}
} {
    puts ___($a)_________
    variadic {*}$a

    ovariadic {*}$a
}
