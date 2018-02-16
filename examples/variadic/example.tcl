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
    ex variadic  {*}$a
    ex ovariadic {*}$a
}
