#!/usr/bin/env tclsh

package require Tcl 8.5
package require optional

foreach a {
    {}
    {6}
    {6 7}
    {6 7 8}
    {6 7 8 9}
    {6 7 8 9 0}
} {
    ex fixed           {*}$a
    ex optional_head   {*}$a
    ex optional_middle {*}$a
    ex optional_tail   {*}$a
}
