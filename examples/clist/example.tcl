
package require Tcl 8.6 9
package require lambda

# Force compile and load.
catch {clist::map}

# squares
ex clist map [lambda {x} {
    expr {$x * $x}
}] {0 1 2 3 4 5  6 7 8 9}

# filter out even numbers <=> select odd numbers
ex ::clist::filter [lambda {x} {
    expr {$x % 2}
}] {0 1 2 3 4 5  6 7 8 9}

# sum
ex ::clist foldl [lambda {a x} {
    expr {$a + $x}
}] 0 {0 1 2 3 4 5  6 7 8 9}
