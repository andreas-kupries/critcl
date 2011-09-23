[subsection {Data As A Tcl Command}]

Here we assume that we have a Tcl procedure which returns a fixed
string. In the final product we are going to C to hide this string
from the casual user.

[example {
    proc somedata {} {
	return {... A large blob of characters ...}
    }
}]

The translation of this is simple and easy:

[example {
    package require critcl

    critcl::cdata somedata {... A large blob of characters ...}
}]

There is nothing really notable here.
