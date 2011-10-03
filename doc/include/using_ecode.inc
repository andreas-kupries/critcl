[subsection {Blocks of arbitrary C}]

Often just defining Tcl commands in C, as demonstrated in the sections
[sectref {A Simple Procedure}],
[sectref {Handling A Variable Number Of Arguments}], and
[sectref {Data As A Tcl Command}]
is not really enough.

For example we may have several of our new C commands using the same
code over and over, and we wish avoid this duplication. Or we wish to
pull in declarations and definitions from some external library.

[para]

In both cases we require the ability to embed an unstructured block of
C code which can contain whatever we want, defines, functions,
includes, etc. without being directly tied to Tcl commands.

The command [cmd critcl::code] provides us with exactly that.

As our example now an excerpt taken from real code, the top of the
[file sha1c.tcl] critcl file in the [term sha1] module of
[term Tcllib]:

[example {
    critcl::ccode {
        #include "sha1.h"
        #include <stdlib.h>
        #include <assert.h>
        
        static
        Tcl_ObjType sha1_type; /* fast internal access representation */
        
        static void 
        sha1_free_rep(Tcl_Obj* obj)
        {
            SHA1_CTX* mp = (SHA1_CTX*) obj->internalRep.otherValuePtr;
            Tcl_Free(mp);
        }

        ...
    }
}]

We see here the beginning of the C code defining a custom
[term Tcl_ObjType] holding the data of a SHA1 context used during the
incremental calculation of sha1 hashes.
