/*
 * For package "@package@".
 * Implementation of Tcl Interpreter Association "@name@".
 */

#ifndef @stem@_IMPLEMENTATION
#define @stem@_IMPLEMENTATION (1)
/* # # ## ### ##### ######## ############# ##################### */

/*
 * Association structure type, and pointers to it.
 */

typedef struct @type@__ {
@struct@
#line 17 "iassoc.h"
} @type@__;
typedef struct @type@__* @type@;

/*
 * Support functions for structure creation and destruction.
 */

static void
@stem@_Release (@type@ data, Tcl_Interp* interp)
{
@destructor@
#line 29 "iassoc.h"
    ckfree((char*) data);
}

static @type@
@stem@_Init (Tcl_Interp* interp@argdecls@)
{
    @type@ data = (@type@) ckalloc (sizeof (@type@__));

@constructor@
#line 39 "iassoc.h"
    return data;

 error:
    ckfree ((char*) data);
    return NULL;
}

/*
 * Structure accessor, automatically creating it if the interpreter does not
 * have it already, setting it up for destruction on interpreter shutdown.
 */

static @type@
@name@ (Tcl_Interp* interp@argdecls@)
{
#define KEY "@label@"

    Tcl_InterpDeleteProc* proc = (Tcl_InterpDeleteProc*) @stem@_Release;
    @type@ data;

    data = Tcl_GetAssocData (interp, KEY, &proc);
    if (data) {
	return data;
    }

    data = @stem@_Init (interp@argnames@);

    if (data) {
	Tcl_SetAssocData (interp, KEY, proc, (ClientData) data);
    }

    return data;
#undef KEY
}

/* # # ## ### ##### ######## ############# ##################### */
#endif /* @stem@_IMPLEMENTATION */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
