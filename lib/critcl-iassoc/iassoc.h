/*
 * For package "@package@".
 * Implementation of Tcl Interpreter Association "@name@".
 */

#ifndef @stem@_IMPLEMENTATION
#define @stem@_IMPLEMENTATION (1)
/* # # ## ### ##### ######## ############# ##################### */

/*
 * Association structure.
 */

typedef struct @type@__ {
@struct@
typedef struct @type@__* @type@;

/*
 * Support functions.
 */

static void
@stem@_AssocRelease (@type@ data, Tcl_Interp* interp)
{
    { @destructor@ }
    ckfree((char*) data);
}

static @type@
@stem@_AssocInit (Tcl_Interp* interp)
{
    @type@ data = (@type@) ckalloc (sizeof (@type@__));

    { @constructor@ }

    return data;

 error:
    ckfree ((char*) data);
    return NULL;
}

static @type@
@name@ (Tcl_Interp* interp)
{
#define KEY "@label@"

    Tcl_InterpDeleteProc* proc = (Tcl_InterpDeleteProc*) @stem@_AssocRelease;
    @type@ data;

    data = Tcl_GetAssocData (interp, KEY, &proc);
    if (data) {
	return data;
    }

    data = @stem@_AssocInit (interp);

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
