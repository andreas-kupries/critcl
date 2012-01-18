/*
 * For package "@package@".
 * Implementation of Tcl Class "@class@".
 */

#ifndef @stem@_IMPLEMENTATION
#define @stem@_IMPLEMENTATION (1)
/* # # ## ### ##### ######## ############# ##################### */

@includes@

/*
 * Class structure. Instance counter.
 */

typedef struct @classtype@ {
    long int counter;
    char buf [50];
} @classtype@;

/*
 * Instance structure.
 */

@itypedecl@

/*
 * Class support functions.
 */

static void
@stem@_ReleaseClass (ClientData cd, Tcl_Interp* interp)
{
    ckfree((char*) cd);
}

static CONST char*
@stem@_NewInstanceName (Tcl_Interp* interp)
{
#define KEY "@package@/@class@"

    Tcl_InterpDeleteProc* proc = @stem@_ReleaseClass;
    @classtype@ * class;

    class = Tcl_GetAssocData (interp, KEY, &proc);

    if (class  == NULL) {
	class = (@classtype@ *) ckalloc (sizeof (@classtype@));
	class->counter = 0;
	Tcl_SetAssocData (interp, KEY, proc, (ClientData) class);
    }
	    
    class->counter ++;
    sprintf (class->buf, "@class@%d", class->counter);
    return class->buf;
#undef KEY
}

/* # # ## ### ##### ######## User: General support */
@support@
/* # # ## ### ##### ######## */

static @instancetype@
@stem@_Constructor (Tcl_Interp* interp)
{
    @ivardecl@;
    /* # # ## ### ##### ######## User: Constructor */
    @constructor@;
    /* # # ## ### ##### ######## */
    return instance;
@ivarerror@;
}

static void
@stem@_PostContructor (@instancetype@ instance,
		       Tcl_Command cmd)
{
    /* # # ## ### ##### ######## User: Post Constructor */
    @postconstructor@;
    /* # # ## ### ##### ######## */
}

static void
@stem@_Destructor (ClientData clientData)
{
    @instancetype@ instance = (@instancetype@) clientData;
    /* # # ## ### ##### ######## User: Destructor */
    @destructor@;
    /* # # ## ### ##### ######## */
    @ivarrelease@;
}

/* # # ## ### ##### ######## User: Methods */
@method_implementations@
/* # # ## ### ##### ######## */

/*
 * Instance command, method dispatch
 */

static int
@stem@_InstanceCommand (ClientData      clientData,
			      Tcl_Interp*     interp,
			      int             objc,
			      Tcl_Obj* CONST* objv)
{
    @instancetype@ instance = (@instancetype@) clientData;
    int mcode;

    static CONST char* methods [] = {
@method_names@
	NULL
    };
    enum methods {
@method_enumeration@
    };

    if (objc < 2) {
	Tcl_WrongNumArgs (interp, objc, objv, "option ?arg arg ...?");
	return TCL_ERROR;
    } else if (Tcl_GetIndexFromObj (interp, objv [1], methods, "option",
				    0, &mcode) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Dispatch to methods. They check the #args in detail before performing
     * the requested functionality
     */

    switch (mcode) {
@method_dispatch@
    }
    /* Not coming to this place */
}

/*
 * Class command, instance construction.
 */

int
@stem@_ClassCommand (ClientData      clientData,
		     Tcl_Interp*     interp,
		     int             objc,
		     Tcl_Obj* CONST* objv)
{
    /* Syntax
     *  - epsilon                         |1
     *  - name                            |2
     *
     * FUTURE: Allow for additional arguments. See (%%) for relevant places.
     */

    CONST char* name;
    @instancetype@ instance;
    Tcl_Obj*    fqn;
    Tcl_CmdInfo ci;
    Tcl_Command cmd;

#define USAGE "?name?"

    /* (%%) */
    if ((objc != 2) && (objc != 1)) {
	Tcl_WrongNumArgs (interp, 1, objv, USAGE);
	return TCL_ERROR;
    }

    /*
     * Extract user specified name, or generate one ourselves.
     */

    if (objc < 2) {
	name = @stem@_NewInstanceName (interp);
    } else {
	name = Tcl_GetString (objv [1]);
    }

    /*
     * Compute the fully qualified command name to use, putting
     * the command into the current namespace if necessary.
     */

    if (!Tcl_StringMatch (name, "::*")) {
	/* Relative name. Prefix with current namespace */

	Tcl_Eval (interp, "namespace current");
	fqn = Tcl_GetObjResult (interp);
	fqn = Tcl_DuplicateObj (fqn);
	Tcl_IncrRefCount (fqn);

	if (!Tcl_StringMatch (Tcl_GetString (fqn), "::")) {
	    Tcl_AppendToObj (fqn, "::", -1);
	}
	Tcl_AppendToObj (fqn, name, -1);
    } else {
	fqn = Tcl_NewStringObj (name, -1);
	Tcl_IncrRefCount (fqn);
    }
    Tcl_ResetResult (interp);

    /*
     * Check if the commands exists already, and bail out if so.
     * We will not overwrite an existing command.
     */

    if (Tcl_GetCommandInfo (interp, Tcl_GetString (fqn), &ci)) {
	Tcl_Obj* err;

	err = Tcl_NewObj ();
	Tcl_AppendToObj    (err, "command \"", -1);
	Tcl_AppendObjToObj (err, fqn);
	Tcl_AppendToObj    (err, "\" already exists, unable to create @class@", -1);

	Tcl_DecrRefCount (fqn);
	Tcl_SetObjResult (interp, err);
	return TCL_ERROR;
    }

    /*
     * Construct instance state, and command.
     */

    instance = @stem@_Constructor (interp);
    if (!instance) {
	return TCL_ERROR;
    }

    cmd = Tcl_CreateObjCommand (interp, Tcl_GetString (fqn),
				@stem@_InstanceCommand,
				(ClientData) instance,
				@stem@_Destructor);

    @stem@_PostContructor (instance, cmd);

    Tcl_SetObjResult (interp, fqn);
    Tcl_DecrRefCount (fqn);
    return TCL_OK;
#undef USAGE
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
