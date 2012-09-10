/*
 * For package "@package@".
 * Implementation of Tcl Class "@class@".
 */

#ifndef @stem@_IMPLEMENTATION
#define @stem@_IMPLEMENTATION (1)
/* # # ## ### ##### ######## ############# ##################### */

@includes@

/*
 * Instance method names and enumeration.
 */

static CONST char* @stem@_methodnames [] = {
@method_names@
    NULL
};

typedef enum @stem@_methods {
@method_enumeration@
} @stem@_methods;

/*
 * Class method names and enumeration.
 */

static CONST char* @stem@_class_methodnames [] = {
    "create",
    "new",@class_method_names@
    NULL
};

typedef enum @stem@_classmethods {
    @stem@_CM_create,
    @stem@_CM_new@class_method_enumeration@
} @stem@_classmethods;

/*
 * Class structures. Instance counter.
 */

typedef struct @classtype@__ {
@ctypedecl@} @classtype@__;
typedef struct @classtype@__* @classtype@;

typedef struct @classtype@_mgr_ {
    const char*   name;                       /* Class name, for debugging */
    long int      counter;                    /* Id generation counter */
    char          buf [sizeof("@class@")+20]; /* Stash for the auto-generated object names. */
    @classtype@__ user;                       /* User-specified class variables */
} @classtype@_mgr_;
typedef struct @classtype@_mgr_* @classtype@_mgr;

/*
 * Instance structure.
 */

@itypedecl@

/* # # ## ### ##### ######## User: General support */
@support@
#line 65 "class.h"
/* # # ## ### ##### ######## */

/*
 * Class support functions.
 */

static void
@stem@_ClassRelease (ClientData cd, Tcl_Interp* interp)
{
    @classtype@_mgr classmgr = (@classtype@_mgr) cd;
    @classtype@     class    = &classmgr->user;
    @classdestructor@
#line 78 "class.h"
    ckfree((char*) cd);
}

static @classtype@_mgr
@stem@_Class (Tcl_Interp* interp)
{
#define KEY "@package@/@class@"

    Tcl_InterpDeleteProc* proc = @stem@_ClassRelease;
    @classtype@_mgr       classmgr;
    @classtype@           class;

    classmgr = Tcl_GetAssocData (interp, KEY, &proc);
    if (classmgr) {
	return classmgr;
    }

    classmgr = (@classtype@_mgr) ckalloc (sizeof (@classtype@_mgr_));
    classmgr->name = "@stem@";
    classmgr->counter = 0;
    class = &classmgr->user;

    @classconstructor@
#line 102 "class.h"

    Tcl_SetAssocData (interp, KEY, proc, (ClientData) classmgr);
    return classmgr;
 error:
    ckfree ((char*) classmgr);
    return NULL;
#undef KEY
}

static CONST char*
@stem@_NewInstanceName (@classtype@_mgr classmgr)
{
    classmgr->counter ++;
    sprintf (classmgr->buf, "@class@%ld", classmgr->counter);
    return classmgr->buf;
}

/* # # ## ### ##### ######## */

static @instancetype@
@stem@_Constructor (Tcl_Interp* interp,
		    @classtype@ class,
		    int            objcskip,
		    int            objc,
		    Tcl_Obj*const* objv)
{
@ivardecl@;
    /* # # ## ### ##### ######## User: Constructor */
    @constructor@
#line 131 "class.h"
    /* # # ## ### ##### ######## */
    return instance;
@ivarerror@;
}

static void
@stem@_PostConstructor (Tcl_Interp* interp,
		        @instancetype@ instance,
		        Tcl_Command cmd,
			Tcl_Obj* fqn)
{
    /* # # ## ### ##### ######## User: Post Constructor */
    @postconstructor@
#line 145 "class.h"
    /* # # ## ### ##### ######## */
}

static void
@stem@_Destructor (ClientData clientData)
{
    @instancetype@ instance = (@instancetype@) clientData;
    /* # # ## ### ##### ######## User: Destructor */
    @destructor@
#line 155 "class.h"
    /* # # ## ### ##### ######## */
@ivarrelease@;
}

/* # # ## ### ##### ######## User: Methods */
@method_implementations@
#line 162 "class.h"
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

    if (objc < 2) {
	Tcl_WrongNumArgs (interp, objc, objv, "option ?arg arg ...?");
	return TCL_ERROR;
    } else if (Tcl_GetIndexFromObj (interp, objv [1],
				    (const char**) @stem@_methodnames,
				    "option", 0, &mcode) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Dispatch to methods. They check the #args in detail before performing
     * the requested functionality
     */

    switch ((@stem@_methods) mcode) {
@method_dispatch@
    }
    /* Not coming to this place */
    return TCL_ERROR;
}

/* # # ## ### ##### ########: Predefined class methods */

static int
@stem@_NewInstance (const char*     name,
		    @classtype@_mgr classmgr,
		    Tcl_Interp*     interp,
		    int             objcskip,
		    int             objc,
		    Tcl_Obj* CONST* objv)
{
    @instancetype@ instance;
    Tcl_Obj*    fqn;
    Tcl_CmdInfo ci;
    Tcl_Command cmd;

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
	Tcl_AppendToObj    (err, "\" already exists, unable to create @class@ instance", -1);

	Tcl_DecrRefCount (fqn);
	Tcl_SetObjResult (interp, err);
	return TCL_ERROR;
    }

    /*
     * Construct instance state, and command.
     */

    instance = @stem@_Constructor (interp, &classmgr->user, objcskip, objc, objv);
    if (!instance) {
	return TCL_ERROR;
    }

    cmd = Tcl_CreateObjCommand (interp, Tcl_GetString (fqn),
				@stem@_InstanceCommand,
				(ClientData) instance,
				@stem@_Destructor);

    @stem@_PostConstructor (interp, instance, cmd, fqn);

    Tcl_SetObjResult (interp, fqn);
    Tcl_DecrRefCount (fqn);
    return TCL_OK;
}

static int
@stem@_CM_createCmd (@classtype@_mgr classmgr,
		     Tcl_Interp*     interp,
		     int             objc,
		     Tcl_Obj* CONST* objv)
{
    /* <class> create <name> ... */
    char* name;

    if (objc < 3) {
	Tcl_WrongNumArgs (interp, 1, objv, "name ?args...?");
	return TCL_ERROR;
    }

    name = Tcl_GetString (objv [2]);

    objc -= 3;
    objv += 3;

    return @stem@_NewInstance (name, classmgr, interp, 3, objc, objv);
}

static int
@stem@_CM_newCmd (@classtype@_mgr classmgr,
		  Tcl_Interp*     interp,
		  int             objc,
		  Tcl_Obj* CONST* objv)
{
    /* <class> new ... */
    const char* name;

    if (objc < 2) {
	Tcl_WrongNumArgs (interp, 1, objv, "?args...?");
	return TCL_ERROR;
    }

    objc -= 2;
    objv += 2;

    name = @stem@_NewInstanceName (classmgr);
    return @stem@_NewInstance (name, classmgr, interp, 2, objc, objv);
}

/* # # ## ### ##### ######## User: Class Methods */
@class_method_implementations@
#line 320 "class.h"
/* # # ## ### ##### ######## */

/*
 * Class command, class method, especially instance construction.
 */

int
@stem@_ClassCommand (ClientData      clientData,
		     Tcl_Interp*     interp,
		     int             objc,
		     Tcl_Obj* CONST* objv)
{
    @classtype@_mgr classmgr;
    @classtype@     class;
    int mcode;

    if (objc < 2) {
	Tcl_WrongNumArgs (interp, 0, objv, "method ?args...?");
	return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj (interp, objv [1],
			     (const char**) @stem@_class_methodnames,
			     "option", 0, &mcode) != TCL_OK) {
	return TCL_ERROR;
    }

    classmgr = @stem@_Class (interp);
    if (!classmgr) {
	return TCL_ERROR;
    }
    class = &classmgr->user;

    /*
     * Dispatch to methods. They check the #args in detail before performing
     * the requested functionality
     */

    switch ((@stem@_classmethods) mcode) {
	case @stem@_CM_create: return @stem@_CM_createCmd (classmgr, interp, objc, objv); break;
	case @stem@_CM_new:    return @stem@_CM_newCmd    (classmgr, interp, objc, objv); break;@class_method_dispatch@
    }
    /* Not coming to this place */
    return TCL_ERROR;
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
