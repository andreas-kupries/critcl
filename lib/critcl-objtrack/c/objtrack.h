#ifndef CRITCL_OBJTRACK_H
#define CRITCL_OBJTRACK_H
/*
 * critcl objtrack class - declaration
 */

/* API structures.
 *
 * Note 1: The structure of tracker entries is exposed for use by the API
 * macros (definition of statically allocated trackers).
 *
 * Note 2: This code uses the field `internalRep.twoPtrValue.ptr2` of Tcl_Obj*
 * to associate tracker structures with their controlling Tcl_Obj*. This means
 * that each Tcl_Obj* can only be associated with a single tracker at any
 * point in time. However, it is possible to move a Tcl_Obj* between trackers.
 * IOW remove it from its current tracker first, then enter it into the new
 * tracker.
 */

/*! Type of callback function used to report the Tcl_Obj* of a tracker.
 *
 * @param interp The Tcl interpreter holding the queried tracker.
 * @param obj    A Tcl_Obj* in the scanned set.
 * @param id     A unique id for that object. Valid only for the current scan.
 *
 * @result A refcount-zero Tcl_Obj* holding a string describing the `obj`.
 */

typedef Tcl_Obj* (*critcl_objtrack_scan_result) (Tcl_Interp* ip, Tcl_Obj* obj, int id);

/* API functions. See objtrack.tcl as well, for the stubs definitions.
 * (Not all are exported).
 */

/*! Create a new named tracker in the registry managed by the specified Tcl
 *  interpreter.
 *
 * @param interp Tcl Interpreter holding the registry to modify
 * @param name   Name of the new tracker.
 *
 * @result Nothing
 */

extern void
critcl_objtrack_new (Tcl_Interp*                 interp,
		     const char*                 name,
		     critcl_objtrack_scan_result scan_result);

/*! Return the names of all known trackers.
 *
 * @param interp Tcl Interpreter holding the registry to query.
 *
 * @result A list-typed Tcl_Obj* holding the names of all trackers known to
 *         the registry managed by the specified Tcl interpreter.
 */

extern Tcl_Obj*
critcl_objtrack_names (Tcl_Interp* interp);

/*! Return a report of all Tcl_Obj* known to the named tracker in the registry
 *  managed by the specified Tcl interpreter.
 *
 * @param interp Tcl Interpreter holding the registry to query.
 * @param name   Name of the tracker to query
 *
 * @result A list-typed Tcl_Obj* holding the descriptions of all Tcl_Obj* in
 *         the queried tracker.
 */

extern Tcl_Obj*
critcl_objtrack_report (Tcl_Interp* interp,
			const char* name);

/*! Enter a Tcl_Obj* into a tracker specified by name.
 *
 * @param name   Name of the tracker to modify
 * @param obj   The Tcl_Obj* to add.
 *
 * @result Nothing
 */

extern void
critcl_objtrack_enter (Tcl_Interp* interp,
		       const char* name,
		       Tcl_Obj*    obj);

/*! Remove a Tcl_Obj* from its tracker.
 *
 * @param obj   The Tcl_Obj* to remove.
 *
 * @result Nothing
 */

extern void
critcl_objtrack_remove (Tcl_Obj* obj);

#endif

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
