/*
 * Objtrack - Implementation
 */

#include <tcl.h>
#include <objtrack.h>
#include <critcl_trace.h>
#include <critcl_assert.h>
#include <critcl_alloc.h>
/* iassoc */
#include <critcl_objtrack_registry.h>

TRACE_ON;

/*
 * Internal
 */

/*! Pointer to a tracker root block
 */

typedef struct objtrack* objtrack_p;

/*! Tracker root block.
 *
 * Defines a circular double-linked list. Adding and removing elements is
 * trivial without any special cases.
 */

typedef struct objtrack {
    Tcl_Obj*          self;      /*!< Back reference to the Tcl_Obj* this entry is for. */
    unsigned int      id;        /*!< Transient id assigned to entry during scanning. */
    objtrack_p previous;  /*!< Previous entry in the set. */
    objtrack_p next;      /*!< Next     entry in the set. */
} objtrack;

/*
 * API
 */

void
critcl_objtrack_new (Tcl_Interp*                 interp,
		     const char*                 name,
		     critcl_objtrack_scan_result scan_result)
{
    TRACE_FUNC ("((Tcl_Interp*) %p,"
		" (const char*) %p '%s',"
		" (critcl_objtrack_scan_result) %p)",
		interp, name, name, scan_result);
    ASSERT (interp,      "Bad interp");
    ASSERT (name,        "Bad name");
    ASSERT (name[0],     "Name is empty");
    ASSERT (scan_result, "Bad callback");

    int isNew;
    Tcl_HashTable* t = &critcl_objtrack_registry (interp)->trackers;

    ASSERT (!Tcl_FindHashEntry (t, name), "Name is used");

    objtrack_p root = ALLOC (objtrack);
    root->previous = root;
    root->next     = root;
    root->id       = -1;
    root->self     = (Tcl_Obj*) scan_result;

    Tcl_SetHashValue (Tcl_CreateHashEntry (t, name, &isNew), root);

    TRACE_RETURN_VOID;
}

Tcl_Obj*
critcl_objtrack_names (Tcl_Interp* interp)
{
    TRACE_FUNC ("((Tcl_Interp*) %p)", interp);
    ASSERT (interp, "Bad interp");

    Tcl_HashEntry* hPtr;
    Tcl_HashSearch search;
    Tcl_HashTable* t = &critcl_objtrack_registry (interp)->trackers;
    Tcl_Obj*       r = Tcl_NewListObj (0, 0);

    /*
     * Tracker names are NUL-terminated, not counted strings.
     * This code relies on that.
     */

    for (hPtr = Tcl_FirstHashEntry (t, &search);
	 hPtr != NULL;
	 hPtr = Tcl_NextHashEntry (&search)) {
	Tcl_Obj* name = Tcl_NewStringObj (Tcl_GetHashKey (t, hPtr), -1);
        Tcl_ListObjAppendElement (interp, r, name);
    }

    TRACE_RETURN ("(Tcl_Obj*) %p", r);
}

Tcl_Obj*
critcl_objtrack_report (Tcl_Interp* interp,
			const char* name)
{
    TRACE_FUNC ("((Tcl_Interp*) %p,"
		" (const char*) %p '%s')",
		interp, name, name);
    ASSERT (interp, "Bad interp");
    ASSERT (name,   "Bad name");
    ASSERT (name[0],"Empty name");

    Tcl_HashTable* t = &critcl_objtrack_registry (interp)->trackers;
    Tcl_HashEntry* hPtr = Tcl_FindHashEntry (t, name);

    ASSERT (hPtr, "Name not known");

    objtrack_p           root        = (objtrack_p) Tcl_GetHashValue (hPtr);
    critcl_objtrack_scan_result scan_result = (critcl_objtrack_scan_result) root->self;
    Tcl_Obj*                    r           = Tcl_NewListObj (0, 0);
    objtrack_p           cursor      = root->next;

    /* Phase I: Assign a unique id to all entries, i.e. active objects.
     *
     * Note that this id may or may not match the entries' id in a previous
     * scan, or in future scans.
     */
    unsigned id = 0;
    while (cursor != root) {
	TRACE ("MARK %p = %d", cursor, id);
	cursor->id = id ++;
	cursor = cursor->next;
    }

    TRACE ("#objects: %d", id);
    ASSERT (cursor == root, "All around failure")

    /* Phase II: Report all entries in the set.
     */
    cursor = root->next;
    while (cursor != root) {
	Tcl_Obj* e = scan_result (interp, cursor->self, cursor->id);
	Tcl_ListObjAppendElement (interp, r, e);
	cursor = cursor->next;
    }

    TRACE_RETURN ("(Tcl_Obj*) %p", r);
}

void
critcl_objtrack_enter (Tcl_Interp* interp,
		       const char* name,
		       Tcl_Obj*    obj)
{
    TRACE_FUNC ("((Tcl_interp*) %p,"
		" (char*) %p '%s')"
		" (Tcl_Obj*) %p)",
		interp, name, name, obj);
    ASSERT (interp, "Bad interp");
    ASSERT (name,   "Bad name");
    ASSERT (name[0],"Empty name");
    ASSERT (obj,    "Bad object");
    ASSERT (!obj->internalRep.twoPtrValue.ptr2, "Object already tracked");

    Tcl_HashTable* t    = &critcl_objtrack_registry (interp)->trackers;
    Tcl_HashEntry* hPtr = Tcl_FindHashEntry (t, name);
    ASSERT (hPtr, "Name not known");

    objtrack_p root  = (objtrack_p) Tcl_GetHashValue (hPtr);
    objtrack_p track = ALLOC (objtrack);

    track->id              = 0;
    track->self            = obj;
    track->previous        = root;
    track->next            = root->next;

    root->next->previous = track;
    root->next           = track;

    obj->internalRep.twoPtrValue.ptr2 = track;

    TRACE_RETURN_VOID;
}

void
critcl_objtrack_remove (Tcl_Obj* obj)
{
    TRACE_FUNC ("((Tcl_Obj*) %p)", obj);
    ASSERT (obj,                               "Bad object");
    ASSERT (obj->internalRep.twoPtrValue.ptr2, "Object not tracked");

    objtrack_p track = (objtrack_p) obj->internalRep.twoPtrValue.ptr2;

    obj->internalRep.twoPtrValue.ptr2 = NULL;

    track->previous->next = track->next;
    track->next->previous = track->previous;

    track->id             = 0;
    track->self           = NULL;
    track->previous       = NULL;
    track->next           = NULL;

    FREE (track);
    TRACE_RETURN_VOID;
}


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
