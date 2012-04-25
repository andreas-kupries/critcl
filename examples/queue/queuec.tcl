# queuec.tcl --
#
#       Implementation of a queue data structure for Tcl.
#       This code based on critcl v3.1, API compatible to the PTI [x].
#       [x] Pure Tcl Implementation.
#
# Mainly demonstrates the utility package for the creation of classes
# and objects in C, with both claaes and their instances represented
# as Tcl commands. In contrast to the stackc demo this does not use a
# separate data structure package, nor separately written method
# implementations.
#
# Copyright (c) 2012 Andreas Kupries <andreas_kupries@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id: queuec.tcl,v 1.1 2008/06/19 23:03:35 andreas_kupries Exp $

package require Tcl 8.4
package require critcl 3.1

critcl::buildrequirement {
    package require critcl::class ; # DSL, easy spec of Tcl class/object commands.
}

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} {BSD licensed}

critcl::summary {Queue objects for Tcl.}

critcl::description {
    This package implements queue objects
    for Tcl.
}

critcl::subject queue
critcl::subject {data structure}
critcl::subject structure
critcl::subject {abstract data structure}
critcl::subject {generic data structure}

# # ## ### ##### ######## ############# #####################
## Configuration and implementation.

critcl::cheaders util.h

critcl::class::def ::queuec {
    include util.h

    field Tcl_Obj* unget  {List object unget elements}
    field Tcl_Obj* queue  {List object holding the main queue}
    field Tcl_Obj* append {List object holding new elements}
    field int      at     {Index of next element to return from the main queue}

    constructor {
	instance->at     = 0;
	instance->unget  = Tcl_NewListObj (0,NULL);
	instance->queue  = Tcl_NewListObj (0,NULL);
	instance->append = Tcl_NewListObj (0,NULL);

	Tcl_IncrRefCount (instance->unget); 
	Tcl_IncrRefCount (instance->queue); 
	Tcl_IncrRefCount (instance->append);
    }

    destructor {
	Tcl_DecrRefCount (instance->unget);
	Tcl_DecrRefCount (instance->queue);
	Tcl_DecrRefCount (instance->append);
    }

    support {
	static int
	queue_size (@instancetype@ q, int* u, int* r, int* a)
	{
	    int ungetc  = 0;
	    int queuec  = 0;
	    int appendc = 0;

	    Tcl_ListObjLength (NULL, q->unget,  &ungetc);
	    Tcl_ListObjLength (NULL, q->queue,  &queuec);
	    Tcl_ListObjLength (NULL, q->append, &appendc);

	    if (u) *u = ungetc;
	    if (r) *r = queuec;
	    if (a) *a = appendc;

	    return ungetc + queuec + appendc - q->at;
	}

	static void
	queue_shift (@instancetype@ q)
	{
	    int queuec = 0;
	    int appendc = 0;

	    /* The queue is not done yet, no shift */
	    Tcl_ListObjLength (NULL, q->queue, &queuec);
	    if (q->at < queuec) return;

	    /* The queue is done, however there is nothing
	     * to shift into it, so we don't
	     */
	    Tcl_ListObjLength (NULL, q->append, &appendc);
	    if (!appendc) return;

	    q->at = 0;
	    Tcl_DecrRefCount (q->queue);
	    q->queue  = q->append;
	    q->append = Tcl_NewListObj (0,NULL);
	    Tcl_IncrRefCount (q->append);
	}

	int
	queue_peekget (@instancetype@  instance,
		       Tcl_Interp*     interp,
		       int             objc,
		       Tcl_Obj* CONST* objv,
		       int             get)
	{
	    /* Syntax: queue peek|get ?n?
	     *	       [0]  [1]       [2]
	     */

	    int       listc = 0;
	    Tcl_Obj** listv;
	    Tcl_Obj*  r;
	    int       n = 1;
	    int       ungetc;
	    int       queuec;
	    int       appendc;

	    if ((objc != 2) && (objc != 3)) {
		Tcl_WrongNumArgs (interp, 2, objv, "?n?");
		return TCL_ERROR;
	    }

	    if (objc == 3) {
		if (Tcl_GetIntFromObj(interp, objv[2], &n) != TCL_OK) {
		    return TCL_ERROR;
		} else if (n < 1) {
		    Tcl_AppendResult (interp, "invalid item count ",
				      Tcl_GetString (objv[2]),
				      NULL);
		    return TCL_ERROR;
		}
	    }

	    if (n > queue_size(instance, &ungetc, &queuec, &appendc)) {
		Tcl_AppendResult (interp,
				  "insufficient items in queue to fill request",
				  NULL);
		return TCL_ERROR;
	    }

	    /* 1. We have item on the unget stack
	     *    a. Enough to satisfy request.
	     *    b. Not enough.
	     * 2. We have items in the return buffer.
	     *    a. Enough to satisfy request.
	     *    b. Not enough.
	     * 3. We have items in the append buffer.
	     *    a. Enough to satisfy request.
	     *    b. Not enough.
	     *
	     * Case 3. can assume 2b, because an empty return buffer will be filled
	     * from the append buffer before looking at either. Case 3. cannot happen
	     * for n==1, the return buffer will contain at least one element.
	     *
	     * We distinguish between single and multi-element requests.
	     *
	     * XXX AK optimizations - If we can return everything from a single
	     * buffer, be it queue, or append, just return the buffer object, do not
	     * create something new.
	     */

	    if (n == 1) {
		if (ungetc) {
		    /* Pull from unget stack */
		    Tcl_ListObjGetElements (interp, instance->unget, &listc, &listv);
		    r = listv [listc-1];
		    Tcl_SetObjResult (interp, r);
		    if (get) {
			/* XXX AK : Should maintain max size info, and proper index, for discard. */
			Tcl_ListObjReplace (interp, instance->unget, listc-1, 1, 0, NULL);
		    }
		} else {
		    queue_shift (instance);
		    Tcl_ListObjGetElements (interp, instance->queue, &listc, &listv);
		    ASSERT_BOUNDS(instance->at,listc);
		    r = listv [instance->at];
		    Tcl_SetObjResult (interp, r);
		    /*
		     * Note: Doing the SetObj now is important. It increments the
		     * refcount of 'r', allowing it to survive if the 'queue_shift' below
		     * kills the internal list (instance->queue) holding it.
		     */
		    if (get) {
			instance->at ++;
			queue_shift (instance);
		    }
		}
	    } else {
		/*
		 * Allocate buffer for result, then fill it using the various data
		 * sources.
		 */

		int i = 0, j;
		Tcl_Obj** resv = NALLOC(n,Tcl_Obj*);

		if (ungetc) {
		    Tcl_ListObjGetElements (interp, instance->unget, &listc, &listv);
		    /*
		     * Note how we are iterating backward in listv. unget is managed
		     * as a stack, avoiding mem-copy operations and both push and pop.
		     */
		    for (j = listc-1;
			 j >= 0 && i < n;
			 j--, i++) {
				    ASSERT_BOUNDS(i,n);
				    ASSERT_BOUNDS(j,listc);
				    resv[i] = listv[j];
				    Tcl_IncrRefCount (resv[i]);
				}
		    if (get) {
			/* XXX AK : Should maintain max size info, and proper index, for discard. */
			Tcl_ListObjReplace (interp, instance->unget, j, i, 0, NULL);
			/* XXX CHECK index calcs. */
		    }
		}
		if (i < n) {
		    queue_shift (instance);
		    Tcl_ListObjGetElements (interp, instance->queue, &listc, &listv);
		    for (j = instance->at;
			 j < listc && i < n; 
			 j++, i++) {
				    ASSERT_BOUNDS(i,n);
				    ASSERT_BOUNDS(j,listc);
				    resv[i] = listv[j];
				    Tcl_IncrRefCount (resv[i]);
				}

		    if (get) {
			instance->at = j;
			queue_shift (instance);
		    } else if (i < n) {
			/* XX */
			Tcl_ListObjGetElements (interp, instance->append, &listc, &listv);
			for (j = 0;
			     j < listc && i < n; 
			     j++, i++) {
					ASSERT_BOUNDS(i,n);
					ASSERT_BOUNDS(j,listc);
					resv[i] = listv[j];
					Tcl_IncrRefCount (resv[i]);
				    }
		    }
		}

		/*
		 * This can happen if and only if we have to pull data from append,
		 * and get is set. Without get XX would have run and filled the result
		 * to completion.
		 */

		if (i < n) {
		    ASSERT(get,"Impossible 2nd return pull witohut get");
		    queue_shift (instance);
		    Tcl_ListObjGetElements (interp, instance->queue, &listc, &listv);
		    for (j = instance->at;
			 j < listc && i < n; 
			 j++, i++) {
				    ASSERT_BOUNDS(i,n);
				    ASSERT_BOUNDS(j,listc);
				    resv[i] = listv[j];
				    Tcl_IncrRefCount (resv[i]);
				}
		    instance->at = j;
		    queue_shift (instance);
		}

		r = Tcl_NewListObj (n, resv);
		Tcl_SetObjResult (interp, r);

		for (i=0;i<n;i++) {
				   Tcl_DecrRefCount (resv[i]);
			       }
		ckfree((char*)resv);
	    }

	    return TCL_OK;
	}
    }

    mdef clear {
	/* Syntax: queue clear
	 *	       [0]   [1]
	 */

	if (objc != 2) {
	    Tcl_WrongNumArgs (interp, 2, objv, NULL);
	    return TCL_ERROR;
	}

	/*
	 * Delete and recreate the queue memory. A combination of delete/new,
	 * except the main structure is left unchanged
	 */

	Tcl_DecrRefCount (instance->unget);
	Tcl_DecrRefCount (instance->queue);
	Tcl_DecrRefCount (instance->append);

	instance->at     = 0;
	instance->unget  = Tcl_NewListObj (0,NULL);
	instance->queue  = Tcl_NewListObj (0,NULL);
	instance->append = Tcl_NewListObj (0,NULL);

	Tcl_IncrRefCount (instance->unget); 
	Tcl_IncrRefCount (instance->queue); 
	Tcl_IncrRefCount (instance->append);

	return TCL_OK;
    }

    mdef destroy {
	/* Syntax: queue destroy
	*	       [0]   [1]
	*/

	if (objc != 2) {
	    Tcl_WrongNumArgs (interp, 2, objv, NULL);
	    return TCL_ERROR;
	}

	Tcl_DeleteCommandFromToken(interp, instance->cmd);
	return TCL_OK;
    }

    mdef get  as queue_peekget 1
    mdef peek as queue_peekget 0

    mdef put {
	/* Syntax: queue push item...
	*	       [0]   [1]  [2]
	*/

	int i;

	if (objc < 3) {
	    Tcl_WrongNumArgs (interp, 2, objv, "item ?item ...?");
	    return TCL_ERROR;
	}

	for (i = 2; i < objc; i++) {
	    Tcl_ListObjAppendElement (interp, instance->append, objv[i]);
	}

	return TCL_OK;
    }

    mdef size {
	/* Syntax: queue size
	 *	       [0]   [1]
	 */

	if ((objc != 2)) {
	    Tcl_WrongNumArgs (interp, 2, objv, NULL);
	    return TCL_ERROR;
	}

	Tcl_SetObjResult  (interp, Tcl_NewIntObj (queue_size (instance, NULL, NULL, NULL)));
	return TCL_OK;
    }

    mdef unget {
	/* Syntax: queue unget item
	 *	       [0]   [1]   [2]
	 */

	if (objc != 3) {
	    Tcl_WrongNumArgs (interp, 2, objv, "item");
	    return TCL_ERROR;
	}

	if (instance->at == 0) {
	    /* Need the unget stack */
	    Tcl_ListObjAppendElement (interp, instance->unget, objv[2]);
	} else {
	    /*
	     * We have room in the return buffer, so splice directly instead of
	     * using the unget stack.
	     */

	    int queuec = 0;
	    Tcl_ListObjLength (NULL, instance->queue,  &queuec);

	    instance->at --;
	    ASSERT_BOUNDS(instance->at,queuec);
	    Tcl_ListObjReplace (interp, instance->queue, instance->at, 1, 1, &objv[2]);
	}

	return TCL_OK;
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide queuec 1
