/*
 * = = == === ===== ======== ============= =====================
 */

#include <critcl_alloc.h>
#include <string.h>

/*
 * = = == === ===== ======== ============= =====================
 */

#ifdef CRITCL_TRACER

/* Tracking the stack of scopes,
 * single-linked list,
 * top to bottom.
 */

typedef struct scope_stack {
    const char*         scope;
    struct scope_stack* down;
} scope_stack;

/*
 * = = == === ===== ======== ============= =====================
 * Tracing state (stack of scopes, associated indentation level)
 */

static scope_stack* top   = 0;
static int          level = 0;

/*
 * = = == === ===== ======== ============= =====================
 * Internals
 */

static void
indent (void)
{
    int i;
    for (i = 0; i < level; i++) { fwrite(" ", 1, 1, stdout); }
    fflush (stdout);
}

static void
scope (void)
{
    if (!top) return;
    fwrite (top->scope, 1, strlen(top->scope), stdout);
    fflush (stdout);
}

static void
separator (void)
{
    fwrite(" | ", 1, 3, stdout);
    fflush             (stdout);
}

/*
 * = = == === ===== ======== ============= =====================
 * API
 */

void
critcl_trace_push (const char* scope)
{
    scope_stack* new = ALLOC (scope_stack);
    new->scope = scope;
    new->down  = top;
    top = new;
    level += 4;
}

void
critcl_trace_pop (void)
{
    scope_stack* next = top->down;
    level -= 4;
    ckfree ((char*)top);
    top = next;
}

void
critcl_trace_header (int on, int ind, const char* filename, int line)
{
    if (!on) return;
    // location prefix
    if (filename) {
	fprintf (stdout, "%s:%6d", filename, line);
	fflush  (stdout);
    }
    // indentation, scope, separator
    if (ind) { indent (); }
    scope ();
    separator();
}

void
critcl_trace_closer (int on)
{
    if (!on) return;
    fwrite ("\n", 1, 1, stdout);
    fflush (stdout);
}

void
critcl_trace_printf (int on, const char *format, ...)
{    
    /*
     * 1MB output-buffer. We may trace large data structures. This is also a
     * reason why the implementation can be compiled out entirely.
     */
    static char msg [1024*1024];
    int len;
    va_list args;
    if (!on) return;
    va_start(args, format);
    len = vsprintf(msg, format, args);
    va_end(args);
    fwrite(msg, 1, len, stdout);
    fflush             (stdout);
}

void
critcl_trace_cmd_args (const char* scopename, int argc, Tcl_Obj*const* argv)
{
    int i;
    critcl_trace_push (scopename);
    for (i=0; i < argc; i++) {
	// No location information
	indent();
	scope();
	separator();
	critcl_trace_printf (1, "ARG [%3d] = '%s'\n",
			     i, Tcl_GetString((Tcl_Obj*) argv[i]));
    }
}

int
critcl_trace_cmd_result (int status, Tcl_Interp* ip)
{
    char* result = Tcl_GetString (Tcl_GetObjResult (ip));
    // No location information
    indent();
    scope();
    separator();
    critcl_trace_printf (1, "RESULT = %d '%s'\n", status, result);
    critcl_trace_pop ();
    return status;
}

#endif /*  CRITCL_TRACER */
/*
 * = = == === ===== ======== ============= =====================
 */

/*
 * local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
