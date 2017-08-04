/*
 * = = == === ===== ======== ============= =====================
 */

#include <critcl_alloc.h>
#include <string.h>

/*
 * = = == === ===== ======== ============= =====================
 */

#ifdef CRITCL_TRACER

/* Tracking the stack of functions,
 * single-linked list,
 * top to bottom.
 */

typedef struct function_stack {
    const char*            str;
    struct function_stack* down;
} function_stack;

/*
 * = = == === ===== ======== ============= =====================
 * Tracing state
 */

static function_stack* top   = 0;
static int             level = 0;

/*
 * = = == === ===== ======== ============= =====================
 * Internal - Function stack
 */

static void
push (const char* str)
{
    function_stack* new = ALLOC (function_stack);
    new->str = str;
    new->down = top;
    top = new;
    level += 4;
}

static void
pop (void)
{
    function_stack* next = top->down;
    level -= 4;
    ckfree ((char*)top);
    top = next;
}

static void
print_prefix (void)
{
    int i;
    for (i = 0; i < level; i++) {
	fwrite(" ", 1, 1, stdout);
	fflush           (stdout);
    }

    if (top) {
	fwrite(top->str, 1, strlen(top->str), stdout);
	fflush                               (stdout);
    }

    fwrite(" ", 1, 1, stdout);
    fflush           (stdout);
}

/*
 * = = == === ===== ======== ============= =====================
 * API
 */

void
critcl_trace_enter (int on, const char* fun)
{
    if (!on) return;
    push (fun);
    print_prefix();
    fwrite("ENTER\n", 1, 6, stdout);
    fflush                 (stdout);
}

/*
 * 1MB output-buffer. We may trace large data structures. This is also a
 * reason why the implementation can be compiled out entirely.
 */
static char msg [1024*1024];

void
critcl_trace_return (int on, const char *pat, ...)
{
    int len;
    va_list args;

    if (!on) return;
    print_prefix();
    fwrite("RETURN = ", 1, 9, stdout);
    fflush                   (stdout);

    va_start(args, pat);
    len = vsprintf(msg, pat, args);
    va_end(args);

    msg[len++] = '\n';
    msg[len] = '\0';

    fwrite(msg, 1, len, stdout);
    fflush             (stdout);

    pop();
}

void
critcl_trace_printf (int on, int indent, const char *pat, ...)
{
    int len;
    va_list args;

    if (!on) return;
    if (indent) print_prefix();

    va_start(args, pat);
    len = vsprintf(msg, pat, args);
    va_end(args);

    msg[len++] = '\n';
    msg[len] = '\0';

    fwrite(msg, 1, len, stdout);
    fflush             (stdout);
}

void
critcl_trace_cmd_args (int argc, Tcl_Obj*const* argv)
{
    int i;
    for (i=0; i < argc; i++) {
	critcl_trace_printf (1, 1, "ARG [%3d] = '%s'", i, Tcl_GetString((Tcl_Obj*) argv[i]));
    }
}

void
critcl_trace_cmd_result (const Tcl_Obj* result)
{
    critcl_trace_printf (1, 1, "RESULT = '%s'", Tcl_GetString((Tcl_Obj*) result));
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
