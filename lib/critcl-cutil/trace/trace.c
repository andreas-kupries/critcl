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

typedef struct FUNCTION_STACK {
    const char*            str;
    struct FUNCTION_STACK* down;
} FUNCTION_STACK;

static FUNCTION_STACK* top   = 0;
static int             level = 0;

static void
push (const char* str)
{
    FUNCTION_STACK* new = ALLOC (FUNCTION_STACK);
    new->str = str;
    new->down = top;
    top = new;
    level += 4;
}

static void
pop (void)
{
    FUNCTION_STACK* next = top->down;
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
 */

void
critcl_trace_enter (const char* fun)
{
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
critcl_trace_return (const char *pat, ...)
{
    int len;
    va_list args;

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
critcl_trace_printf (const char *pat, ...)
{
    int len;
    va_list args;

    print_prefix();

    va_start(args, pat);
    len = vsprintf(msg, pat, args);
    va_end(args);

    msg[len++] = '\n';
    msg[len] = '\0';

    fwrite(msg, 1, len, stdout);
    fflush             (stdout);
}

void
critcl_trace_printf0 (const char *pat, ...)
{
    int len;
    va_list args;

    /* 0 -- do not use the current indent -- */

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
	critcl_trace_printf ("ARG [%3d] = '%s'", i, Tcl_GetString(argv[i]));
    }
}

void
critcl_trace_cmd_result (const Tcl_Obj* result)
{
    critcl_trace_printf ("RESULT = '%s'", Tcl_GetString(result));
}

#endif
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
