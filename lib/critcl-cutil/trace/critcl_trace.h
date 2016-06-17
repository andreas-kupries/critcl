#ifndef __CRITCL_UTIL_TRACE_H
#define __CRITCL_UTIL_TRACE_H 1

/*
 * = = == === ===== ======== ============= =====================
 */

#include <tcl.h>

/*
 * Narrative tracing support, controlled by CRITCL_TRACER
 */

#ifdef CRITCL_TRACER

void critcl_trace_enter      (const char* function_name);
void critcl_trace_return     (const char *pat, ...);
void critcl_trace_printf0    (const char *pat, ...);
void critcl_trace_printf     (const char *pat, ...);
void critcl_trace_cmd_args   (int argc, Tcl_Obj*const* argv);
void critcl_trace_cmd_result (const Tcl_Obj* result);

#define TRACE_ENTER(fun)       critcl_trace_enter (fun)
#define TRACE_RETURN(format,x) critcl_trace_return (format,x) ; return x
#define TRACE_RETURN_VOID      critcl_trace_return ("%s","(void)") ; return
#define TRACE0(x)              critcl_trace_printf0 x
#define TRACE(x)               critcl_trace_printf x

#define TRACE_ARGS(f,c,v) {		\
	critcl_trace_enter (f);		\
	critcl_trace_cmd_args(c,v); }

/* Note how the code is wrapped into a {...} block. This makes it a proper
// replacement even for return statements which were not put into their own
// block.
*/
#define TRACE_RESULT(ip,x)     {		   \
    critcl_trace_cmd_result(Tcl_GetObjResult(ip)); \
    critcl_trace_return ("%d",(x)) ;		   \
    return (x);			   \
    }

#else /* !CRITCL_TRACER */

#define TRACE_ENTER(fun)
#define TRACE_RETURN(f,x) return x
#define TRACE_RETURN_VOID  return
#define TRACE0(x)
#define TRACE(x)
#define TRACE_ARGS(f,c,v)
#define TRACE_RESULT(ip,x) return x

#endif /* -- CRITCL_TRACER */

#endif /* __CRITCL_UTIL_TRACE_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
