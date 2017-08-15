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

void critcl_trace_enter      (int on, const char* function_name, int nonewline);
void critcl_trace_return     (int on, const char *pat, ...);
void critcl_trace_printf     (int on, int indent, int nl, const char *pat, ...);

void critcl_trace_cmd_args   (int argc, Tcl_Obj*const* argv);
void critcl_trace_cmd_result (const Tcl_Obj* result);

#define CRITCL_TRACE_XSTR(x) #x
#define CRITCL_TRACE_STR(x)  CRITCL_TRACE_XSTR(x)

#define TRACE_ON   TRACE_TAG_ON  (__THIS_FILE)
#define TRACE_OFF  TRACE_TAG_OFF (__THIS_FILE)

#define TRACE_TAG_ON(t)   static int __critcl_tag_ ## t ## _status = 1;
#define TRACE_TAG_OFF(t)  static int __critcl_tag_ ## t ## _status = 0;

#define TRACE_ENTER(fun, format, ...) {					 \
	critcl_trace_enter  (__critcl_tag___THIS_FILE_status, (fun), 1); \
	critcl_trace_printf (__critcl_tag___THIS_FILE_status, 0, 1, (format), __VA_ARGS__); }

#define TRACE_RETURN(format, x)  critcl_trace_return (__critcl_tag___THIS_FILE_status, (format), (x)) ; return (x)
#define TRACE_RETURN_VOID        critcl_trace_return (__critcl_tag___THIS_FILE_status, "%s","(void)") ; return

#define TRACE0(format, ...)  critcl_trace_printf (__critcl_tag___THIS_FILE_status, 0, 1, (format), __VA_ARGS__)
#define TRACE(format, ...)   critcl_trace_printf (__critcl_tag___THIS_FILE_status, 1, 1, (format), __VA_ARGS__)

#define TRACE0C(format, ...)  critcl_trace_printf (__critcl_tag___THIS_FILE_status, 0, 0, (format), __VA_ARGS__)
#define TRACEC(format, ...)   critcl_trace_printf (__critcl_tag___THIS_FILE_status, 1, 0, (format), __VA_ARGS__)

#define TRACE_TAG_ENTER(t, fun, format, ...) {				\
	critcl_trace_enter  (__critcl_tag_ ## t ## _status, (fun), 1);	\
	critcl_trace_printf (__critcl_tag___THIS_FILE_status, 0, 1, (format), __VA_ARGS__); }

#define TRACE_TAG_RETURN(t, format, x)  critcl_trace_return  (__critcl_tag_ ## t ## _status, (format), (x)) ; return (x)
#define TRACE_TAG_RETURN_VOID(t)        critcl_trace_return  (__critcl_tag_ ## t ## _status, "%s", "(void)") ; return

#define TRACE0_TAG(t, format, ...)  critcl_trace_printf (__critcl_tag_ ## t ## _status, 0, 1, (format), __VA_ARGS__)
#define TRACE_TAG(t, format, ...)   critcl_trace_printf (__critcl_tag_ ## t ## _status, 1, 1, (format), __VA_ARGS__)

#define TRACE0C_TAG(t, format, ...)  critcl_trace_printf (__critcl_tag_ ## t ## _status, 0, 0, (format), __VA_ARGS__)
#define TRACEC_TAG(t, format, ...)   critcl_trace_printf (__critcl_tag_ ## t ## _status, 1, 0, (format), __VA_ARGS__)

#define TRACE_CPROC_ARGS(f,c,v) {	\
	critcl_trace_enter (1,f,0);	\
	critcl_trace_cmd_args(c,v); }

/* Note how the expansions above and below are wrapped into {...}
// blocks. This makes them a proper replacement even for return statements
// which were not put into their own block.
*/
#define TRACE_CPROC_RESULT(ip,x) {			\
	critcl_trace_cmd_result(Tcl_GetObjResult(ip));	\
	critcl_trace_return (1, "%d", (x));		\
	return (x);					\
    }

#define TRACE_CPROC_VOID critcl_trace_return  (1, "%s", "(void)") ; return

#else /* !CRITCL_TRACER */

#define TRACE_ON
#define TRACE_OFF
#define TRACE_TAG_ON(t)
#define TRACE_TAG_OFF(t)

#define TRACE_ENTER(fun,format,...)
#define TRACE_RETURN(f,x) return x
#define TRACE_RETURN_VOID  return
#define TRACE0(format,...)
#define TRACE(format,...)
#define TRACE0C(format,...)
#define TRACEC(format,...)

#define TRACE_TAG_ENTER(t,fun,format,...)
#define TRACE_TAG_RETURN(t,f,x) return x
#define TRACE_TAG_RETURN_VOID(t)  return
#define TRACE0_TAG(t,format,...)
#define TRACE_TAG(t,format,...)
#define TRACE0C_TAG(t,format,...)
#define TRACEC_TAG(t,format,...)

#define TRACE_CPROC_ARGS(f,c,v)  /**/
#define TRACE_CPROC_RESULT(ip,x) return x
#define TRACE_CPROC_VOID         return

#endif /* -- CRITCL_TRACER */

#endif /* __CRITCL_UTIL_TRACE_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
