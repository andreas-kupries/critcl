
#ifdef __cplusplus
extern "C" {
#endif
      ${ext}
DLLEXPORT int
${ininame}_Init(Tcl_Interp *ip)
{
#if USE_TCL_STUBS
  if (!MyInitTclStubs(ip)) return TCL_ERROR;
#endif
