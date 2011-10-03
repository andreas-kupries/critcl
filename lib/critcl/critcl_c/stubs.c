
# line 1 "MyInitTclStubs"

#if USE_TCL_STUBS
  TclStubs *tclStubsPtr;
  TclPlatStubs *tclPlatStubsPtr;
  struct TclIntStubs *tclIntStubsPtr;
  struct TclIntPlatStubs *tclIntPlatStubsPtr;

  static int
  MyInitTclStubs (Tcl_Interp *ip)
  {
    typedef struct {
      char *result;
      Tcl_FreeProc *freeProc;
      int errorLine;
      TclStubs *stubTable;
    } HeadOfInterp;

    HeadOfInterp *hoi = (HeadOfInterp*) ip;

    if (hoi->stubTable == NULL || hoi->stubTable->magic != TCL_STUB_MAGIC) {
      ip->result = "This extension requires stubs-support.";
      ip->freeProc = TCL_STATIC;
      return 0;
    }

    tclStubsPtr = hoi->stubTable;

    if (Tcl_PkgRequire(ip, "Tcl", "$mintcl", 0) == NULL) {
      tclStubsPtr = NULL;
      return 0;
    }

    if (tclStubsPtr->hooks != NULL) {
	tclPlatStubsPtr = tclStubsPtr->hooks->tclPlatStubs;
	tclIntStubsPtr = tclStubsPtr->hooks->tclIntStubs;
	tclIntPlatStubsPtr = tclStubsPtr->hooks->tclIntPlatStubs;
    }

    return 1;
  }
#endif
