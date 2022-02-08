
This is the section for developers writing, or wishing to write, a
package embedding C into Tcl via critcl.

[para]

I guess that we are allowed to asssume that you, gentle reader, are
here because you have written some Tcl code which is not fast enough
(any more) and you wish to make it "go faster" by replacing parts (or
all) of it with speedy C.

[para]

Another, and I believe reasonable assumption to make would be that you
have already investigated and ruled out or done things like changes to
data structures and algorithms which reduce O(n*n) work to O (n*log n),
O(n), or even O(1).

Of course, nothing prevents you from forging ahead here even if you
have not done such. Still, even in that case I would recommend that
you consider investigating this line of making your code go faster as
well.

[para]

Now, with these introductory words out of the way, lets jump into the
meat of things.

[include using_eproc.inc]        [comment {%% cproc}]
[include using_eprocstr.inc]     [comment {%% cproc, strings}]
[include using_eproctypes.inc]   [comment {%% cproc types, intro & trivial}]
[include using_eproctypes2.inc]  [comment {%% cproc types, semi-trivial}]
[include using_eproctypes3.inc]  [comment {%% cproc types, support (incl alloc'd)}]
[include using_eproctypes4.inc]  [comment {%% cproc types, results}]
[include using_ecommand.inc]     [comment {%% ccommand}]
[include using_edata.inc]        [comment {%% cdata}]
[include using_ecode.inc]        [comment {%% ccode}]
[include using_econst.inc]       [comment {%% cconst}]
[include using_edefines.inc]     [comment {%% cdefines}]
[include using_eheaders.inc]     [comment {%% cheaders}]
[include using_ecsources.inc]    [comment {%% csources}]
[include using_elibraries.inc]   [comment {%% clibraries, framework}]
[include using_eflags.inc]       [comment {%% cflags, ldflags}]
[include using_etclc.inc]        [comment {%% tsources I}]
[include using_etclfallback.inc] [comment {%% tsources II, compiling, failed}]
[include using_eload.inc]        [comment {%% load}]
[include using_efailed.inc]      [comment {%% failed}]
[include using_etcl.inc]         [comment {%% tcl}]
[include using_etk.inc]          [comment {%% tk}]
[include using_echeck.inc]       [comment {%% check}]
[include using_elicense.inc]     [comment {%% license}]

[comment {
	No examples for: cinit, compiling, compiled, preload, done
	No examples for the supporting generator classes.
}]
