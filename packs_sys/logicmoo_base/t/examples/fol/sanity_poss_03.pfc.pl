:- include(test_header).

% =================================================================================
% Set our engine up
% =================================================================================

:- expects_dialect(clif).
% deduce instances from usages in args having the effect of deducing human,dwelling,beverage_class are classes
==> feature_setting(make_wff,true).
==> feature_setting(add_admitted_arguments,true).
% set truth maintainance system to remove previous assertions that new assertions disagree with 
==> feature_setting(tms_mode,remove_conflicting).
:- set_prolog_flag(runtime_debug,3). % mention it when we remove previous assertions
:- set_prolog_flag_until_eof(do_renames,mpred_expansion).
%:- set_prolog_flag_until_eof(runtime_speed,0). % but dont gripe about speed
:- kif_compile.

% =================================================================================
% poss / ~poss sanity tests
% =================================================================================

:- mpred_trace_exec.

:- dynamic(a/1).

:- wdmsg("BEGIN TESTS").

:- mpred_test(poss(a(_))).

~a(_).

:- mpred_test(~poss(a(_))).

:- mpred_test(\+ poss(a(b))).

poss(a(b)).

:- mpred_test(poss(a(b))).
:- mpred_test(~ poss(a(_))).



