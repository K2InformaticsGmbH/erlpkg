-ifndef(ERLPKG_HRL).
-define(ERLPKG_HRL, true).

-define(C(_F,_A), rebar_api:console(_F, _A)).
-define(I(_F,_A), rebar_api:info(_F, _A)).
-define(W(_F,_A), rebar_api:warn(_F, _A)).
-define(E(_F,_A), rebar_api:error(_F, _A)).
-define(D(_F,_A), rebar_api:debug("[~p:~p] "_F, [?MODULE, ?LINE | _A])).

-define(FNJ(__Parts), filename:join(__Parts)).
-define(FNJ(__Part1, __Part2), filename:join(__Part1, __Part2)).

-endif. % ERLPKG_HRL
