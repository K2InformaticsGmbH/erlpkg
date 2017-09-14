-ifndef(ERLPKG_HRL).
-define(ERLPKG_HRL, true).

-define(C(_F,_A), rebar_api:console(_F, _A)).
-define(I(_F,_A), rebar_api:info(_F, _A)).
-define(W(_F,_A), rebar_api:warn(_F, _A)).
-define(E(_F,_A), rebar_api:error(_F, _A)).
-define(D(_F,_A), rebar_api:debug("[~p:~p] "_F, [?MODULE, ?LINE | _A])).
-define(ABORT(_F,_A), rebar_api:abort(_F,_A)).

-define(C(_F), ?C(_F,[])).
-define(I(_F), ?I(_F,[])).
-define(W(_F), ?W(_F,[])).
-define(E(_F), ?E(_F,[])).
-define(D(_F), ?D(_F,[])).
-define(ABORT(_F), rebar_api:abort(_F)).

-define(FNJ(__Parts), filename:join(__Parts)).
-define(FNJ(__Part1, __Part2), filename:join(__Part1, __Part2)).

-record(item, {id, type, % file | dir | component
               guid, name, path, file_info}).

-define(H(__F), integer_to_list(erlang:phash2(__F,16#100000000),32)).

-define(OSCMD(__Cmd),
    (fun() ->
        CR = os:cmd(__Cmd),
        CmdResp = re:replace(CR, "[\r\n ]*$", "", [{return, list}]),
        ?D(__Cmd++": ~s", [CmdResp]),
        CmdResp
    end)()
).

-endif. % ERLPKG_HRL
