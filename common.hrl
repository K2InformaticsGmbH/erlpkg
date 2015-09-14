-ifndef(_COMMON_H_).
-define(_COMMON_H_, true).

-define(H(__F), integer_to_list(erlang:phash2(__F,16#100000000),32)).

-define(TRACE,  io:format("TRACE ~p~n", [?LINE])).

-record(item, { id
              , type % file | dir | component
              , guid
              , name
              , path
              , file_info
        }).

-define(TS, (fun() ->
                    case catch common:timestamp() of
                        {'EXIT', _} -> "";
                        _TS -> _TS
                    end
             end)()
       ).

-define(E(__Fmt,__Args), io:format("~s[~3..0w] "++__Fmt++"~n",
                                   [?TS,?LINE|__Args])).
-define(E(__Fmt), ?E(__Fmt,[])).

-define(L(__Fmt,__Args),
        (fun() ->
                 case get(verbose) of
                     __V when __V == true; __V == undefined ->
                         io:format("~s[~3..0w] "++__Fmt++"~n", [?TS,?LINE|__Args]),
                         if __V == undefined -> put(verbose, true);
                            true -> ok end;
                     false -> ok
                 end
         end)()).
-define(L(__Fmt), ?L(__Fmt,[])).

-define(OSCMD(__Cmd),
    (fun() ->
        CR = os:cmd(__Cmd),
        CmdResp = re:replace(CR, "[\r\n ]*$", "", [{return, list}]),
        ?L(__Cmd++": ~s", [CmdResp]),
        CmdResp
    end)()
).

-define(FNJ(__Parts), filename:join(__Parts)).
-define(FNJ(__Part1, __Part2), filename:join(__Part1, __Part2)).

-record(config, {platform, app, desc, version, tmpSrcDir, topDir, rebar,
                 buildPath, pkgName, pkgCompany, pkgComment, privFolders,
                 candle, light, upgradeCode, patchCode, tab, stats}).

-endif.
