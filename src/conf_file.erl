-module(conf_file).

-export([parse_vmargs/1, parse_config/1, map/2, parse_release/1]).

parse_vmargs(CmdArgsFile) ->
    {ok, Bin} = file:read_file(CmdArgsFile),
    maps:from_list(lists:filtermap(      
      fun(<<>>) -> false;
         (B) ->
              case re:run(B, "^[ ]*#.*") of
                  nomatch ->
                      case re:run(B, "^([^ ]*) ([^ ]*$)",
                                  [{capture, [1,2], list}]) of
                          {match, [K, V]} -> {true, {K, V}};
                          _ -> false
                      end;
                  _ -> false
              end
      end, re:split(Bin, "[\r\n]", [{return, binary}]))).

parse_config(ErlConfFile) ->
    {ok, ConfTerm} = file:consult(ErlConfFile),
    lists:flatten(ConfTerm).

parse_release(RelAppDir) ->
    {ok,[[{release,_,Release,_,_,permanent}]]}
    = file:consult(filename:join(RelAppDir, "releases/RELEASES")),
    Release.

map(ConfigMap, CtxMap) ->
    maps:map(
      fun(_K, {Cnf,Path}) ->
              Conf = maps:get(Cnf, CtxMap),
              lists:foldl(
                fun({DepApp, Var}, V) ->
                        lists:flatten(
                          [V, case proplists:get_value(
                                     Var, proplists:get_value(DepApp, Conf)) of
                                  Part when is_list(Part) ->
                                      case io_lib:printable_list(Part) of
                                          true -> io_lib:format("~s", [Part]);
                                          _ -> io_lib:format("~p", [Part])
                                      end;
                                  Other -> io_lib:format("~p", [Other])
                              end]);
                   ({ConfProp}, V) ->
                        case Conf of
                            #{ConfProp := Part} when is_list(Part) ->
                                V ++ Part;
                            #{ConfProp := Other}  ->
                                lists:flatten([V, io_lib:format("~p",
                                                                [Other])])
                        end;
                   (Part, V) -> V ++ Part
                end, "", Path)
      end, ConfigMap).
