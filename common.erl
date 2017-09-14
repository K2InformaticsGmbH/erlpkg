-module(common).

-include("common.hrl").

-export([main/1,copy_first_time/1,run_port/2,run_port/3,gen_patch_ts/0,
         timestamp/0,print_stats/0]).

-define(Lg(__Fmt, __Args), ?L("~p : "++__Fmt, [?MODULE | __Args])).
-define(Lg(__Str), ?Lg(__Str, [])).

main(ScriptPath) when is_list(ScriptPath) ->
    ?Li("~p : executing ~s with verbose ~p",
        [?MODULE, escript:script_name(), get(verbose)]),
    try
        C0 = get(config),
        Platform = C0#config.platform,

        {ProjectPath, RootPath} =
        case lists:reverse(filename:split(ScriptPath)) of
            [Platform, "erlpkg", "lib" | RootPathPartsRev] ->
                [_, "_build" | ProjectPathPartsRev] = RootPathPartsRev,
                {filename:join(lists:reverse(ProjectPathPartsRev)),
                 filename:join(lists:reverse(RootPathPartsRev))};
            _ -> exit({"owner project path not found", ScriptPath})
        end,
        RelDir = ?FNJ([RootPath, "rel"]),
        case filelib:is_dir(RelDir) of
            false -> exit("no release found, use rebar3 release");
            true -> ok
        end,
        {App, Desc, Version} = app_info(RelDir),
        ReleaseTopDir = ?FNJ([RootPath, "erlpkg_release"]),
        case filelib:is_dir(ReleaseTopDir) of
            false ->
                ok = file:make_dir(ReleaseTopDir),
                ?Lg("Created ~s", [ReleaseTopDir]);
            _ -> ok
        end,
        AppStr = atom_to_list(App),
        AppRelDir = ?FNJ([RelDir, AppStr]),
        case filelib:is_dir(AppRelDir) of
            false -> exit({"rel path not found", AppRelDir});
            _ -> ok
        end,
        AppRelLibDir = ?FNJ([AppRelDir, "lib", AppStr++"-"++Version]),
        case filelib:is_dir(AppRelLibDir) of
            false -> exit({"app rel path not found", AppRelLibDir});
            _ -> ok
        end,
        Conf = C0#config{app = AppStr, desc = Desc, version = Version,
                         topDir = ReleaseTopDir, projDir = ProjectPath,
                         scriptDir = ScriptPath, relDir = AppRelDir,
                         appRelDir = AppRelLibDir},

        put(config, Conf),

        % Copying application specific files
        copy_first_time("erlpkg.conf"),
        {ok, Config} = file:consult(?FNJ([ReleaseTopDir,"erlpkg.conf"])),
        PkgName = case proplists:get_value(name, Config, '$not_found') of
                      '$not_found' -> "Application Name";
                      PName -> PName
                  end,
        PkgCompany = case proplists:get_value(company, Config, '$not_found') of
                         '$not_found' -> "Name of the Company";
                         Comp -> Comp
                     end,
        PkgComment = case proplists:get_value(comment, Config, '$not_found') of
                         '$not_found' -> PkgName++" is a registered trademark of "++PkgCompany;
                         PCmnt -> PCmnt
                     end,
        PrivFolders = case proplists:get_value(privfolders, Config, '$not_found') of
                          '$not_found' -> "*";
                          PrivDirs -> PrivDirs
                      end,
        put(config, Conf#config{pkgName = PkgName, pkgCompany = PkgCompany, stats = #{},
                                pkgComment = PkgComment, privFolders = PrivFolders}),
        ?Lg("main -- SUCCESS")
    catch _:Error ->
              ?Lg("ERROR ~p~n~p", [Error, erlang:get_stacktrace()])
    end.

copy_first_time(File) ->
    C = get(config),
    Dst = ?FNJ([C#config.topDir,File]),
    case filelib:is_file(Dst) of
        true ->
            ?Lg("override for ~s found in ~s", [File, C#config.topDir]);
        false ->
            ?Lg("checking ~s", [Dst]),
            case filelib:ensure_dir(Dst) of
                ok -> ok;
                {error, Error} ->
                    exit({"failed to create path for "++Dst, Error})
            end,
            ProjSrc = ?FNJ([C#config.projDir,"config",File]),
            case filelib:is_file(ProjSrc) of
                true ->
                    ?Lg("override found in ~s", [ProjSrc]),
                    case file:copy(ProjSrc, Dst) of
                        {error, Error1} ->
                            exit({"failed to copy "++File, Error1});
                        {ok, _BytesCopied} ->
                            ?Lg("copied ~s to ~s", [ProjSrc, Dst])
                    end;
                false ->
                    Src = ?FNJ([C#config.scriptDir, File]),
                    case file:copy(Src, Dst) of
                        {error, Error1} ->
                            exit({"failed to copy "++File, Error1});
                        {ok, _BytesCopied} ->
                            ?Lg("copied ~s to ~s", [Src, Dst])
                    end
            end
    end.

app_info(RelDir) ->
    [AppNameStr] = filelib:wildcard("*",RelDir),
    AppName = list_to_atom(AppNameStr),
    RELEASES = ?FNJ([RelDir, AppNameStr, "releases", "RELEASES"]),
    {ok, [[{release,AppNameStr,Version,_,_,_}|_]]} = file:consult(RELEASES),
    APP = ?FNJ([RelDir, AppNameStr, "lib", AppNameStr++"-"++Version,"ebin",
                AppNameStr++".app"]),
    {ok, [{application,AppName,Props}]} = file:consult(APP),
    Desc = proplists:get_value(description, Props),
    {AppName, Desc, Version}.

run_port(Cmd, Args) ->
    log_cmd(Cmd,
            erlang:open_port(
              {spawn_executable,Cmd},
              [{line, 128},{args, Args}, exit_status,
               stderr_to_stdout, {parallelism, true}])).
run_port(Cmd, Args, Cwd) ->
    ?Lg("run_port(~p, ~p, ~p)", [Cmd, Args, Cwd]),
    log_cmd(Cmd,
            erlang:open_port(
              {spawn_executable,Cmd},
              [{cd,Cwd},{line,128},{args,Args},exit_status,stderr_to_stdout,
               {parallelism,true}])).

-define(NL(__Fmt,__Args), io:format(__Fmt, __Args)).
-define(NL(__Fmt), ?NL(__Fmt,[])).
log_cmd(Cmd, Port) when is_port(Port) ->
    receive
        {'EXIT',Port,Reason} -> ?E("~s terminated for ~p", [Cmd, Reason]);
        {Port,closed} -> ?E("~s terminated", [Cmd]);
        {Port,{exit_status,Status}} ->
            ?E("~s exit with status ~p", [Cmd, Status]),
            catch erlang:port_close(Port);
        {Port,{data,{F,Line}}} ->
            ?NL("~s" ++ if F =:= eol -> "~n"; true -> "" end, [Line]),
            log_cmd(Cmd, Port);
        {Port,{data,Data}} ->
            ?NL("~p", [Data]),
            log_cmd(Cmd, Port)
    end.

gen_patch_ts() ->
    C = get(config),
    BeamPath = filename:join(C#config.appRelDir, "ebin"),
    [{{{_,Month,Day},{Hour,_,_}},_}|_]
    = lists:reverse(
        lists:usort(
          [{filelib:last_modified(filename:join(BeamPath, B)),
            B} || B <- filelib:wildcard("*.beam", BeamPath)]
         )),
    put(config,
        C#config{
          patchCode = lists:flatten(
                        io_lib:format(
                          "~2..0B~2..0B~2..0B",
                          [Month,Day,Hour])
                       )}).

print_stats() ->
    C = get(config),
    ?L("--------------------------------------------------------------------------------"),
    ?L("total build time ~s", [ft(maps:get(total, C#config.stats))]),
    ?L("--------------------------------------------------------------------------------"),
    maps:fold(fun(K, V, _) when K /= total -> ?L("~p time ~s", [K, ft(V)]);
                 (_, _, _) -> undefined
              end, undefined, C#config.stats),
    ?L("--------------------------------------------------------------------------------").

ft(T) when T < 1000 -> integer_to_list(T)++"us";
ft(T) when T >= 1000, T < 1000000 -> integer_to_list(T div 1000)++"ms";
ft(T) when T >= 1000000 -> integer_to_list(T div 1000000)++"s".

timestamp() ->
    TS = {_,_,Micro} = os:timestamp(),
    {{Year,Month,Day},{Hour,Minute,Second}}
    = calendar:now_to_universal_time(TS),
    io_lib:format("~2..0w~2..0w~4..0w-~2..0w~2..0w~2..0w.~s",
                  [Day,Month,Year,Hour,Minute,Second,
                   string:left(integer_to_list(Micro), 3, $0)]).
