-module(common).

-include("common.hrl").

-export([main/1,copy_first_time/1,run_port/2,run_port/3]).

-define(Lg(__Fmt,__Args), ?L("~p : "++__Fmt, [?MODULE|__Args])).
-define(Lg(__Fmt), ?Lg(__Fmt,[])).

main(ScriptPath) when is_list(ScriptPath) ->
    io:format(user, "[~p] ~p : executing ~s with verbose ~p~n",
              [?LINE, ?MODULE, escript:script_name(), get(verbose)]),
    try
        C0 = get(config),
        Platform = C0#config.platform,

        RootPath = case lists:reverse(filename:split(filename:absname(ScriptPath))) of
                       [Platform, "erlpkg", "deps"
                        | RootPathPartsRev] ->
                           filename:join(lists:reverse(RootPathPartsRev));
                       _ ->
                           exit({"owner project path not found",
                                  ScriptPath})
                   end,
        Rebar = case os:find_executable("rebar") of
            false ->
                case os:find_executable("rebar", RootPath) of
                    false -> exit("rebar not found");
                    R-> R
                end;
            R -> R
        end,
        SDir = ?FNJ([RootPath, "src"]),
        AppSrcData = case filelib:is_dir(SDir) of
                     false -> get_app_src_from_rebar_conf(RootPath);
                     true -> get_app_src(SDir)
                 end,
        {App, Desc, Version} = app_info_from_app_src(AppSrcData),
        ReleaseTopDir = ?FNJ([RootPath, "rel", "erlpkg_release"]),
        case filelib:is_dir(ReleaseTopDir) of
            false ->
                ok = file:make_dir(ReleaseTopDir),
                ?Lg("Created ~s", [ReleaseTopDir]);
            _ -> ok
        end,
        BuildPath = ?FNJ([ReleaseTopDir,"build"]),
        case filelib:is_dir(BuildPath) of
            false ->
                ok = file:make_dir(BuildPath),
                ?Lg("Created ~s", [BuildPath]);
            _ -> ok
        end,
        AppStr = atom_to_list(App),
        Conf = C0#config{app = AppStr, desc = Desc, rebar = Rebar,
                         topDir = RootPath, version = Version,
                         tmpSrcDir = ?FNJ([ReleaseTopDir, AppStr++"-"++Version]),
                         buildPath = BuildPath},

        put(config, Conf),

        % Copying application specific files
        copy_first_time("erlpkg.conf"),
        {ok, Config} = file:consult(?FNJ([RootPath,"rel","files","erlpkg.conf"])),
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
        put(config, Conf#config{pkgName = PkgName, pkgCompany = PkgCompany,
                                pkgComment = PkgComment, privFolders = PrivFolders})
    catch _:Error ->
              ?Lg("ERROR ~p~n~p", [Error, erlang:get_stacktrace()])
    end. 

copy_first_time(File) ->
    C = get(config),
    case filelib:is_file(?FNJ([C#config.topDir,"rel","files",File])) of
        true ->
            ?Lg("override for ~s found in rel/files", [File]);
        false ->
            Src = ?FNJ([C#config.topDir,"deps","erlpkg",C#config.platform,File]),
            Dst = ?FNJ([C#config.topDir,"rel","files",File]),
            ?Lg("checking ~s", [Dst]),
            case filelib:ensure_dir(Dst) of
                ok -> ok;
                {error, Error1} ->
                    exit({"failed to create path for "++Dst, Error1})
            end,
            case file:copy(Src, Dst) of
                {error, Error} ->
                    exit({"failed to copy "++File, Error});
                {ok, _BytesCopied} ->
                   ?Lg("copied ~s to rel/files", [File])
            end
    end.

get_app_src_from_rebar_conf(RootPath) ->
    case catch file:consult(?FNJ([RootPath, "rebar.config"])) of
        {'EXIT', Error} ->
            exit({"rebar.config not found", Error});
        {ok, RebarConf} ->
            case proplists:get_value(sub_dirs, RebarConf) of
                undefined ->
                    exit("sub_dirs not defined in rebar.config");
                SubDirs ->
                    case lists:foldl(
                           fun(Dir, undefined) ->
                                   case catch get_app_src(
                                                ?FNJ([RootPath,Dir,"src"])) of
                                       {'EXIT', _} -> undefined;
                                       AppSrc -> AppSrc
                                   end;
                              (_Dir, AppSrc) -> AppSrc
                           end, undefined, SubDirs) of
                        undefined ->
                            exit({"src dir not found", SubDirs});
                        AppSrcFileData -> AppSrcFileData
                    end
            end
    end.

get_app_src(SDir) ->
    case filelib:wildcard("*.app.src", SDir) of
        [AppSrcFile] ->
            case catch file:consult(?FNJ([SDir, AppSrcFile])) of
                {'EXIT', Error} -> exit({"bad file", AppSrcFile, Error});
                {ok, AppSrcFileData} -> AppSrcFileData
            end;
        Else -> exit({"unable to proceed with following app.src info", Else})
    end.

app_info_from_app_src(AppSrcData) ->
    case lists:keyfind(application, 1, AppSrcData) of
        false -> exit({"malformed", AppSrcData});
        {application, AppName, AppConfig} ->
            Desc = case proplists:get_value(description, AppConfig) of
                       undefined -> "";
                       D -> D
                   end,
            Version = case proplists:get_value(vsn, AppConfig) of
                       undefined -> exit("version not defined");
                       V -> V
                   end,
            {AppName, Desc, Version}
    end.

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
