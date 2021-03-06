-module(erlpkg).
-behaviour(provider).

-include("erlpkg.hrl").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, erlpkg).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            % How to use the plugin
            {example, "rebar3 erlpkg args"},
            % list of options understood by the plugin
            {opts,
             [{company,     $c, "company",      string, "Name of the company"},
              {upgradecode, $u, "upgradecode",  string, "UUID of the product"},

              {icon,    $i, "icon",     {string, "defaults/app.ico"},     "Application Icon"},
              {banner,  $b, "banner",   {string, "defaults/493x58.jpg"},  "Application Banner"},
              {dialog,  $d, "dialog",   {string, "defaults/493x312.jpg"}, "Application Dialog Skin"},
              {license, $l, "license",  {string, "defaults/License.rtf"}, "Application License"},
              {xdlgs,   $e, "extra dialogs",  undefined, "Application Extra Wix dialogs (optional)"},
              {msi,     $m, "extra confs",  undefined, "Application Extra Wix configs (optional)"}
             ]},
            {short_desc, "MSI and RPM builder"},
            {desc, "Windows MSI and Linux RPM installer packager for erlang"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [OTP_VSN, SYSTEM_ARCH, WORDSIZE] =
    case re:run(rebar_api:get_arch(), "^([0-9\.]+)-(.*)-([0-9]+)$",
                [{capture, [1,2,3], list}]) of
        {match, [O, S, W]} -> [O, S, W];
        Other -> ?ABORT("{~p,~p} rebar_api:get_arch() : ~p",
                        [?MODULE, ?LINE, Other])
    end,
    {ok, RootDir} = file:get_cwd(),
    [AppInfo] = rebar_state:project_apps(State),
    AppName = binary_to_list(rebar_app_info:name(AppInfo)),
    {Profile, Release} = case rebar_state:current_profiles(State) of
        [default, prod] -> {"prod", AppName};
        [default | Profiles] ->
            {string:join([atom_to_list(P) || P <- Profiles], "+"),
             atom_to_list(lists:last(Profiles))};
        Other1 -> ?ABORT("{~p,~p} bad profiles : ~p", [?MODULE, ?LINE, Other1])
    end,
    ReleaseDir = ?FNJ([RootDir, "_build", Profile, "rel"]),
    PkgDir = ?FNJ(ReleaseDir, "erlpkg"),
    ConfDir = ?FNJ(RootDir, "config"),
    Opts = get_opts(ConfDir, State),
    Version = rebar_app_info:original_vsn(AppInfo),
    Description = proplists:get_value(
                    description, rebar_app_info:app_details(AppInfo), ""),
    ReleaseAppDir = ?FNJ(ReleaseDir, Release),
    ?D("ReleaseAppDir ~p", [ReleaseAppDir]),
    {ok, [[{release, Release, ReleaseVsnDir, _, _, _}]]} =
        file:consult(?FNJ([ReleaseAppDir, "releases", "RELEASES"])),
    C0 = Opts#{
        rel => Release, app => AppName, version => Version, desc => Description,
        pkgDir => PkgDir, rootDir => RootDir, otp => OTP_VSN,
        arch => SYSTEM_ARCH, word => WORDSIZE, profile => Profile,
        configDir => ConfDir, relAppDir => ReleaseAppDir,
        relDir => ReleaseVsnDir
    },
    ?D("CONFIG:~n~p", [C0]),
    case SYSTEM_ARCH of
        "win32" ->
            VmArgs = conf_file:parse_vmargs(
                       ?FNJ([ReleaseAppDir,"releases",Version,"vm.args"])),
            SysConfig = conf_file:parse_config(
                          ?FNJ([ReleaseAppDir,"releases",Version,"sys.config"])),
            C1 = patch_timestamp(C0),
            C2 = windows:build(C1#{vmargs => VmArgs, sysconfig => SysConfig}),
            ?I("STATS:~n~p", [maps:get(stats, C2, "no stats")]),
            ok;
        "x86_64-unknown-linux-gnu" ->
            C1 = linux:build(C0),
            ?I("STATS:~n~p", [maps:get(stats, C1, "no stats")]),
            ok;
        SYSTEM_ARCH -> ?ABORT("not supported ~s", [SYSTEM_ARCH])
    end,
    {ok, State}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

patch_timestamp(C) ->
    BeamPath = ?FNJ([maps:get(relAppDir, C), "lib",
                     maps:get(app, C)++"-"++maps:get(version, C),
                     "ebin"]),
    [{{{_,Month,Day},{Hour,_,_}},_}|_]
    = lists:reverse(
        lists:usort(
          [{filelib:last_modified(filename:join(BeamPath, B)),
            B} || B <- filelib:wildcard("*.beam", BeamPath)]
         )),
    PatchCode = lists:flatten(
                  io_lib:format(
                    "~2..0B~2..0B~2..0B", [Month,Day,Hour])),
    C#{patchCode => PatchCode}.

get_opts(ConfDir, State) ->
    {ClOpts,_} = rebar_state:command_parsed_args(State),
    RebarConfigOpts = rebar_state:get(State, erlpkg_opts, []),
    Opts = maps:from_list(
             lists:ukeymerge(1, lists:ukeysort(1, RebarConfigOpts),
                             lists:ukeysort(1, ClOpts))),
    if map_size(Opts) == 0 -> ?ABORT("missing arguments");
       true ->
           [Dir|_] =
           lists:filtermap(
             fun(App) ->
                     case rebar_app_info:name(App) of
                         <<"erlpkg">> -> {true, rebar_app_info:dir(App)};
                         _ -> false
                     end
             end,
             rebar_state:all_plugin_deps(State)),
           maps:fold(
             fun(K,V,M) when K == icon; K == banner; K == dialog;
                             K == license ->
                     FullPath = ?FNJ(ConfDir, V),
                     case filelib:is_regular(FullPath) of
                         true -> M#{K => FullPath};
                         false ->
                             FullDefaultPath = ?FNJ(Dir, V),
                             case filelib:is_regular(FullDefaultPath) of
                                 true -> M#{K => FullDefaultPath};
                                 false ->
                                     ?ABORT("bad config ~p:~p",
                                            [K, FullDefaultPath])
                             end
                     end;
                (xdlgs, Vs, M) when is_list(Vs) ->
                     M#{xdlgs =>
                        lists:foldl(
                          fun(IV, IVs) ->
                                  FullIVPath = ?FNJ(ConfDir, IV),
                                  case filelib:is_regular(FullIVPath) of
                                      true -> [FullIVPath | IVs];
                                      false ->
                                          ?W("[xdlgs] ~s NOT FOUND in ~s",
                                             [IV, ConfDir]),
                                          IVs
                                  end
                          end, [], Vs)};
                (K,V,M) -> M#{K => V}
             end, #{}, Opts)
    end.
