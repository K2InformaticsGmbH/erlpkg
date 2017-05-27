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
              {upgradecode, $u, "upgradecode",  string, "UUID of the product"}
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
    Profile =
    case rebar_state:current_profiles(State) of
        [default, P] -> P;
        Other1 -> ?ABORT("{~p,~p} bad profiles : ~p",
                         [?MODULE, ?LINE, Other1])
    end,
    ReleaseDir = ?FNJ([RootDir, "_build", Profile, "rel"]),
    PkgDir = ?FNJ(ReleaseDir, "erlpkg"),
    {ClOpts,_} = rebar_state:command_parsed_args(State),
    RebarConfigOpts = rebar_state:get(State, erlpkg_opts, []),
    Opts = lists:ukeymerge(1, lists:ukeysort(1, RebarConfigOpts),
                           lists:ukeysort(1, ClOpts)),
    if length(Opts) == 0 -> ?ABORT("missing arguments");
       true -> ok end,
    [AppInfo] = rebar_state:project_apps(State),
    AppName = binary_to_list(rebar_app_info:name(AppInfo)),
    Version = rebar_app_info:original_vsn(AppInfo),
    Description = proplists:get_value(
                    description, rebar_app_info:app_details(AppInfo), ""),
    ReleaseAppDir = ?FNJ(ReleaseDir, AppName),
    ConfDir = ?FNJ(RootDir, "config"),
    C0 = (maps:from_list(Opts))
    #{app => AppName, version => Version, desc => Description,
      topDir => RootDir, pkgDir => PkgDir, rootDir => RootDir, otp => OTP_VSN,
      arch => SYSTEM_ARCH, word => WORDSIZE, profile => Profile,
      configDir => ConfDir, relAppDir => ReleaseAppDir},

    C1 = patch_timestamp(C0),
    ?D("CONFIG:~n~p", [C1]),
    case SYSTEM_ARCH of
        "win32" ->
            C2 = windows:build(C1),
            ?D("CONFIG:~n~p", [C2]),
            ok;
            %?C("rebar_api:console()", []),
            %?I("rebar_api:info()", []),
            %?W("rebar_api:warn()", []),
            %?E("rebar_api:error()", []);
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
