-module(erlpkg_prv).
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
        Other -> rebar_api:abort("{~p,~p} rebar_api:get_arch() : ~p",
                                 [?MODULE, ?LINE, Other])
    end,
    case SYSTEM_ARCH of
        "win32" ->
            case {os:find_executable("candle.exe"),
                  os:find_executable("light.exe")} of
                {C, L} when C == false; L == false ->
                    rebar_api:abort("{~p,~p} candle.exe/light.exe not found "
                                    "make sure http://wixtoolset.org/ is installed and in path",
                                    [?MODULE, ?LINE]);
                _ -> ok
            end;
        _ ->
            rebar_api:abort("unsupported ~p", [SYSTEM_ARCH])
    end,
    {ok, RootDir} = file:get_cwd(),
    Profile =
    case rebar_state:current_profiles(State) of
        [default, P] -> P;
        Other1 -> rebar_api:abort("{~p,~p} bad profiles : ~p",
                                  [?MODULE, ?LINE, Other1])
    end,
    ReleaseDir = filename:join([RootDir, "_build", Profile, "rel"]),
    {Args,_} = rebar_state:command_parsed_args(State),
    if length(Args) == 0 ->
           rebar_api:abort("missing arguments");
       true ->
           ?D("Args ~p", [Args])
    end,
    [AppInfo] = rebar_state:project_apps(State),
    AppName = binary_to_list(rebar_app_info:name(AppInfo)),
    Version = rebar_app_info:original_vsn(AppInfo),
    Description = proplists:get_value(
                    description, rebar_app_info:app_details(AppInfo), ""),
    ?D("OTP ~p, ARCH ~p, WORD ~p profile ~p~nRoot ~p~nReleaseDir ~p"
       "~nApp ~p~nAppVsn ~p~nDetails ~p",
       [OTP_VSN, SYSTEM_ARCH, WORDSIZE, Profile, RootDir, ReleaseDir,
        AppName, Version, Description]),
    ?C("rebar_api:console()", []),
    ?I("rebar_api:info()", []),
    ?W("rebar_api:warn()", []),
    ?E("rebar_api:error()", []),
    ?D("debug", []),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
