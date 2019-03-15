#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname inject_git_rev

main(#{git := true}) ->
	Script = escript:script_name(),
	[_, _ | PathRev] = lists:reverse(filename:split(filename:dirname(Script))),
	Dir = filename:join(lists:reverse(["lib" | PathRev])),
	Libs = lists:filtermap(
		fun(Lib) ->
			L = filename:join(Dir, Lib),
			case filelib:is_dir(L) of
				true -> {true, L};
				_ -> false
			end
		end, filelib:wildcard("*", Dir)
	),
	io:format(user, "Libs ~p~n", [Libs]),
	libs(Libs),
	halt(0); % `rebar3 as prod release` success
main(#{git := false}) ->
	io:format("~p: command `git` isn't in path. Aborting release!~n", [?LINE]),
	% fails `rebar3 as prod release`
	halt(1);
main(_) ->
	 main(#{git => git()}).

git() ->
    case os:find_executable("git") of
        false -> false;
        GitExe when is_list(GitExe) -> true
    end.

libs([]) -> all_done;
libs([Lib | Libs]) ->
	{ok, Cwd} = file:get_cwd(),
	case file:set_cwd(Lib) of
        ok ->
            case list_to_binary(os:cmd("git rev-parse HEAD")) of
                <<"fatal:", _>> ->
					io:format(user, "not_git ~p~n", [Lib]);
                Revision ->
                    case re:run(
                        os:cmd("git remote -v"),
                        "(http[^ ]+)", [{capture, [1], list}]
                    ) of
                        {match,[Url|_]} ->
                            lib(Url, Revision);
                        _ ->
							io:format(user, "no_remote ~p~n", [Lib])
                    end
            end;
        _ -> 
			io:format(user, "not_found ~p~n", [Lib])
    end,
	ok = file:set_cwd(Cwd),
	libs(Libs).

lib(Url, Revision) ->
	CleanUrl = re:replace(Url, "\\.git", "", [{return, list}]),
	RawUrlPrefix = list_to_binary([CleanUrl, "/raw/", string:trim(Revision)]),
	Beams = filelib:wildcard("*.beam", "ebin"),
	git_url(Beams, RawUrlPrefix).

git_url([], _RawUrlPrefix) -> all_done;
git_url([Beam | Beams], RawUrlPrefix) ->
	Url = git_file(RawUrlPrefix, Beam),
    BeamFile = filename:join("ebin", Beam),
	{ok, _, Chunks} = beam_lib:all_chunks(BeamFile),
    CInf = binary_to_term(proplists:get_value("CInf", Chunks)),
    CInfOpts = proplists:get_value(options, CInf),
    OptCInfo = proplists:get_value(compile_info, CInfOpts, [{gitOrigin, Url}]),
    OptCInfo1 = lists:keyreplace(gitOrigin, 1, OptCInfo, {gitOrigin, Url}),
    CInfOpts1 = lists:keystore(compile_info, 1, CInfOpts, {compile_info, OptCInfo1}),
    CInf1 = lists:keyreplace(options, 1, CInf, {options, CInfOpts1}),
    Chunks1 = lists:keyreplace("CInf", 1, Chunks, {"CInf", term_to_binary(CInf1)}),
    {ok, PatchBeamBin} = beam_lib:build_module(Chunks1),
    ok = file:write_file(BeamFile, PatchBeamBin),
	io:format("gitOrigin inserted into ~s~n", [BeamFile]),
	git_url(Beams, RawUrlPrefix).

git_file(RawUrlPrefix, BeamFile) ->
	SrcFile = re:replace(BeamFile, "\\.beam", ".erl", [{return, list}]),
	SrcFilePath = filename:join("src", SrcFile),
	git_file(RawUrlPrefix, BeamFile, SrcFilePath).

git_file(RawUrlPrefix, BeamFile, SrcFilePath) ->
    case {
        list_to_binary(os:cmd("git ls-files --error-unmatch " ++ SrcFilePath)),
        filename:extension(SrcFilePath)
    } of
        {<<"error: pathspec", _/binary>>, ".erl"} ->
            git_file(
                RawUrlPrefix, BeamFile,
                re:replace(
                    SrcFilePath, "\\.erl", "\\.yrl",
                    [{return, list}]
                )
            );
        {<<"error: pathspec", _/binary>>, ".yrl"} ->
            git_file(
                RawUrlPrefix, BeamFile,
                re:replace(
                    SrcFilePath, "\\.yrl", "\\.xrl",
                    [{return, list}]
                )
            );
        _ ->
			filename:join(RawUrlPrefix, SrcFilePath)
    end.
