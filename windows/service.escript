#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

main([Cmd|CmdParams]) when Cmd=="install";Cmd=="uninstall";Cmd=="start";
                           Cmd=="stop";Cmd=="restart";Cmd=="console";
                           Cmd=="query";Cmd=="attach";Cmd=="upgrade" ->
    ScriptFile = escript:script_name(),
    RootPath = case lists:reverse(
                      filename:split(
                        filename:absname(filename:dirname(ScriptFile))
                       )) of
                   ["bin"|Rest] -> filename:join(lists:reverse(Rest));
                   [".","bin"|Rest] -> filename:join(lists:reverse(Rest))
               end,
    ReleaseDir = filename:join([RootPath, "releases"]),
    {ok, StartErlData} = file:read_file(filename:join([ReleaseDir,
                                                       "start_erl.data"])),
    [ErtsVsn, AppVsn] = re:split(re:replace(StartErlData, "[\r\n]+", "",
                                            [global, {return, list}]),
                                 " ", [{return, list}]),
    VmArgsOrig      = filename:join([ReleaseDir, AppVsn, "vm.args.orig"]),
    ErtsBin         = filename:join([RootPath, "erts-"++ErtsVsn, "bin"]),
    ErlSrv          = filename:join([ErtsBin, "erlsrv.exe"]),
    Werl            = filename:join([ErtsBin, "werl.exe"]),

    VmArgsProps = case filelib:is_file(VmArgsOrig) of
                      true -> read_vm_args(VmArgsOrig);
                      _ ->
                          VmArgs = filename:join([ReleaseDir, AppVsn,
                                                  "vm.args"]),
                          ok = file:rename(VmArgs, VmArgsOrig),
                          VmArgsPropsInt = read_vm_args(VmArgsOrig),
                          NewVmArgs
                          = lists:flatten(
                              [[K," ",V,"\n"]
                               || {K, V} <- proplists:delete(
                                              "-name", VmArgsPropsInt)]),
                          ok = file:write_file(VmArgs, list_to_binary(NewVmArgs)),
                          VmArgsPropsInt
                  end,
    NodeName = proplists:get_value("-name", VmArgsProps),
    AppName = filename:basename(ScriptFile, ".escript"),
    [SrvName|_] = CmdParams,
    case Cmd of
        "install" ->
            StartErl = filename:join([RootPath, "bin", "start_erl.cmd"]),
            [_,ServiceDescription|_] = CmdParams,
            run_port(ErlSrv, ["add",            SrvName,
                              "-c",             ServiceDescription,
                              "-name",          NodeName,
                              "-w",             "\""++RootPath++"\"",
                              "-m",             "\""++StartErl++"\"",     
                              "-args",          "\" ++ "++AppName++" ++ "
                                                    ++RootPath++"\"",
                              "-stopaction",    "\"init:stop().\""]);
        "uninstall" ->
            run_port(ErlSrv, ["stop", SrvName]),
            run_port(ErlSrv, ["remove", SrvName]),
            run_port(filename:join([ErtsBin, "epmd.exe"]), ["-kill"]);
        "start" -> run_port(ErlSrv, ["start", SrvName]);
        "stop" -> run_port(ErlSrv, ["stop", SrvName]);
        "query" -> run_port(ErlSrv, ["list", SrvName]);
        "console" ->
            NodeBootScript = filename:join([ReleaseDir, AppVsn, AppName]),
            SysConfig = filename:join([ReleaseDir, AppVsn, "sys.config"]),
            run_port(Werl, ["-boot",        NodeBootScript,
                            "-config",      SysConfig,
                            "-args_file",   VmArgsOrig]);
        "attach" ->
            CleanBootScript = filename:join([ReleaseDir, AppVsn, "start_clean"]),
            [_,Host] = re:split(NodeName, "@", [{return, list}]),
            io:format("NodeName ~p~n", [NodeName]),
            run_port(Werl, ["-boot",        CleanBootScript,
                            "-remsh",       NodeName,
                            "-name",        "console@"++Host,
                            "-setcookie",   proplists:get_value("-setcookie",
                                                                VmArgsProps)
                           ]);
        "restart" ->
            main(["stop"|Rest]),
            main(["start"|Rest]);
        "upgrade" ->
            io:format("ERROR: not supported yet~n")
    end;
main(_) ->
    io:format("Usage: [install|uninstall|start|stop|restart|console|query|"
              "attach|upgrade]~n").


read_vm_args(VmArgs) ->
    {ok, VmArgsData} = file:read_file(VmArgs),
    [case re:run(VmArgsRow, "([^ ]+)(.*)",
                 [global, {capture, [1,2], list}]) of
         {match, [[K,V]]} -> {K, string:strip(V, both)}
     end || VmArgsRow <- re:split(string:strip(
                                    re:replace(VmArgsData, "#.*[\r\n]+", "",
                                               [global, {return, list}]),
                                    both, $\n), "[\n]+", [{return, list}])].

run_port(Cmd, Args) ->
    log_cmd(Cmd,
            erlang:open_port({spawn_executable, Cmd},
                             [{line, 128},{args, Args}, exit_status,
                              stderr_to_stdout, {parallelism, true}])).

log_cmd(Cmd, Port) when is_port(Port) ->
    receive
        {'EXIT',Port,Reason} ->
            io:format("Error : ~s terminated for ~p~n", [Cmd, Reason]);
        {Port,closed} ->
            io:format("Error : ~s terminated~n", [Cmd]);
        {Port,{exit_status,Status}} ->
            io:format("~s exit with status ~p~n", [Cmd, Status]),
            catch erlang:port_close(Port);
        {Port,{data,{F,Line}}} ->
            io:format("~s" ++ if F =:= eol -> "~n"; true -> "" end, [Line]),
            log_cmd(Cmd, Port);
        {Port,{data,Data}} ->
            io:format("~p", [Data]),
            log_cmd(Cmd, Port)
    end.
