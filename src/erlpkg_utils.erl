-module(erlpkg_utils).
-include("erlpkg.hrl").

-export([run_port/2, run_port/3]).

run_port(Cmd, Args) ->
    ?D("run_port(~p, ~p)", [Cmd, Args]),
    log_cmd(Cmd, Args,
            erlang:open_port(
              {spawn_executable,Cmd},
              [{line, 128},{args, Args}, exit_status,
               stderr_to_stdout, {parallelism, true}])).
run_port(Cmd, Args, Cwd) ->
    ?D("run_port(~p, ~p, ~p)", [Cmd, Args, Cwd]),
    log_cmd(Cmd, Args,
            erlang:open_port(
              {spawn_executable,Cmd},
              [{cd,Cwd},{line,128},{args,Args},exit_status,stderr_to_stdout,
               {parallelism,true}])).

log_cmd(Cmd, Args, Port) -> log_cmd(Cmd, Args, Port, []).
log_cmd(Cmd, Args, Port, Buf) when is_port(Port) ->
    receive
        {'EXIT',Port,Reason} -> ?E("~s terminated for ~p", [Cmd, Reason]);
        {Port,closed} -> ?E("~s terminated", [Cmd]);
        {Port,{exit_status,Status}} ->
            if Status == 0 -> ?D("~s ~p finished successfully", [Cmd, Args]);
               true -> ?E("~s ~p exit with status ~p", [Cmd, Args, Status])
            end,
            catch erlang:port_close(Port);
        {Port,{data,{F,Line}}} ->
            log_cmd(
              Cmd, Args, Port,
              if F =:= eol ->
                     ?D("~s", [lists:reverse([Line|Buf])]),
                     [];
                 true -> [Line|Buf]
              end);
        {Port,{data,Data}} ->
            ?D("~p", [Data]),
            log_cmd(Cmd, Args, Port)
    end.
