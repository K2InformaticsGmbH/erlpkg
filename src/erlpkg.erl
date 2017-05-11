-module(erlpkg).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = erlpkg_prv:init(State),
    {ok, State1}.
