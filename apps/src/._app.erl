%%%-------------------------------------------------------------------
%% @doc . public API
%% @end
%%%-------------------------------------------------------------------

-module(._app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ._sup:start_link().

stop(_State) ->
    ok.

%% internal functions
