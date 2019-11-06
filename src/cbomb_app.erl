%%%-------------------------------------------------------------------
%% @doc cbomb public API
%% @end
%%%-------------------------------------------------------------------

-module(cbomb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cbomb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
