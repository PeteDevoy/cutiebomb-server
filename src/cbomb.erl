%%% starts the cutie bomb server
-module(cbomb).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    %TODO: check return val?
    cbomb_sup:start_link().

stop(_State) ->
    %TODO: cleanup
    %cbomb_sup:stop_link(),
    ok.