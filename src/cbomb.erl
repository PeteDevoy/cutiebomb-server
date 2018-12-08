%%% starts the cutie bomb server
-module(cbomb).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    cbomb_sup:start_link().

stop(_) -> ok.