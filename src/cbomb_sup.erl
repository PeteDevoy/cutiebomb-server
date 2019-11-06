%%% The supervisor in charge of all the socket acceptors.

%%% More info on supervisors: http://erlang.org/doc/man/supervisor.html
%%%                           https://learnyousomeerlang.com/supervisors

%%% Based off code borrowed lovingly from: https://learnyousomeerlang.com
%%% /static/erlang/processquest/apps/sockserv-1.0.0/src/sockserv_sup.erl

-module(cbomb_sup).

-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
    io:fwrite("cbomb_sup:start_link called\n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:fwrite("cbomb_sup:init called\n"),

    %{ok, Port} = application:get_env(port),
    Port = 8080,
    
    %% {active, once}? see https://stackoverflow.com/questions/51364148/
    %% {packet, line}? Line mode, a packet is a line-terminated with newline,
    %%                 lines longer than the receive buffer are truncated
    {ok, ListenSocket} = gen_tcp:listen(Port, [{ip, {127,0,0,1}}, {active,once}, {packet, 0}]),
    %inet:setopts(ListenSocket, [{line_delimiter, NullByte}]),1

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 60,
    MaxSecondsBetweenRestarts = 3600,
    ChildSpecification = {  
        socket, % id
        {cbomb_server, start_link, [ListenSocket]}, % mod, fun, [args] to start
        temporary, % the process is never to be restarted
        1000, % milliseconds to wait for exit signal from child proc before kill
        worker, % type of child process
        [cbomb_server] % modules used http://erlang.org/doc/man/supervisor.html
    },

    spawn_link(fun empty_listeners/0),

    {
        ok,
        {
            {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts}, % flags
            [ChildSpecification]
        }
    }.

start_socket() ->
    io:fwrite("cbomb_sup:start_socket called\n"),
    supervisor:start_child(?MODULE, []).

%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.