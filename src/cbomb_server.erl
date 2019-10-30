%%% Handles socket connections from game client
-module(cbomb_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, % p  layer's name
                next, % next step, used when initializing
                socket}). % the current socket

-define(SOCK(Msg), {tcp, _Port, Msg}).
-define(TIME, 800).
-define(EXP, 50).

%% Receive "listen socket" from cbomb_sup which we used to listen for TCP cnxns
%% Returns an "accept socket" for  data transfer
start_link(Socket) ->
   gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    %% properly seeding the process
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed({A,B,C}),
    %% Because accepting a connection is a blocking functssion call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

%% init calls this via gen_server:cast to accept the TCP connection
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    io:fwrite("handle_cast derp\n"), 
    % wait accept connection to establish e
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSocket, [{active, once}, {packet, line}, {line_delimiter, $\0}]),
    cbomb_sup:start_socket(), %start acceptor child process
    %send(AcceptSocket, "<badxml/>", []),
    io:fwrite("handle_cast returning... packet mode: line, line delim: null\n"),
    {noreply, S#state{socket=AcceptSocket, next=xml}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
  
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(?SOCK(Str), S = #state{socket=AcceptSocket, next=xml}) ->
    io:fwrite("handle_info state next=xml...\n"),
    Tag = cbomb_xml:get_tag(Str),
    Reply = cbomb_xml:get_response(Tag),
    %gen_server:cast(self(), roll_stats),
    send(AcceptSocket, Reply, []),
    {noreply, S#state{socket=AcceptSocket, next=xml}};

%this is unused junk, should be deleted or used for another real state:
handle_info(?SOCK(Str), S = #state{socket=AcceptSocket, next=login}) ->
    Tag = cbomb_xml:get_tag(Str),
    Reply = cbomb_xml:get_response(Tag),
    io:fwrite("handle_info state next=login...\n"),
    %gen_server:cast(self(), roll_stats),
    io:fwrite("<login/>\n"),
    send(AcceptSocket, Reply, []),
    {noreply, S#state{socket=AcceptSocket, next=stats}};

handle_info(?SOCK(E), S = #state{socket=Socket}) ->
    io:format("Unexpected input: ~p~n", [E]),
    %send(Socket, "<badxml/>", [E]),
    {noreply, S};
handle_info({tcp_closed, _Socket}, S) ->
    io:fwrite("tcp closed...\n"),
    {stop, normal, S};
handle_info({tcp_error, _Socket, _}, S) ->
    io:fwrite("tcp error...\n"),
    {stop, normal, S};
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, #state{socket=S}) ->
    gen_tcp:close(S);
terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).

%% Send a message through a socket, then make it active again.
%% The difference between an active and a passive socket is that
%% an active socket will send incoming data as Erlang messages, while
%% passive sockets will require to be polled with gen_tcp:recv/2-3.
%%
%% Depending on the context, you might want one or the other. I chose
%% to have active sockets because they feel somewhat easier to work
%% with. However, one problem with active sockets is that all input
%% is blindly changed into messages and makes it so the Erlang VM
%% is somewhat more subject to overload. Passive sockets push this
%% responsibility to the underlying implementation and the OS and are
%% somewhat safer.
%%
%% A middle ground exists, with sockets that are 'active once'.
%% The {active, once} option (can be set with inet:setopts or
%% when creating the listen socket) makes it so only *one* message
%% will be sent in active mode, and then the socket is automatically
%% turned back to passive mode. On each message reception, we turn
%% the socket back to {active once} as to achieve rate limiting.
send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++[0], Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.