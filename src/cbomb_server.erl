%%% Handles socket connections from game client
-module(cbomb_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, % p  layer's name
                userid,
                avatar,
                next, % next step, used when initializing
                socket, % the current socket
                opponentid}).

-record(tag, {name, attributes, children}).

-define(SOCK(Msg), {tcp, _Port, Msg}).
-define(TIME, 800).
-define(EXP, 50).

%% Receive "listen socket" from cbomb_sup which we used to listen for TCP cnxns
%% Returns an "accept socket" for  data transfer
start_link(Socket) ->
   io:fwrite("cbomb_server:start_link called\n"),
   gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
   io:fwrite("cbomb_server:init called\n"),
    %% properly seeding the process
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed({A,B,C}),
    %% Because accepting a connection is a blocking functssion call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

%% init calls this via gen_server:cast to accept the TCP connection
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    io:fwrite("cbomb_server:handle_cast called\n"), 
    % wait accept connection to establish e
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSocket, [{active, once}, {packet, line}, {line_delimiter, $\0}]),
    {ok, Pid} = cbomb_sup:start_socket(), %start acceptor child process
    PidStr = pid_tokens(Pid),
    io:fwrite(lists:concat(["Acceptor proccess pid is ", PidStr, "\n"])), 
    %send(AcceptSocket, "<badxml/>", []),
    io:fwrite("cbomb_server:handle_cast returning... packet mode: line, line delim: null\n"),
    io:fwrite(lists:concat(["Set state: connect.\n"])),
    {noreply, S#state{socket=AcceptSocket, next=connect}};

handle_cast(logged_on, S = #state{socket=AcceptSocket}) ->
    ok = ebus:sub(self(), "lobby"), %sub to lobby messages
    ok = ebus:sub(self(), S#state.userid), %sub to direct messages 
    AddUserTag = cbomb_xml:add_user(S#state.name, S#state.avatar,  S#state.userid),
    ok = ebus:pub("lobby", {lobby, AddUserTag}),
    io:fwrite(lists:concat([S#state.name, " :: set state: lobby_lurk. (", S#state.userid, ")\n"])),
    {noreply, S#state{next=lobby_lurk}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
  
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_info(?SOCK(Str="<policy-file-request/>\0"), S = #state{socket=AcceptSocket, next=connect}) ->
    io:fwrite("handle_info state next=connect(flash policy)...\n"),
    io:fwrite(lists:concat(["flashpolicy xml is ", Str, "\n"])),
    Tag = cbomb_xml:get_tag(Str),
    Reply = cbomb_xml:get_response(Tag),
    %gen_server:cast(self(), roll_stats),
    send(AcceptSocket, Reply, []),
    {noreply, S};

handle_info(?SOCK(Str), S = #state{socket=AcceptSocket, next=connect}) ->
    %io:fwrite("handle_info state next=connect...\n"),
    %io:fwrite(lists:concat(["connect xml is ", Str, "\n"])),
    Tag = cbomb_xml:get_tag(Str),
    Username = proplists:get_value(username, Tag#tag.attributes),
    Avatar = proplists:get_value(avatar, Tag#tag.attributes),
    PidStr = pid_tokens(self()), %FIXME: better unique ID. Enumerate socks?
    Reply = cbomb_xml:logged_on(Username, PidStr),
    send(AcceptSocket, Reply, []),
    gen_server:cast(self(), logged_on),
    io:fwrite(lists:concat([S#state.name, " :: set state: lobby_lurk. I am (", S#state.userid, ")\n"])),
    {noreply, S#state{name=Username, userid=PidStr, avatar=Avatar, next=lobby_lurk}};

handle_info(?SOCK(Str), S = #state{socket=AcceptSocket, next=lobby_lurk}) ->
    %TODO: below if statement should probably be split into different functions 
    io:fwrite(lists:concat([S#state.name, " :: sent message in lobby_lurk state\n"])),
    %io:fwrite("handle_info state next=lobby_lurk...\n"),
    Tag = cbomb_xml:get_tag(Str),
    IsTargeted = proplists:is_defined(targetUserId, Tag#tag.attributes),
    if
        true == IsTargeted, Tag#tag.name == accept ->
            TargetUser = proplists:get_value(targetUserId, Tag#tag.attributes),
            ok = ebus:pub(TargetUser, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
            ok = inet:setopts(AcceptSocket, [{active, once}]);
        true == IsTargeted, Tag#tag.name == invite ->
            TargetUser = proplists:get_value(targetUserId, Tag#tag.attributes),
            ok = ebus:pub(TargetUser, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
            ok = inet:setopts(AcceptSocket, [{active, once}]);
        true == IsTargeted, Tag#tag.name == decline ->
            TargetUser = proplists:get_value(targetUserId, Tag#tag.attributes),
            ok = ebus:pub(TargetUser, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
            ok = inet:setopts(AcceptSocket, [{active, once}]);
        true == IsTargeted, Tag#tag.name == cancel ->
            TargetUser = proplists:get_value(targetUserId, Tag#tag.attributes),
            ok = ebus:pub(TargetUser, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
            ok = inet:setopts(AcceptSocket, [{active, once}]);
        true ->
            TargetUser = none,
            Reply = cbomb_xml:get_response(Tag),
            ok = ebus:pub("lobby", {lobby, Reply})
    end,
    %send(AcceptSocket, Reply, []),
    {noreply, S#state{opponentid=TargetUser}};

handle_info(?SOCK(Str), S = #state{socket=AcceptSocket, next=terrain_choice}) ->
    io:fwrite(lists:concat([S#state.name, " :: sent message in terrain_choice state\n"])),
    %io:fwrite("handle_info state next=terrain_choice...\n"),
    Tag = cbomb_xml:get_tag(Str),
    %io:fwrite(lists:concat([S#state.name, "(", S#state.userid, ") -> Self (", S#state.userid, ")", Str, "\n"])),
    %io:fwrite(lists:concat([S#state.name, "(", S#state.userid, ") -> Opponent (", S#state.opponentid, ")", Str, "\n"])),
    ok = ebus:pub(S#state.userid, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
    ok = ebus:pub(S#state.opponentid, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
    {noreply, S#state{socket=AcceptSocket, next=terrain_choice}};

handle_info(?SOCK(Str), S = #state{socket=AcceptSocket, next=in_game}) ->
    io:fwrite(lists:concat([S#state.name, " :: sent message in in_game state\n"])),
    %io:fwrite("handle_info state next=in_game...\n"),
    Tag = cbomb_xml:get_tag(Str),
    if
        Tag#tag.name == exitToLobby ->
            %TODO handle surrender
            
            LeavingUser = proplists:get_value(userid, Tag#tag.attributes, none),
            io:fwrite(lists:concat([S#state.name, " :: ebus:sub to lobby\n"])),
            ok = ebus:sub(self(), "lobby"), %sub to lobby messages
            AddUserTag = cbomb_xml:add_user(S#state.name, S#state.avatar,  S#state.userid),
            ok = ebus:pub("lobby", {lobby, AddUserTag}),
            NextState = lobby_lurk;
        Tag#tag.name == s ->
            io:fwrite(lists:concat([S#state.name, " ::  (", S#state.userid, ") -> Opponent (", S#state.opponentid, ") ", Tag#tag.name, "\n"])),
            ok = ebus:pub(S#state.opponentid, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
            %because we don't send anything back we need to set the socket to
            %listen here because it will not called by the send() function
            ok = inet:setopts(AcceptSocket, [{active, once}]),
            NextState = in_game;
        true ->
            ok = ebus:pub(S#state.opponentid, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
            ok = ebus:pub(S#state.userid, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
            NextState = in_game
    end,
    %ok = ebus:pub(S#state.userid, {{private, Tag#tag.name}, string:trim(Str, trailing, "\0")}),
    io:fwrite(lists:concat([S#state.name, " :: leaving function\n"])),
    {noreply, S#state{socket=AcceptSocket, next=NextState}};

handle_info({lobby, Msg},  S = #state{socket=AcceptSocket}) ->
    %io:fwrite(lists:concat(["LOBBY: ", Msg, "I am ", S#state.name, " (", S#state.userid, ")\n"])),
    Tag = cbomb_xml:get_tag(Msg),
    if
        %if we have an addUser tag for another user, send them one for us
        Tag#tag.name == addUser ->
            ArrivingUser = proplists:get_value(userid, Tag#tag.attributes, none),
            AddUserTag = cbomb_xml:add_user(S#state.name, S#state.avatar,  S#state.userid),
            ebus:pub(ArrivingUser, {{private, addUser}, AddUserTag}),
            send(AcceptSocket, Msg, []);
        true -> 
            send(AcceptSocket, Msg, [])
    end,
    {noreply, S};

handle_info({{private, addUser}, Msg},  S = #state{socket=AcceptSocket}) ->
    %io:fwrite(lists:concat(["PRIVATE: ", Msg, "I am ", S#state.name, " (", S#state.userid, ")\n"])),
    send(AcceptSocket, Msg, []),
    {noreply, S};

handle_info({{private, accept}, Msg},  S = #state{socket=AcceptSocket}) ->
    %io:fwrite(lists:concat(["PRIVATE: ", Msg, "I am ", S#state.name, " (", S#state.userid, ")\n"])),
    Tag = cbomb_xml:get_tag(Msg),
    ChallengerID = proplists:get_value(targetUserId, Tag#tag.attributes),
    ChallengeeID = proplists:get_value(userid, Tag#tag.attributes),
    send(AcceptSocket, Msg, []),
    ebus:pub(ChallengeeID, {{private, addToService}}),
    ebus:pub(ChallengerID, {{private, addToService}}),
    %TODO: next = in_game
    if
        S#state.userid == ChallengeeID ->
            OpponentID = ChallengerID;
        true ->
            OpponentID = ChallengeeID
    end,
    %challengee takes their turn first
    %io:fwrite(lists:concat([S#state.name, " :: set state: terrain_choice. I am ", S#state.name, " (", S#state.userid, ")\n"])),
    {noreply, S#state{opponentid=OpponentID}};

handle_info({{private, addToService}},  S = #state{socket=AcceptSocket, next=lobby_lurk}) ->
    %io:fwrite(lists:concat(["PRIVATE (in_game:addToService) : I am ", S#state.name, " (", S#state.userid, ")\n"])),
    io:fwrite(lists:concat([S#state.name, " :: ebus:unsub from lobby\n"])),
    ok = ebus:unsub(self(), "lobby"), %unsub from lobby messages
    Msg = lists:concat(["<addedToService username=\"", S#state.name, "\" userid=\"", S#state.userid ,"\"/>"]),
    %passthrough
    send(AcceptSocket, Msg, []),
    {noreply, S#state{next=terrain_choice}};

handle_info({{private, selection}, Msg},  S = #state{socket=AcceptSocket, next=terrain_choice}) ->
    %io:fwrite(lists:concat(["PRIVATE (terrain_choice): ", Msg, "I am ", S#state.name, " (", S#state.userid, ")\n"])),
    io:fwrite(lists:concat([S#state.name, " :: set state: in_game. (", S#state.userid, ")\n"])),    
    send(AcceptSocket, Msg, []),
    {noreply, S#state{next=in_game}};

handle_info({{private, s}, Msg},  S = #state{socket=AcceptSocket}) ->
    %io:fwrite(lists:concat(["PRIVATE (in_game): ", Msg, "I am ", S#state.name, " (", S#state.userid, ")\n"])),
    send(AcceptSocket, Msg, []),
    {noreply, S};

handle_info({{_, disconnectUser}, Msg},  S = #state{socket=AcceptSocket}) ->
    send(AcceptSocket, Msg, []),
    {noreply, S};

handle_info({{private, _}, Msg},  S = #state{socket=AcceptSocket}) ->
    %io:fwrite(lists:concat(["PRIVATE (in_game): ", Msg, "I am ", S#state.name, " (", S#state.userid, ")\n"])),
    %passthrough
    send(AcceptSocket, Msg, []),
    {noreply, S};

handle_info(?SOCK(E), S = #state{socket=Socket}) ->
    io:format("Unexpected input: ~p~n", [E]),
    %send(Socket, "<badxml/>", [E]),
    {noreply, S};

handle_info({tcp_closed, _Socket}, S = #state{next=lobby_lurk}) ->
    % clean-up
    ebus:unsub(self(), "lobby"),

    % notify lobby lurkers
    Tag = cbomb_xml:disconnect_user(S#state.userid),
    ebus:pub("lobby", {{lobby, disconnectUser}, Tag}),
    {stop, normal, S};

handle_info({tcp_closed, _Socket}, S = #state{next=in_game}) ->
    Tag = cbomb_xml:disconnect_user(S#state.userid),
    %notify opponent
    ebus:pub(S#state.opponentid, {{private, disconnectUser}, Tag}),
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

pid_tokens(Pid) ->
    PidStr = pid_to_list(Pid),
    PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
    [N, P1, P2] = [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])],
    P1.