%%%-------------------------------------------------------------------
%%% @author Alex Arnell <alex.arnell@gmail.com>
%%% @copyright 2014 Heroku
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(shh_drv).
-behaviour(gen_server).

%% API
-export([start_link/1, send/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SOCKET_OPTS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(RECONNECT_AFTER, 15000).

-record(state, {opts,
                proto :: unix | tcp,
                socket,
                buffer,
                reconnect_after,
                timer}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link(ConnectOpts) ->
    gen_server:start_link(?MODULE, [ConnectOpts], []).

send(Client, Data) when is_binary(Data) ->
    gen_server:call(Client, {send, Data}).

%%%-------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------

init(Args) ->
    State = init_state_from_args(Args),
    {ok, reconnect_after_delay(State)}.

handle_call({send, Data}, _From, State0) ->
    {Reply, State} = handle_send(Data, State0),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    error_logger:warning_msg("~p unexpected call ~p~n", [?MODULE, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("~p unexpected cast ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(reconnect, State0) ->
    State = handle_reconnect(State0#state{ timer=undefined }),
    {noreply, send_buffered_data(State)};
handle_info({tcp_error, _Socket, _Reason}, State) ->
    % will be followed by a close
    {noreply, State};
handle_info({tcp_closed, _Socket}, State0) ->
    {noreply, reconnect_after_delay(State0)};
handle_info(Info, State) ->
    error_logger:warning_msg("~p unexpected info ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

init_state_from_args([ConnectOpts]) ->
    #state{ opts=ConnectOpts, buffer=queue:new() }.

reconnect_after_delay(#state{ timer=Ref }=State0) when is_reference(Ref) ->
    State0;
reconnect_after_delay(#state{ socket=undefined, timer=undefined }=State0) ->
    State = next_reconnect_delay(State0),
    Ref = erlang:send_after(State#state.reconnect_after, self(), reconnect),
    State#state{ timer=Ref };
reconnect_after_delay(#state{ proto=Proto, socket=Socket }=State0) ->
    close_socket(Proto, Socket),
    reconnect_after_delay(State0#state{ socket=undefined }).

close_socket(unix, Socket) ->
    unixsock:close(Socket);
close_socket(tcp, Socket) ->
    gen_tcp:close(Socket).

next_reconnect_delay(State=#state{ reconnect_after=undefined }) ->
    State#state{ reconnect_after=0 };
next_reconnect_delay(State=#state{ reconnect_after=0 }) ->
    State#state{ reconnect_after=?RECONNECT_AFTER };
next_reconnect_delay(State) ->
    State.

handle_reconnect(#state{ opts={tcp, Host, Port, Timeout} }=State0) ->
    handle_shh_connect_reply(shh_tcp_connect(Host, Port, Timeout), State0#state{ proto=tcp });
handle_reconnect(#state{ opts={unix, Path} }=State0) ->
    handle_shh_connect_reply(shh_unix_connect(Path), State0#state{ proto=unix }).

shh_tcp_connect(Host, Port, Timeout) ->
    gen_tcp:connect(Host, Port, ?SOCKET_OPTS, Timeout).

shh_unix_connect(Path) ->
    unixsock:connect(Path).

handle_shh_connect_reply({ok, Socket}, State0) ->
    State0#state{ socket=Socket, reconnect_after=undefined };
handle_shh_connect_reply({error, Reason}, State) ->
    log_warning(connection, Reason, State),
    reconnect_after_delay(State).

send_buffered_data(#state{ socket=undefined }=State0) ->
    reconnect_after_delay(State0);
send_buffered_data(State0) ->
    case dequeue_from_buffer(State0) of
        {empty, State} -> State;
        {{value, Data}, State} ->
            handle_send_reply(handle_send(Data, State))
    end.

handle_send_reply({ok, State}) ->
    send_buffered_data(State);
handle_send_reply({{error, _Reason}, State}) ->
    State.

dequeue_from_buffer(#state{ buffer=Buffer0 }=State0) ->
    {Item, Buffer} = queue:out(Buffer0),
    State = State0#state{ buffer=Buffer },
    {Item, State}.

handle_send(empty, State) ->
    {{error, empty}, State};
handle_send(Data, #state{ socket=undefined }=State0) ->
    State = append_data_to_buffer(Data, State0),
    {{error, not_connected}, State};
handle_send({value, Data}, State) ->
    handle_send(Data, State);
handle_send(Data, #state{ proto=Proto, socket=Socket }=State) when is_binary(Data) ->
    handle_socket_reply(send_to_socket(Proto, Socket, Data), Data, State).

send_to_socket(tcp, Socket, Data) ->
    gen_tcp:send(Socket, Data);
send_to_socket(unix, Socket, Data) ->
    procket:sendto(Socket, Data).

handle_socket_reply(ok, _Data, State) ->
    {ok, State};
handle_socket_reply({error, Reason}, Data, State0) ->
    log_warning(send, Reason, State0),
    State = append_data_to_buffer(Data, State0),
    {{error, Reason}, reconnect_after_delay(State)}.

append_data_to_buffer(Data, #state{ buffer=Buffer0 }=State0) ->
    State0#state{ buffer=queue:in(Data, Buffer0) }.

log_warning(Action, Reason, #state{ opts=Opts }) ->
    error_logger:warning_msg("~p ~p got ~p using ~p~n", [?MODULE, Action, Reason, Opts]).
