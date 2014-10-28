%%%-------------------------------------------------------------------
%%% @author Alex Arnell <alex.arnell@gmail.com>
%%% @copyright 2014 Heroku
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ehmon_shh_tcp).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SOCKET_OPTS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(RECONNECT_DELAY, 15000).

-record(state, {
          host = '127.0.0.1',
          port = 8000,
          connect_timeout = 5000,
          reconnect_delay = undefined,
          socket = undefined
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Port, Prefix) ->
    gen_server:start_link({local, ehmon_shh_report}, ?MODULE, [Host, Port, Prefix], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port]) ->
    State = #state{host=Host,
                   port=Port},
    State1 = reconnect_after_delay(State),
    {ok, State1}.


handle_call({send_report, Report}, _From, State) ->
    Reply = tcp_send_report(State#state.socket, ehmon_shh_report:format_report(Report)),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_error, _Socket, _Reason}, State) ->
    % will be followed by a close
    {noreply, State};
handle_info({tcp_closed, _Socket}, State0) ->
    State = reconnect_after_delay(State0),
    {noreply, State#state{ socket=undefined }};
handle_info(reconnect, State0) ->
    case connect(State0) of
        {ok, State} ->
            {noreply, State#state{ reconnect_delay=undefined }};
        {error, {connection_error, Reason}} ->
            error_logger:warning_msg("Shh connection error: ~p~n", [Reason]),
            State = reconnect_after_delay(State0),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(State) ->
    case gen_tcp:connect(State#state.host, State#state.port,
                         ?SOCKET_OPTS, State#state.connect_timeout) of
        {ok, Socket} ->
            {ok, State#state{ socket=Socket }};
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.

reconnect_after_delay(State0) ->
    State = next_reconnect_delay(State0),
    erlang:send_after(State#state.reconnect_delay, self(), reconnect), 
    State.

next_reconnect_delay(State=#state{ reconnect_delay=undefined }) ->
    State#state{ reconnect_delay=0 };
next_reconnect_delay(State=#state{ reconnect_delay=0}) ->
    State#state{ reconnect_delay=?RECONNECT_DELAY };
next_reconnect_delay(State) ->
    State.

tcp_send_report(undefined, _Report) ->
    {error, not_connected};
tcp_send_report(Socket, Report) ->
    case gen_tcp:send(Socket, Report) of
        ok -> ok;
        Err={error, _Reason} ->
            Err
    end.

