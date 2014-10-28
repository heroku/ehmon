%%%-------------------------------------------------------------------
%%% @author Alex Arnell <alex.arnell@gmail.com>
%%% @copyright 2014 Heroku
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ehmon_shh_unix).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(RECONNECT_DELAY, 15000).

-record(state, {
            path="/#shh/",
            socket=undefined,
            reconnect_delay=undefined,
            buffer=queue:new()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Path) ->
    gen_server:start_link({local, ehmon_shh_report}, ?MODULE, [Path], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Path]) ->
    {ok, reconnect_after_delay(#state{path=Path})}.

handle_call({send_report, Report}, _From, State0) ->
    FormattedReport = ehmon_shh_report:format_report(Report),
    {Reply, State} = send_report_to_socket(State0, FormattedReport),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(reconnect, State0) ->
    State = handle_connect(connect(State0)),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(#state{ path=Path }=State) ->
    {unixsock:connect(Path), State}.

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

handle_connect({{ok, Socket}, State}) ->
    send_pending_reports(State#state{ socket=Socket, reconnect_delay=undefined });
handle_connect({{error, Reason}, State}) ->
    error_logger:warning_msg("Shh connection ~p error: ~p~n", [State#state.path, Reason]),
    reconnect_after_delay(State).

send_pending_reports(#state{ socket=undefined }=State0) ->
    next_reconnect_delay(State0);
send_pending_reports(#state{ buffer=Buffer0 }=State0) ->
    case queue:len(Buffer0) of
        0 ->
            State0;
        _ ->
            {{value, Report}, Buffer} = queue:out(Buffer0),
            State = State0#state{ buffer=Buffer },
            {_Reply, State1} = send_report_to_socket(State, Report),
            State1
    end.

send_report_to_socket(#state{ socket=undefined }=State, _Report) ->
    {{error, not_connected}, State};
send_report_to_socket(#state{ socket=Socket }=State0, Report) ->
    handle_socket_reply(procket:sendto(Socket, Report), State0, Report).

handle_socket_reply(ok, State, _Report) ->
    {ok, State};
handle_socket_reply({error, Reason=epipe}, State0, Report) ->
    State = buffer_append_report(State0, Report),
    {{error, Reason}, reconnect_after_delay(State)};
handle_socket_reply({error, Reason}, State, _Report) ->
    error_logger:warning_msg("Failed to connect to ssh: ~p~n", [Reason]),
    {{error, Reason}, reconnect_after_delay(State)}.

buffer_append_report(#state{ buffer=Buffer }=State0, Report) ->
    State0#state{ buffer=queue:in(Report, Buffer) }.
