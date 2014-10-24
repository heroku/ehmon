%%%-------------------------------------------------------------------
%%% @author Alex Arnell <alex.arnell@gmail.com>
%%% @copyright 2014 Heroku
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ehmon_shh_report).
-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/3,
         send_report/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SOCKET_OPTS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(RECONNECT_DELAY, 15000).
-define(PREFIX, "erlang.ehmon").

-record(state, {
          host = '127.0.0.1',
          port = 8000,
          prefix = ?PREFIX,
          connect_timeout = 5000,
          reconnect_delay = undefined,
          socket = undefined
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Host = ehmon_app:config(shh_report_host, "127.0.0.1"),
    Port = ehmon_app:config(shh_report_port, 8000),
    Prefix = ehmon_app:config(shh_report_prefix, ?PREFIX),
    start_link(Host, Port, Prefix).

start_link(Host, Port, Prefix) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Prefix], []).

send_report(Report) ->
    gen_server:call(?MODULE, {send_report, Report}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Host, Port, Prefix]) ->
    State = #state{host=Host,
                   port=Port,
                   prefix=Prefix},
    State1 = reconnect_after_delay(State),
    {ok, State1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send_report, Report}, _From, State0) ->
    {Reply, State} = handle_send_report(Report, State0),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
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

handle_send_report(Report, State) ->
    Reply = tcp_send_report(State#state.socket, format_report(Report, State#state.prefix)),
    {Reply, State}.

format_report(Report, Prefix) ->
    format_report(Report, Prefix, []).

format_report([], _Prefix, Out) ->
    iolist_to_binary(Out);
format_report([Metric | Rest], Prefix, Out) ->
    format_report(Rest, Prefix, prepend_metric(Prefix, Metric, Out)).

prepend_metric(_Prefix, {otp, _}, Out) ->
    Out;
prepend_metric(Prefix, Metric, Out) ->
    io:format(standard_io, "ehmon_report ~s~n", [format_metric(Prefix, Metric)]),
    [format_metric(Prefix, Metric) | Out].

format_metric(Prefix, {Name, Val}) ->
    io_lib:format("~w ~s.~w ~w ~w ~s~n", [unixtime(), Prefix, Name, Val, gauge, format_unit(Name)]).

format_unit(cswit) ->
    "ContextSwitches,cswit";
format_unit(procs) ->
    "Processes,procs";
format_unit(ports) ->
    "Ports";
format_unit(maxfds) ->
    "FileDescriptors,fds";
format_unit(etstabs) ->
    "EtsTables,etstabs";
format_unit(scheduler) ->
    "SchedulerUsage,cores";
format_unit(memtot) ->
    "Bytes,b";
format_unit(rq) ->
    format_unit(procs);
format_unit(maxprocs) ->
    format_unit(procs);
format_unit(maxports) ->
    format_unit(ports);
format_unit(maxetstabs) ->
    format_unit(etstabs);
format_unit(memproc) ->
    format_unit(memtot);
format_unit(memets) ->
    format_unit(memtot);
format_unit(membin) ->
    format_unit(memtot);
format_unit(memcode) ->
    format_unit(memtot);



format_unit(milliseconds) ->
    "MilliSeconds,ms";
format_unit(nanoseconds) ->
    "NanoSeconds,ns";
format_unit(requests) ->
    "Requests,reqs";
format_unit(errors) ->
    "Errors,errs";
format_unit(packets) ->
    "Packets,pkts";
format_unit(inodes) ->
    "INodes,inodes";
format_unit(files) ->
    "Files,files";
format_unit(connections) ->
    "Connections,conns";
format_unit(sockets) ->
    "Sockets,socks";
format_unit(avg) ->
    "Avg,avg";
format_unit(objects) ->
    "Objects,objs";
format_unit(routines) ->
    "Routines,routines".

tcp_send_report(undefined, _Report) ->
    {error, not_connected};
tcp_send_report(Socket, Report) ->
    case gen_tcp:send(Socket, Report) of
        ok -> ok;
        Err={error, _Reason} ->
            Err
    end.

unixtime() ->
    unixtime(os:timestamp()).

unixtime({Mega, Sec, Micro}) ->
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.
