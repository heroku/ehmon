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
         send_report/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(DEFAULT_CONNECT_TIMEOUT, 5000).

-record(state, {client}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [connect_opts()], []).

format_report(Report) ->
    format_report(Report, prefix()).

format_report(Report, Prefix) ->
    format_report(Report, now_to_rfc3339(), Prefix, []).

prefix() ->
    ehmon_app:config(shh_report_prefix, "erlang.ehmon").

send_report(Report) ->
    gen_server:call(?MODULE, {send_report, Report}).

%%%-------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------

init([ConnectOpts]) ->
    {ok, Client} = shh_drv:start_link(ConnectOpts),
    {ok, #state{ client=Client }}.

handle_call({send_report, Report}, _From, State0) ->
    {Reply, State} = handle_send_report(Report, State0),
    {reply, Reply, State};
handle_call(_Request, _From, State0) ->
    {reply, ok, State0}.

handle_cast(_Msg, State0) ->
    {noreply, State0}.

handle_info(_Info, State0) ->
    {noreply, State0}.

terminate(_Reason, _State0) ->
    ok.

code_change(_OldVsn, State0, _Extra) ->
    {ok, State0}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

connect_opts() ->
    connect_opts({tcp,
                  ehmon_app:config(shh_tcp_host, undefined),
                  ehmon_app:config(shh_tcp_port, undefined),
                  ehmon_app:config(shh_tcp_connect_timeout, ?DEFAULT_CONNECT_TIMEOUT)}).

connect_opts({tcp, undefined, Port, _Timeout}) when
      Port =/= undefined ->
    {error, {shh_tcp_host, missing}};
connect_opts({tcp, Host, undefined, _Timeout}) when
      Host =/= undefined ->
    {error, {shh_tcp_port, missing}};
connect_opts({tcp, undefined, undefined, _Timeout}) ->
    connect_opts({unix, ehmon_app:config(shh_unix_socket, undefined)});
connect_opts({unix, undefined}) ->
    {error, {ssh_unix_socket, missing}};
connect_opts(Opts) ->
    Opts.

handle_send_report(Report, State0) ->
    FormattedReport = format_report(Report),
    send_report_to_client(FormattedReport, State0).

send_report_to_client(Report, #state{ client=Client }=State0) ->
    handle_client_reply(shh_drv:send(Client, Report), State0).

handle_client_reply(ok, State) ->
    {ok, State};
handle_client_reply({error, Reason}, State0) ->
    {{error, Reason}, State0}.

format_report([], _RFC3339, _Prefix, Out) ->
    iolist_to_binary(Out);
format_report([Metric | Rest], RFC3339, Prefix, Out) ->
    format_report(Rest, RFC3339, Prefix, prepend_metric(RFC3339, Prefix, Metric, Out)).

prepend_metric(_RFC3339, _Prefix, {otp, _}, Out) ->
    Out;
prepend_metric(RFC3339, Prefix, Metric, Out) ->
    [format_metric(RFC3339, Prefix, Metric) | Out].

format_metric(RFC3339, Prefix, {Name, Val}) ->
    io_lib:format("~s ~s.~w ~w ~w ~s~n", [RFC3339, Prefix, Name, Val, gauge, format_unit(Name)]).

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

now_to_rfc3339() ->
    now_to_rfc3339(os:timestamp()).

now_to_rfc3339(Now) ->
    {Date, {Hour, Min, Sec}} = calendar:now_to_universal_time(Now),
    now_to_rfc3339(Date, {Hour, Min, floor_to_interval(Sec)}, 'Z').

now_to_rfc3339({Year, Month, Day}, {Hour, Minute, Second}, Offset) ->
    OffStr = case Offset of
                 'Z' -> "Z";
                 {Dir, H, M} ->
                     io_lib:format("~s~2.10.0B:~2.10.0B",
                                   [case Dir of '+' -> "+"; '-' -> "-" end,
                                    H, M])
             end,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B~s",
                  [Year, Month, Day, Hour, Minute, Second, OffStr]).

floor_to_interval(Sec) ->
    Interval = ehmon_app:config(report_interval),
    Sec - (Sec rem Interval).