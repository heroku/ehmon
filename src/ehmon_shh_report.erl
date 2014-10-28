%%%-------------------------------------------------------------------
%%% @author Alex Arnell <alex.arnell@gmail.com>
%%% @copyright 2014 Heroku
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ehmon_shh_report).

%% API
-export([start_link/0,
         format_report/1,
         format_report/2,
         prefix/0,
         send_report/1]).

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
    start_client(connect_opts()).

format_report(Report) ->
    format_report(Report, prefix()).

format_report(Report, Prefix) ->
    format_report(Report, now_to_rfc3339(), Prefix, []).

prefix() ->
    ehmon_app:config(shh_report_prefix, "erlang.ehmon").

send_report(Report) ->
    gen_server:call(?MODULE, {send_report, Report}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_opts() ->
    connect_opts({tcp,
                  ehmon_app:config(shh_report_host, undefined),
                  ehmon_app:config(shh_report_port, undefined)}).

connect_opts({tcp, undefined, Port}) when
      Port =/= undefined ->
    {error, {shh_report_host, missing}};
connect_opts({tcp, Host, undefined}) when
      Host =/= undefined ->
    {error, {shh_report_port, missing}};
connect_opts({tcp, undefined, undefined}) ->
    connect_opts({unix, ehmon_app:config(shh_report_socket, undefined)});
connect_opts({unix, undefined}) ->
    {error, {ssh_report_socket, missing}};
connect_opts(Opts) ->
    Opts.

start_client({tcp, Host, Port}) ->
    ehmon_shh_tcp:start_link(Host, Port);
start_client({unix, Path}) ->
    ehmon_shh_unix:start_link(Path).

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
