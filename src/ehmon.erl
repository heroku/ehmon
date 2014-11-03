%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ehmon public API
%% @end
%%%-------------------------------------------------------------------
-module(ehmon).

%% API
-export([ init/0
          ,update/1
          ,info_report/1
          ,stdout_report/1
          ,report/1
        ]).

%% We store scheduler data from the last run here to compare.
-record(state, {prev_times :: undefined | [pos_integer()],
                current_times :: [pos_integer()]}).

%%====================================================================
%% API
%%====================================================================

init() ->
    #state{prev_times = undefined,
           current_times=erlang:statistics(scheduler_wall_time)}.

update(#state{current_times=SchedulerTimes}) ->
    #state{prev_times=SchedulerTimes,
           current_times=erlang:statistics(scheduler_wall_time)}.

info_report(Report) ->
    error_logger:info_msg("ehmon_report ~s~n", [report_string(Report)]).

stdout_report(Report) ->
    io:format(standard_io, "ehmon_report ~s~n", [report_string(Report)]).

-spec report(#state{}) -> iolist().
report(State) ->
    Stats = [{context_switches,
              element(1, erlang:statistics(context_switches))},
              {run_queue, erlang:statistics(run_queue)}],
    Info = [{K, erlang:system_info(K)} ||
               K <- [check_io, otp_release, process_count, process_limit] ],
    Mem = erlang:memory(),
    Extra = [{scheduler, scheduler_time(State)},
             {ports, length(erlang:ports())},
             {maxports, case os:getenv("ERL_MAX_PORTS") of
                            false -> 1024;
                            MaxPortsS -> list_to_integer(MaxPortsS)
                        end},
             {etstabs, length(ets:all())},
             {maxetstabs, case os:getenv("ERL_MAX_ETS_TABLES") of
                              false -> 1400;
                              MaxEtsTabsS -> list_to_integer(MaxEtsTabsS)
                          end}],
    report_props(Extra ++ Mem ++ Stats ++ Info).

%%====================================================================
%% Internal functions
%%====================================================================

report_props(Info) ->
    IO = proplists:get_value(check_io, Info),
    [{rq, proplists:get_value(run_queue, Info)}
     ,{cswit, proplists:get_value(context_switches, Info)}
     ,{otp, proplists:get_value(otp_release, Info)}
     ,{procs, proplists:get_value(process_count, Info)}
     ,{maxprocs, proplists:get_value(process_limit, Info)}
     ,{ports, proplists:get_value(ports, Info)}
     ,{maxports, proplists:get_value(maxports, Info)}
     ,{maxfds, proplists:get_value(max_fds, IO)}
     ,{etstabs, proplists:get_value(etstabs, Info)}
     ,{maxetstabs, proplists:get_value(maxetstabs, Info)}
     ,{scheduler, proplists:get_value(scheduler, Info)}
     ,{memtot, proplists:get_value(total, Info)}
     ,{memproc, proplists:get_value(processes_used, Info)}
     ,{memets, proplists:get_value(ets, Info)}
     ,{membin, proplists:get_value(binary, Info)}
     ,{memcode, proplists:get_value(code, Info)}
    ].

report_string(Report) ->
   string:join([ [atom_to_list(K), "=", format_value(V)]
                  || {K, V} <- Report ],
                " ").

%% Given a list of current {Core, ActiveTime, TotalTime} scheduler
%% tuples and previous scheduler tuples, this function calculates the
%% diff between the two for each core, and then adds them up to give a
%% rough equivalent of how many cores have been busy.
scheduler_time(#state{prev_times=Prev, current_times=Current}) ->
    lists:foldl(
      fun({{I, A0, T0}, {I, A1, T1}}, Sum) -> Sum + (A1 - A0)/(T1 - T0) end,
      0, lists:zip(lists:sort(Prev), lists:sort(Current))).


format_value(L) when is_list(L) ->
    L;
format_value(F) when is_float(F) ->
    io_lib:format("~.2f",[F]);
format_value(B) when is_binary(B) ->
    B;
format_value(I) when is_integer(I) ->
    erlang:integer_to_list(I).
