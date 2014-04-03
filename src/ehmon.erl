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

%%====================================================================
%% API
%%====================================================================

init() ->
    {undefined, erlang:statistics(scheduler_wall_time)}.

update({_, SchedulerTimes}) ->
    {SchedulerTimes, erlang:statistics(scheduler_wall_time)}.

info_report(Iolist) ->
    error_logger:info_msg("ehmon_report ~s~n", [Iolist]).

stdout_report(Iolist) ->
    io:format(standard_io, "ehmon_report ~s~n", [Iolist]).

-spec report({[OldSchedulerTimes::integer()],
              [NewSchedulerTimes::integer()]}) -> iolist().
report(SchedulerTimes) ->
    Stats = [{context_switches,
              element(1, erlang:statistics(context_switches))},
              {run_queue, erlang:statistics(run_queue)}],
    Info = [{K, erlang:system_info(K)} ||
               K <- [check_io, otp_release, process_count, process_limit] ],
    Mem = erlang:memory(),
    Extra = [{scheduler, scheduler_time(SchedulerTimes)},
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
    report_string(Extra ++ Mem ++ Stats ++ Info).

%%====================================================================
%% Internal functions
%%====================================================================

report_string(Info) ->
    IO = proplists:get_value(check_io, Info),
    Items =
        [{"rq", get_value(run_queue, Info)}
         ,{"cswit", get_value(context_switches, Info)}
         ,{"otp", get_value(otp_release, Info)}
         ,{"procs", get_value(process_count, Info)}
         ,{"maxprocs", get_value(process_limit, Info)}
         ,{"ports", get_value(ports, Info)}
         ,{"maxports", get_value(maxports, Info)}
         ,{"maxfds", get_value(max_fds, IO)}
         ,{"etstabs", get_value(etstabs, Info)}
         ,{"maxetstabs", get_value(maxetstabs, Info)}
         ,{"scheduler", get_value(scheduler, Info)}
         ,{"memtot", get_value(total, Info)}
         ,{"memproc", get_value(processes_used, Info)}
         ,{"memets", get_value(ets, Info)}
         ,{"membin", get_value(binary, Info)}
         ,{"memcode", get_value(code, Info)}
        ],
    string:join([ [K, "=", V]
                  || {K, V} <- Items ],
                " ").

%% Given a list of current {Core, ActiveTime, TotalTime} scheduler
%% tuples and previous scheduler tuples, this function calculates the
%% diff between the two for each core, and then adds them up to give a
%% rough equivalent of how many cores have been busy.
scheduler_time({LastSchedulerTimes, CurrentSchedulerTimes}) ->
    lists:foldl(
      fun({{I, A0, T0}, {I, A1, T1}}, Sum) -> Sum + (A1 - A0)/(T1 - T0) end,
      0, lists:zip(lists:sort(LastSchedulerTimes),
                   lists:sort(CurrentSchedulerTimes))).


get_value(K, List) ->
    case proplists:get_value(K, List, "unknown") of
        L when is_list(L) -> L;
        F when is_float(F) -> io_lib:format("~.2f",[F]);
        B when is_binary(B) -> B;
        I when is_integer(I) -> erlang:integer_to_list(I)
    end.

%% Mem ++ Stats ++ Info :
%% [
%%  {total,16505112},
%%  {processes,9562622},
%%  {processes_used,9562622},
%%  {system,6942490},
%%  {atom,194289},
%%  {atom_used,173407},
%%  {binary,270616},
%%  {code,3713062},
%%  {ets,308904},
%%  {context_switches,1898},
%%  {garbage_collection,[{"number_of_gcs",511},
%%                       {"words_reclaimed",1065648}]},
%%  {io,[{input,4039278},{output,1071732}]},
%%  {reductions,[{"total_reductions",574475},
%%               {"reductions_since_last_call",574475}]},
%%  {run_queue,0},
%%  {runtime,[{"total_run_time",230},
%%            {"time_since_last_call",230}]},
%%  {wall_clock,[{"Total_Wall_Clock_time",588637},
%%               {"wall_clock_time_since_last_call",588637}]},
%%  {allocated_areas,[{sys_misc,24622},
%%                    {static,991232},
%%                    {atom_space,[98328,77446]},
%%                    {atom_table,95961},
%%                    {module_table,9084},
%%                    {export_table,50316},
%%                    {export_list,259104},
%%                    {register_table,276},
%%                    {fun_table,3266},
%%                    {module_refs,2048},
%%                    {loaded_code,3389244},
%%                    {dist_table,619},
%%                    {node_table,227},
%%                    {bits_bufs_size,0},
%%                    {bif_timer,80200},
%%                    {link_lh,0},
%%                    {process_table,262144},
%%                    {ets_misc,58024}]},
%%  {build_type,opt},
%%  {c_compiler_used,[{compiler,gnuc},{version,<<"4.2.1">>}]},
%%  {check_io,[{name,erts_poll},
%%             {primary,select},
%%             {fallback,false},
%%             {kernel_poll,false},
%%             {memory_size,6128},
%%             {total_poll_set_size,2},
%%             {lazy_updates,true},
%%             {pending_updates,0},
%%             {batch_updates,false},
%%             {concurrent_updates,false},
%%             {max_fds,256}]},
%%  {compat_rel,15},
%%  {cpu_topology,undefined},
%%  {creation,0},
%%  {debug_compiled,false},
%%  {dist,<<"=node:'nonode@nohost'\n=no_distribution\n">>},
%%  {dist_ctrl,[]},
%%  {driver_version,<<"2.0">>},
%%  {elib_malloc,false},
%%  {dist_buf_busy_limit,1048576},
%%  {garbage_collection,[{min_bin_vheap_size,46368},
%%                       {min_heap_size,233},
%%                       {fullsweep_after,65535}]},
%%  {global_heaps_size,0},
%%  {heap_type,private},
%%  {kernel_poll,false},
%%  {logical_processors,4},
%%  {logical_processors_available,unknown},
%%  {logical_processors_online,4},
%%  {machine,<<"BEAM">>},
%%  {modified_timing_level,undefined},
%%  {multi_scheduling,enabled},
%%  {multi_scheduling_blockers,[]},
%%  {otp_release,<<"R15B01">>},
%%  {process_count,28},
%%  {process_limit,32768},
%%  {scheduler_bind_type,unbound},
%%  {scheduler_bindings,[unbound,unbound,unbound,unbound]},
%%  {scheduler_id,3},
%%  {schedulers,4},
%%  {schedulers_online,4},
%%  {smp_support,true},
%%  {system_version,<<"Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]\n">>},
%%  {system_architecture,<<"i386-apple-darwin11.3.0">>},
%%  {threads,true},
%%  {thread_pool_size,0},
%%  {trace_control_word,0},
%%  {update_cpu_info,unchanged},
%%  {version,<<"5.9.1">>},
%%  {wordsize,8}]
