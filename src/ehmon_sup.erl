%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ehmon top level supervisor
%% @end
%%%-------------------------------------------------------------------
-module(ehmon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ReportSrv = [{ehmon_report_srv,
                  {ehmon_report_srv, start_link, []},
                  permanent, 2000, worker, [ehmon_report_srv]}],
    Children = append_shh_reporter(ReportSrv),
    {ok, { {one_for_one, 1, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================

append_shh_reporter(Children) ->
    AppConfig = ehmon_app:config(report_mf, {ehmon, stdout_report}),
    append_shh_reporter(AppConfig, Children).

append_shh_reporter({ehmon_shh_report, _}, Children) ->
    Children ++ [{ehmon_shh_report,
                  {ehmon_shh_report, start_link, []},
                  permanent, 2000, worker, [ehmon_report_srv]}];
append_shh_reporter({ehmon, _}, Children) ->
    Children.

