-module(prop_lev_cin_fsm).

-compile([export_all]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("leviathan.hrl").

-define(APPS, [dobby, dobby_clib]).
-define(SERVER, lev_cin_fsm).
-define(SUBSCRIBER, subscriber).

-define(STATE, cin_fsm_state).
-define(CIN_ID, "cin1").
-define(CEN_IDS, ["cen1", "cen2"]).
-define(HOST_ID, "192.168.0.101").

-define(CALL(Args), {call, gen_fsm, send_event, [?SERVER | Args]}).
-define(STATUS_CHANGE(S),   {status_change,
                             list_to_binary(atom_to_list(S))}).

-record(state, {cin_id :: cin_id(),
                host_id :: host_id(),
                cen_ids :: [cen_id()]}).



%%%===================================================================
%%% Properties
%%%===================================================================

prop_lev_cen_fsm_works_fine() ->
    ?FORALL(
       Cmds,
       proper_fsm:commands(?MODULE),
       ?TRAPEXIT(       
          begin
              setup(),
              register(?SERVER,
                       element(2, lev_cin_fsm:start_link(?CIN_ID,
                                                         ?HOST_ID,
                                                         ?CEN_IDS))),
              {History, State, Result} = proper_fsm:run_commands(?MODULE,
                                                                 Cmds),
              ok = lev_cin_fsm:stop(?SERVER),
              ?WHENFAIL(
                 io:format("History: ~w~nState: ~w\nResult: ~w~n",
                           [History, State, Result]),
                 aggregate(zip(proper_fsm:state_names(History),
                               command_names(Cmds)),
                           Result =:= ok))
          end)).

%%%===================================================================
%%% Callbacks
%%%===================================================================

initial_state() ->
    importing.

initial_state_data() ->
    #state{cin_id = ?CIN_ID, host_id = ?HOST_ID, cen_ids = ?CEN_IDS}.

importing(_S) ->
    [
     transition(pending, [?STATUS_CHANGE(pending)])
    ].

pending(_S) ->
    [
     transition(preparing, [?STATUS_CHANGE(preparing)])
    ].

preparing(_S) ->
    [
     transition(ready, [?STATUS_CHANGE(ready)])
    ].

ready(_S) ->
    [
     transition(destroying, [?STATUS_CHANGE(destroying)])
    ].

destroying(_S) ->
    [
     transition(pending, [?STATUS_CHANGE(pending)])
    ].


precondition(_From, _Target, _StateData, {call,_,_,_}) ->
    true.

postcondition(importing, pending, StateData, _Call, _Result) ->
    #state{cin_id = CinId, host_id = HostId} = StateData,
    subscriber_got_message({imported, {CinId, HostId}});
postcondition(pending, preparing, StateData, _Call, _Result) ->
    #state{cin_id = CinId, host_id = HostId} = StateData,
    subscriber_got_message({prepared, {CinId, HostId}});
postcondition(ready, destroying, StateData, _Call, _Result) ->
    #state{cin_id = CinId, host_id = HostId} = StateData,
    subscriber_got_message({destroyed, {CinId, HostId}});
postcondition(From, Target, _, _, _) when From =/= Target ->
    true.

next_state_data(_From, _Target, StateData, _Result, _Call) ->
    StateData.

%%%===================================================================
%%% Helpers
%%%===================================================================

setup() ->
    teardown(),
    mock(),
    application:set_env(lager, handlers,
                        [{lager_console_backend, error}]),
    [{ok, _} = application:ensure_all_started(A) || A <- ?APPS],
    publish_cin(),
    {ok, _Pid} = proc_lib:start_link(?MODULE, subscriber, [self()]).
    

mock() ->
    meck:new(leviathan_cin),
    meck:expect(leviathan_cin, prepare, 1, ok),
    meck:expect(leviathan_cin, destroy, 1, ok).

publish_cin() ->
    ok = dby:publish(
           <<"publisher">>,
           {leviathan_dby:dby_cin_id(?CIN_ID),
            [
             {<<"type">>, <<"cin">>},
             {<<"status">>, <<"importing">>}
            ]},
           [persistent]).


teardown() ->
    meck:unload(),
    catch dby_db:clear(),
    case catch (?SUBSCRIBER ! {stop, self()}) of
        {'EXIT', _} ->
            ok;
        _ ->
            receive stopped -> ok end
    end,
    [application:stop(A) || A <- lists:reverse(?APPS)].


transition(NextState, Args) ->
    {NextState, ?CALL(Args)}.


subscriber(Parent) ->
    register(?SUBSCRIBER, Me = self()),
    Fn = fun(Msg) -> Me ! {lev_msg, Msg} end,
    SubId = leviathan_dby:subscribe_for_cin_message(?CIN_ID, Fn),
    proc_lib:init_ack(Parent, {ok, self()}),
    subscriber_loop(fun(Pid) ->
                            leviathan_dby:unsubscribe(SubId),
                            Pid ! stopped
                    end).

subscriber_loop(OnStop) ->
    receive
        {ex_msg, Pid, Ref, ExpectedMessage} ->
            receive {lev_msg, ExpectedMessage} -> Pid ! {ok, Ref} end,
            subscriber_loop(OnStop);
        {stop, Pid} ->
            OnStop(Pid)
    end.

subscriber_got_message(ExpectedMessage) ->
    ?SUBSCRIBER ! {ex_msg, self(), Ref = make_ref(), ExpectedMessage},
    receive {ok, Ref} -> true
    after 5000 -> false end.
