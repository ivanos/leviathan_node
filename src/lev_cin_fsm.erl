-module(lev_cin_fsm).
-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, stop/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% CIN Statuses
-export([importing/2, pending/2, preparing/2, ready/2, destroying/2]).

%% ------------------------------------------------------------------
%% Types & Records & Macros & Includes
%% ------------------------------------------------------------------



-record(cin_fsm_state, {id :: cin_id(),
                        cen_ids :: [cen_id()],
                        host_id ::  host_id(),
                        subscription :: subscription_id()}).

-define(SERVER, self()).
-define(INIT_CIN_STATUS, <<"importing">>).
-define(STATE, cin_fsm_state).

-include("leviathan.hrl").
-include_lib("dobby_clib/include/dobby.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CinId, HostId, CinIds) ->
    gen_fsm:start_link(?MODULE, [CinId, HostId, CinIds], []).

stop(FsmRef) ->
    gen_fsm:stop(FsmRef).


%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([CinId, HostId, CinIds]) ->
    DeliveryFn = delivery_fun(status_change),
    SubId = make_subscription(CinId, ?INIT_CIN_STATUS, DeliveryFn),
    State = #?STATE{id = CinId,
                    host_id = HostId,
                    cen_ids = CinIds,
                    subscription = SubId},
    send_cin_message(imported, State),
    {ok, importing, State}.
     

importing({status_change, <<"pending">>}, State) ->
    {next_state, pending, State}.
    
pending({status_change, <<"preparing">>}, State) ->
    leviathan_cin:prepare([State#?STATE.id]),
    send_cin_message(prepared, State),
    {next_state, preparing, State}.

preparing({status_change, <<"ready">>}, State) ->
    {next_state, ready, State}.

ready({status_change, <<"destroying">>}, State) ->
    leviathan_cin:destroy([State#?STATE.id]),
    send_cin_message(destroyed, State),
    {next_state, destroying, State}.

destroying({status_change, <<"pending">>}, State) ->
    {next_state, pending, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #?STATE{subscription = SubId}) ->
    leviathan_dby:unsubscribe(SubId),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

make_subscription(CinId, CurrentStatus, DeliveryFn) ->
    {CurrentStatus, SubId} =
        leviathan_dby:subscribe_for_cin_status_change(CinId,
                                                      DeliveryFn),
    SubId.

send_cin_message(Type, #?STATE{id = CinId, host_id = HostId}) ->
    leviathan_dby:send_cin_message(CinId, {Type, {CinId, HostId}}).

delivery_fun(Event) ->
    FsmRef = ?SERVER,
    fun(Status) ->
            gen_fsm:send_event(FsmRef, {Event, Status})
    end.
    
