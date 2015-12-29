-module(lev_cen_fsm).
-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% CEN Statuses
-export([importing/2, pending/2, preparing/2, ready/2, destroying/2]).

%% ------------------------------------------------------------------
%% Types & Records & Macros & Includes
%% ------------------------------------------------------------------

-type cen_id() :: string().
-type host_id() :: string().

-record(state, {id :: cen_id(),
                host_id ::  host_id(),
                subscription :: subscription_id()}).

-define(SERVER, self()).
-define(INIT_CEN_STATUS, <<"importing">>).

-include_lib("dobby_clib/include/dobby.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CenId, HostId) ->
    gen_fsm:start_link(?MODULE, [CenId, HostId], []).


%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([CenId, HostId]) ->
    DeliveryFn = delivery_fun(status_change),
    SubId = make_subscription(CenId, ?INIT_CEN_STATUS, DeliveryFn),
    State = #state{id = CenId, host_id = HostId, subscription = SubId},
    send_cen_message(imported, State),
    {ok, importing, State}.
     

importing({status_change, <<"pending">>}, State) ->
    {next_state, pending, State}.
    
pending({status_change, <<"preparing">>}, State) ->
    leviathan_cen:prepare([State#state.id]),
    send_cen_message(prepared, State),
    {next_state, preparing, State}.

preparing({status_change, <<"ready">>}, State) ->
    {next_state, ready, State}.

ready({status_change, <<"destroying">>}, State) ->
    leviathan_cen:destroy([State#state.id]),
    send_cen_message(destroyed, State),
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

terminate(_Reason, _StateName, #state{subscription = SubId}) ->
    leviathan_dby:unsubscribe(SubId),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

make_subscription(CenId, CurrentStatus, DeliveryFn) ->
    {CurrentStatus, SubId} =
        leviathan_dby:subscribe_for_cen_status_change(CenId,
                                                      DeliveryFn),
    SubId.

send_cen_message(Type, #state{id = CenId, host_id = HostId}) ->
    leviathan_dby:send_cen_message(CenId, {Type, {CenId, HostId}}).

delivery_fun(Event) ->
    FsmRef = ?SERVER,
    fun(Status) ->
            gen_fsm:send_event(FsmRef, {Event, Status})
    end.
    
