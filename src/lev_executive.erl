-module(lev_executive).
-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% CENs API
-export([import_cens/1, make_cens/1, destroy_cens/1]).
%% CINs API
-export([import_cins/1, make_cins/1, destroy_cins/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, idle/3, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

%% CEN states
-export([cen_import/2, cen_prepare/2, cen_destroy/2]).
%% CIN states
-export([cin_import/2, cin_prepare/2, cin_destroy/2]).


%% ------------------------------------------------------------------
%% Types & Records & Macros & Includes
%% ------------------------------------------------------------------

-type cen_id() :: string().
-type host_id() :: string().

-record(state, {
          host_to_node :: #{host_id() => node()},
          subscriptions :: #{cen_id() =>
                                 {[host_id()], subscription_id()}},
          reply_to :: pid()}).

-define(SERVER, ?MODULE).

-include_lib("dobby_clib/include/dobby.hrl").
-include_lib("kernel/include/inet.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

import_cens(JsonBin) ->
    gen_fsm:sync_send_event(?SERVER, {import_cens, JsonBin}, 10000).

make_cens(CenIds) ->
    gen_fsm:sync_send_event(?SERVER, {make_cens, CenIds}, 10000).

destroy_cens(CenIds) ->
    gen_fsm:sync_send_event(?SERVER, {destroy_cens, CenIds}, 10000).

import_cins(CinToCensBin) ->
    gen_fsm:sync_send_event(?SERVER, {import_cins, CinToCensBin}, 10000).

make_cins(CinIds) ->
    gen_fsm:sync_send_event(?SERVER, {make_cins, CinIds}, 10000).

destroy_cins(CinIds) ->
    gen_fsm:sync_send_event(?SERVER, {destroy_cins, CinIds}, 10000).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, idle, #state{host_to_node = host_to_node()}}.

%% CENs
idle({import_cens, JsonBin}, From, State) ->
    gen_fsm:send_event(?SERVER, {import, build_cen_lm(JsonBin)}),
    {next_state, cen_import, State#state{reply_to = From}};
idle({make_cens, CenIds}, From, State) ->
    gen_fsm:send_event(?SERVER, {prepare, CenIds}),
    {next_state, cen_prepare, State#state{reply_to = From}};
idle({destroy_cens, CenIds}, From, State) ->
    gen_fsm:send_event(?SERVER, {destroy, CenIds}),
    {next_state, cen_destroy, State#state{reply_to = From}};
%% CINs
idle({import_cins, CinToCensBin}, From, State) ->
    gen_fsm:send_event(?SERVER, {import, build_cin_lm(CinToCensBin)}),
    {next_state, cin_import, State#state{reply_to = From}};
idle({make_cins, CenIds}, From, State) ->
    gen_fsm:send_event(?SERVER, {prepare, CenIds}),
    {next_state, cin_prepare, State#state{reply_to = From}};
idle({destroy_cins, CenIds}, From, State) ->
    gen_fsm:send_event(?SERVER, {destroy, CenIds}),
    {next_state, cin_destroy, State#state{reply_to = From}};
%% Unimplemented
idle(_, _From, State) ->
    {reply, not_implemented, idle, State}.

cen_import({import, CenLM}, State) ->
    import_cens_into_dobby(CenLM),
    import_cens_into_cluster_stores(CenLM),
    CenToHost = cen_to_host_map(CenLM),
    Subs = make_cen_subscriptions(CenToHost,
                                  delivery_fun(imported),
                                  cen_done_fun(pending)),
    start_cens_fsms(CenToHost, State#state.host_to_node),
    {next_state, cen_import, State#state{subscriptions = Subs}};
cen_import({imported, Key}, State) ->
    case handle_subscription_event(Key, State) of
        {finished, NewState} ->
            lager:info("cen_import: CENs imported"),
            {next_state, idle,  NewState};
        {continue, NewState} ->
            {next_state, cen_import, NewState}
    end.

cen_prepare({prepare, CenIds}, State) ->
    CenToHost = cen_to_host_map(CenIds),
    Subs = make_cen_subscriptions(CenToHost,
                                  delivery_fun(prepared),
                                  cen_done_fun(ready)),
    set_cens_status(CenIds, preparing),
    {next_state, cen_prepare, State#state{subscriptions = Subs}};
cen_prepare({prepared, Key}, State) ->
    case handle_subscription_event(Key, State) of
        {finished, NewState} ->
            lager:info("cen_prepare: CENs prepared"),
            {next_state, idle, NewState};
        {continue, NewState} ->
            {next_state, cen_prepare, NewState}
    end.

cen_destroy({destroy, CenIds}, State) ->
    CenToHost = cen_to_host_map(CenIds),
    Subs = make_cen_subscriptions(CenToHost,
                                  delivery_fun(destroyed),
                                  cen_done_fun(pending)),
    set_cens_status(CenIds, destroying),
    {next_state, cen_destroy, State#state{subscriptions = Subs}};
cen_destroy({destroyed, Key}, State) ->
    case handle_subscription_event(Key, State) of
        {finished, NewState} ->
            lager:info("cen_destroy: CENs destroyed"),
            {next_state, idle, NewState};
        {continue, NewState} ->
            {next_state, cen_destroy, NewState}
    end.

cin_import({import, CinLM}, State) ->
    import_cins_into_dobby(CinLM),
    import_cins_into_cluster_stores(CinLM),
    CinToHost = cin_to_host_map(CinLM),
    CinToCen = cin_to_cen_map(CinLM),
    Subs = make_cin_subscriptions(CinToHost,
                                  delivery_fun(imported),
                                  cin_done_fun(pending)),
    start_cins_fsms(CinToHost, CinToCen, State#state.host_to_node),
    {next_state, cin_import, State#state{subscriptions = Subs}};
cin_import({imported, Key}, State) ->
    case handle_subscription_event(Key, State) of
        {finished, NewState} ->
            lager:info("cin_import: CINs imported"),
            {next_state, idle, NewState};
        {continue, NewState} ->
            {next_state, cin_import, NewState}
    end.

cin_prepare({prepare, CinIds}, State) ->
    CinToHost = cin_to_host_map(CinIds),
    Subs = make_cin_subscriptions(CinToHost,
                                  delivery_fun(prepared),
                                  cin_done_fun(ready)),
    set_cins_status(CinIds, preparing),
    {next_state, cin_prepare, State#state{subscriptions = Subs}};
cin_prepare({prepared, Key}, State) ->
    case handle_subscription_event(Key, State) of
        {finished, NewState} ->
            lager:info("cin_prepare: CINs prepared"),
            {next_state, idle, NewState};
        {continue, NewState} ->
            {next_state, cin_prepare, NewState}
    end.

cin_destroy({destroy, CinIds}, State) ->
    CinToHost = cin_to_host_map(CinIds),
    Subs = make_cin_subscriptions(CinToHost,
                                  delivery_fun(destroyed),
                                  cin_done_fun(pending)),
    set_cins_status(CinIds, destroying),
    {next_state, cin_destroy, State#state{subscriptions = Subs}};
cin_destroy({destroyed, Key}, State) ->
    case handle_subscription_event(Key, State) of
        {finished, NewState} ->
            lager:info("cin_destroy: CINs destroyed"),
            {next_state, idle, NewState};
        {continue, NewState} ->
            {next_state, cin_destroy, NewState}
    end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({NodeUpOrDown, Node}, StateName, State) ->
    lager:info("~p: ~p~n", [Node, NodeUpOrDown]),
    {next_state, StateName, State#state{host_to_node = host_to_node()}};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{subscriptions = Subs}) ->
    maps:fold(
      fun(_CenId, HostToSub, _) ->
              [leviathan_dby:unsubscribe(S)
               || S <- maps:values(HostToSub)]
      end, undefined, Subs),
    net_kernel:monitor_nodes(false).

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: CENs
%% ------------------------------------------------------------------

build_cen_lm(JsonBin) ->
    leviathan_cen:decode_binary(JsonBin).

import_cens_into_dobby(CenLM) ->
    ok = leviathan_dby:import_cens(<<"HOST">>, CenLM).

import_cens_into_cluster_stores(CenLM) ->
    [ok = rpc:call(N, leviathan_cen_store, import_cens,
                   [<<"HOST">>, CenLM]) || N <- [node() | nodes()]].

%% TODO: CenToHost should be build based on Dobby    
cen_to_host_map(CenIds) when is_list(CenIds) ->
    CenLM = leviathan_cen_store:get_levmap(CenIds),
    leviathan_cen:map_cen_id_to_host(CenLM);
cen_to_host_map(CenLM) ->
    leviathan_cen:map_cen_id_to_host(CenLM).

make_cen_subscriptions(CenToHost, DeliveryFn, CenDoneFn) ->
    maps:fold(
      fun(CenId, Hosts, Acc) ->
              Sub = leviathan_dby:subscribe_for_cen_message(
                      CenId,
                      DeliveryFn),
              SubMap = subscription_map(Hosts, Sub, CenDoneFn),
              maps:put(CenId, SubMap, Acc)
      end, #{}, CenToHost).

cen_done_fun(NextCenStatus) ->
    fun(Id) -> set_cens_status([Id], NextCenStatus) end.

%% TODO: CEN FSMs should be started based on occurence of new CEN
%% identifier in Dobby.
start_cens_fsms(CenToHost, HostToNode) ->
    maps:fold(fun(CenId, Hosts, _) ->
                      start_cen_fsms(CenId, Hosts, HostToNode)
              end, undefined, CenToHost),
    ok.

start_cen_fsms(CenId, Hosts, HostToNode) ->
    lists:foreach(
      fun(HostId) ->
              Node = maps:get(HostId, HostToNode),
              {ok, _}  = lev_cen_sup:add_cen_fsm(Node, [CenId, HostId]),
              lager:info("{~p, ~p}: waiting", [CenId, Node])
      end, Hosts).

set_cens_status(CenIds, Status) ->
    lists:foreach(
      fun(Id) ->
              leviathan_dby:set_cen_status(Id, Status)
      end, CenIds).

%% ------------------------------------------------------------------
%% Internal Function Definitions: CINs
%% ------------------------------------------------------------------

build_cin_lm(CinsToCensBin) ->
    leviathan_cin:decode_binary(CinsToCensBin).

import_cins_into_dobby(CinLM) ->
    ok = leviathan_dby:import_cins(<<"HOST">>, CinLM).

import_cins_into_cluster_stores(CinLM) ->
    [ok = rpc:call(N, leviathan_cin_store, import_cins,
                   [<<"HOST">>, CinLM]) || N <- [node() | nodes()]].

%% TODO: CinToHost should be built based on Dobby
cin_to_host_map(CinIds) when is_list(CinIds) ->
    CinLM = leviathan_cin_store:get_levmap(CinIds),
    leviathan_cin:map_cin_id_to_host(CinLM);
cin_to_host_map(CinLM) ->
    leviathan_cin:map_cin_id_to_host(CinLM).

%% TODO: CinToCen should be built based on Dobby
cin_to_cen_map(CinLM) ->
    leviathan_cin:map_cin_id_to_cen(CinLM).

make_cin_subscriptions(CinToHost, DeliveryFn, CinDoneFn) ->
    maps:fold(
      fun(CinId, Hosts, Acc) ->
              Sub = leviathan_dby:subscribe_for_cin_message(
                      CinId,
                      DeliveryFn),
              SubMap = subscription_map(Hosts, Sub, CinDoneFn),
              maps:put(CinId, SubMap, Acc)
      end, #{}, CinToHost).

%% TODO: CIN FSMs should be started based on occurence of new CEN
%% identifier in Dobby.
start_cins_fsms(CinToHost, CinToCen, HostToNode) ->
    maps:fold(fun(CinId, Hosts, _) ->
                      CenIds = maps:get(CinId, CinToCen),
                      start_cin_fsms(CinId, Hosts, CenIds, HostToNode)
              end, undefined, CinToHost),
    ok.

start_cin_fsms(CinId, Hosts, CenIds, HostToNode) ->
    lists:foreach(
      fun(HostId) ->
              Node = maps:get(HostId, HostToNode),
              {ok, _}  = lev_cin_sup:add_cin_fsm(Node, [CinId,
                                                        HostId,
                                                        CenIds]),
              lager:info("{~p, ~p}: waiting", [CinId, Node])
      end, Hosts).


set_cins_status(CinIds, Status) ->
    lists:foreach(
      fun(Id) ->
              leviathan_dby:set_cin_status(Id, Status)
      end, CinIds).

cin_done_fun(NextCenStatus) ->
    fun(Id) -> set_cins_status([Id], NextCenStatus) end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Other
%% ------------------------------------------------------------------

handle_subscription_event(Key, #state{subscriptions = Subs0} = State) ->
    case  handle_subscription_event2(Key, Subs0) of
        Subs1 when map_size(Subs1) == 0 ->
            gen_fsm:reply(State#state.reply_to, ok),
            {finished,
             State#state{subscriptions = Subs1, reply_to = undefined}};
        Subs1 ->
            {continue, State#state{subscriptions = Subs1}}
    end.

handle_subscription_event2({CenOrCinId, HostId}, Subs) ->
    case maps:get(CenOrCinId, Subs) of
        #{hosts := [HostId], sub_id := SubId, on_finish := FinishFn} ->
            ok = leviathan_dby:unsubscribe(SubId),
            FinishFn(CenOrCinId),
            maps:remove(CenOrCinId, Subs);
        #{hosts := Hosts0} = SubMap ->
            Hosts1 = lists:delete(HostId, Hosts0),
            maps:put(CenOrCinId, maps:put(hosts, Hosts1, SubMap), Subs)
    end.


delivery_fun(Event) ->
    Node = node(),
    fun({E, {CenOrCinId, HostId}}) when E =:= Event ->
            ok = gen_fsm:send_event({?SERVER, Node},
                                    {E, {CenOrCinId, HostId}});
       (_) ->
            ok
    end.

subscription_map(Hosts, SubId, OnFinishFn) ->
    #{hosts => Hosts, sub_id => SubId, on_finish => OnFinishFn}.

host_to_node() ->
    host_to_node([node()| nodes()]).

host_to_node(Nodes) ->
    L = [begin
             [_, HostName] = string:tokens(atom_to_list(N), "@"),
             {ok, #hostent{h_addr_list = Ips}} =
                 inet:gethostbyname(HostName),
             [{inet:ntoa(Ip), N} || Ip <- Ips]
         end || N <- Nodes],
    maps:from_list(lists:flatten(L)).
