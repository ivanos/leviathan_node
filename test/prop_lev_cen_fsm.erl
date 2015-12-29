-module(prop_lev_cen_fsm).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("dobby_clib/include/dobby.hrl").


-define(APPS, [dobby, dobby_clib]).

-type cen_id() :: string().
-type host_id() :: string().

-record(state, {id :: cen_id(),
                host_id ::  host_id(),
                subscription :: subscription_id()}).

%%%===================================================================
%%% Properties
%%%===================================================================

%%%===================================================================
%%% Callbacks
%%%===================================================================

initial_state() ->
    importing.

initial_state_data() ->
    #state{id = "cen_id", host_id = "host_id"}.

%%%===================================================================
%%% Helpers
%%%===================================================================


    
