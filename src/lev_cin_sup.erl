-module(lev_cin_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_cin_fsm/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_cin_fsm(Node, Options) ->
    supervisor:start_child({?MODULE, Node}, Options).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {
       {simple_one_for_one, 5, 10},
       [?CHILD(lev_cin_fsm, worker, [])]
      }}.

