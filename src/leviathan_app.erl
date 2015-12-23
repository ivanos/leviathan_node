-module(leviathan_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    setup_dobby(),
    leviathan_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

setup_dobby() ->
    case application:get_env(leviathan, master_node, undefined) of
        N when N == undefined orelse N == node() ->
            %% we are the master => start dobby
            {ok ,_ } = application:ensure_all_started(dobby);
        MasterNode ->
            true = net_kernel:connect(MasterNode),
            ok = global:sync()
    end.

