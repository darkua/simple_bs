-module(simple_bs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    simple_bs_sup:start_link().

stop(_State) ->
    ok.
