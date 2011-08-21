-module(message_box2).

-export([start/0, stop/0]).

%%--------------------------------------------------------------------
%% @doc
%% Boot file_box system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() -> ok).

start() ->
    application:start(?MODULE),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Shut down file_box system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> ok).

stop() ->
    application:stop(?MODULE),
    ok.
