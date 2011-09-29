%% File : message_box_config.erl
%% Description : configuration manager for message_box

-module(message_box2_config).
-include_lib("eunit/include/eunit.hrl").
-export([load/1, get/1]).

get(Name) ->
    {ok, Value} = application:get_env(message_box2, Name),
    Value.

%% ===================================================================
%% @doc for test.
%% ===================================================================
load(File) ->
    case file:consult(File) of
        {ok, [{config, ConfigList}]} ->
	    read_config(message_box2, ConfigList);
	Other ->
	    ?debugVal(Other),
	    Other
    end.

read_config(AppName, ConfigList) ->
    case ConfigList of
	[] -> ok;
	[Config | Tail] ->
	    {Key, Val} = Config,
	    application:set_env(AppName, Key, Val),
	    read_config(AppName, Tail)
    end.
