-module(message_box2_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("user.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    message_box2_config:load(),

    case node() of
	nonode@nohost -> ok;
	Node -> 
	    Cookie = message_box2_config:get(cookie),
	    erlang:set_cookie(Node, Cookie),
            NodeList = message_box2_config:get(node_list),
            connect_to_nodes(NodeList)
    end,

    mnesia:create_schema([node()]), ?debugVal("mnesia create schema."),
    mnesia:start(),                 ?debugVal("mnesia started."),
    create_tables(disc_copies),     ?debugVal("mnesia create tables."),
    mmysql:init(),                  ?debugVal("mysql init."),
    Reply = message_box2_sup:start_link(),
    spawn_link(fun() -> m_user_sup:start_all_users() end),
    Reply.

stop(_State) ->
    ok.

connect_to_nodes(NodeList) ->
    case NodeList of
        [] ->
            receive
            after 2000 -> ok
            end,
            io:format("connected nodes:~p~n", [nodes()]),
            ok;
        [Node | Tail] ->
            net_kernel:connect(Node),
            connect_to_nodes(Tail)
    end.

create_tables(CopyType) ->
    mnesia:create_table(user, [{CopyType, [node()]}, 
                               {type, set},
                               {attributes, record_info(fields, user)}]),

    mnesia:create_table(follow, [{CopyType, [node()]}, 
                                 {type, set},
                                 {attributes, record_info(fields, follow)},
                                 {index, [id]}]).
