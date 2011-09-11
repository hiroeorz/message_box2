%% File : home_db.erl
%% Description : users home time line database.

-module(mentions_db).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

-define(DBName, "mentions").

-export([init/1]).
-export([save_message_id/3, get_timeline/3, restore_table/2]).

%% for test
-export([insert_message_to_mysql/2]).

%%--------------------------------------------------------------------
%% @private
%%
%% @doc get message id list from local ets and sqlite3 database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(User::#user{}|integer()) -> {ok, Tid::tid()}).

init(UserId) when is_integer(UserId) ->
    {ok, User} = message_box2_user_db:lookup_id(UserId),
    init(User);

init(User) ->
    {ok, Tid} = create_tables(User),
    restore_table(User, Tid),
    {ok, Tid}.    

-spec(create_tables(User::#user{}) -> {ok, Tid::tid()}).

create_tables(User)->  
    Tid = ets:new(mentions_db, [ordered_set, {keypos, #message_index.id}]),
    ok = create_mysql_tables(User#user.id),
    {ok, Tid}.

-spec(create_mysql_tables(UserId::integer()) -> ok).

create_mysql_tables(UserId) ->
    TableName = mmysql:users_table(UserId, ?DBName),
    case lists:member(TableName, mmysql:users_table_list(UserId)) of
	true -> ok;
	false ->
            ?OK_PACKET = mmysql:execute("create table ~s (
                                            id INTEGER PRIMARY KEY,
                                            message_id INTEGER NOT NULL)",
                                        [TableName]),
            ok
    end.

-spec(restore_table(User::#user{}, Tid::tid()) -> ok).

restore_table(User, Tid) ->
    UserId = User#user.id,
    MessageMaxSizeOnMemory = 
        message_box2_config:get(message_max_size_on_memory),

    TableName = mmysql:users_table(UserId, ?DBName),
    SqlResults = mmysql:execute("select * from ~s
                                   order by id desc limit ~w",
                                [TableName, MessageMaxSizeOnMemory]),

    Records = parse_message_records(SqlResults),
    restore_records(Tid, Records).

-spec(restore_records(Tid::tid(), Records::[term()]) -> ok).

restore_records(Tid, Records) ->
    case Records of
	[] -> ok;
	[Record | Tail] ->
	    ets:insert(Tid, Record),
	    restore_records(Tid, Tail)
    end.

%%--------------------------------------------------------------------
%% @private
%%
%% @doc save message to database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_message_id(Tid::tid(), UserId::integer(), MessageId::integer()) -> 
             {ok, MessageId::integer()}).

save_message_id(Tid, UserId, MessageId) ->
    Id = get_max_id(UserId) - 1,
    MessageIndex = #message_index{id=Id, message_id=MessageId},

    MessageMaxSizeOnMemory = 
        message_box2_config:get(message_max_size_on_memory),

    ets:insert(Tid, MessageIndex),
    insert_message_to_mysql(UserId, MessageIndex),
    util:shurink_ets(Tid, MessageMaxSizeOnMemory),
    {ok, MessageId}.

%%--------------------------------------------------------------------
%% @private
%%
%% @doc get timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_timeline(Tid::tid(), UserId::integer(), Count::integer()) -> 
             [#message{}]).

get_timeline(Tid, UserId, Count) ->
    Pid = self(),
    MessageIds = get_timeline_ids(Tid, UserId, Count),

    Fun = fun(Id) ->
		  MessageIndex = case ets:lookup(Tid, Id) of
                                     [Index] -> Index;
                                     [] -> get_message_from_db(UserId, Id)
                                 end,

		  MessageId = MessageIndex#message_index.message_id,
		  
		  Result = 
		      case util:get_user_from_message_id(MessageId) of
			  {ok, FollowerUser} ->
			      m_user:get_message(FollowerUser#user.id, 
						 MessageId);
			  Other -> {error, {Other, {not_found, MessageId}}}
		      end,
		  Pid ! {message_reply, Result}
	  end,
    lists:map(fun(Id) -> spawn(fun() -> Fun(Id) end) end, MessageIds),
    MessageList = collect_loop(Pid, length(MessageIds), []),
    lists:sort(fun(A, B) -> A#message.datetime > B#message.datetime end, 
	       MessageList).


%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%%
%% @doc collect other users message
%%
%% @end
%%--------------------------------------------------------------------
collect_loop(Pid, Count, Result) ->
    if length(Result) >= Count -> Result;
       true ->
	    receive
		{message_reply, {ok, NewMessage}} ->
		    collect_loop(Pid, Count, [NewMessage | Result]);
		{message_reply, _Other} ->
		    collect_loop(Pid, Count, Result)
	    after 2000 -> Result
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%%
%% @doc get message from sqlite3
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_message_from_db(UserId::integer(), Id::integer()) -> [integer()]).

get_message_from_db(UserId, Id) ->
    TableName = mmysql:users_table(UserId, ?DBName),
    SqlResults = sqlite3:sql_exec("select * from ~s where id = ~w",
                                  [TableName, Id]),
    case SqlResults of
        [] ->
            {error, not_found};
        Records ->
            [Msg] = parse_message_records(Records),
            Msg
    end.

%%--------------------------------------------------------------------
%% @private
%%
%% @doc get message id list from local ets and mysql database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_timeline_ids(Tid::tid(), UserId::integer(), Count::integer()) ->
             [integer()]).

get_timeline_ids(Tid, UserId, Count) ->
    MessageIdsFromEts = 
	case ets:first(Tid) of
	    '$end_of_table' -> [];
	    First -> util:get_timeline_ids(Tid, Count, First, [First])
	end,

    if length(MessageIdsFromEts) < Count -> 
	    get_message_ids_from_db(UserId, Count, MessageIdsFromEts);
       true -> MessageIdsFromEts
    end.    

-spec(get_message_ids_from_db(UserId::integer(), Count::integer(), 
                              MessageIdsFromEts::[integer()]) ->
             [integer()]).

get_message_ids_from_db(_UserId, _Count, []) -> [];
get_message_ids_from_db(UserId, Count, MessageIdsFromEts) ->
    TableName = mmysql:users_table(UserId, ?DBName),
    SqlResult = 
        mmysql:execute("select * from ~s
                          where id > ~w
                          order by id limit ~w",
                      [TableName, 
                       lists:max(MessageIdsFromEts),
                       Count - length(MessageIdsFromEts)]),

    Ids = lists:map(fun(Record) -> Record#message_index.id end,
                    parse_message_records(SqlResult)),

    MessageIdsFromEts ++ Ids.    

-spec(get_max_id(UserId::integer()) -> integer()).

get_max_id(UserId) ->
    TableName = mmysql:users_table(UserId, ?DBName),
    Result = mmysql:execute("select * from ~s order by id limit 1",
                           [TableName]),
    case parse_message_records(Result) of
	[] -> 0;
	[LastRecord] -> LastRecord#message_index.id
    end.

-spec(parse_message_records(Result::[term()]) -> [#message{}]).

parse_message_records(Result) ->    
    parse_message_records(Result#result_packet.rows, []).

parse_message_records(RowList, RecordList) ->
    case RowList of
	[] -> lists:reverse(RecordList);
	[Row | Tail] -> 
	    [Id, MsgId] = Row,
	    Record = #message_index{id = Id, message_id = MsgId},
	    parse_message_records(Tail, [Record | RecordList])
    end.

insert_message_to_mysql(UserId, MessageIndex) ->
    TableName = mmysql:users_table(UserId, ?DBName),
    ?OK_PACKET = mmysql:execute("insert into ~s (id, message_id)
                                   values(~w, ~w)",
                                [TableName,
                                 MessageIndex#message_index.id,
                                 MessageIndex#message_index.message_id]),
    ok.
