%% File : mentions_db.erl
%% Description : users mentions time line database.

-module(mentions_db).
-include_lib("eunit/include/eunit.hrl").
-include("../include/message_box.hrl").
-include("../include/message.hrl").
-include("../include/user.hrl").

-export([init/1]).
-export([save_message_id/3, get_timeline/3]).

%% for test
-export([insert_message_to_sqlite3/2]).

%%--------------------------------------------------------------------
%% @private
%%
%% @doc get message id list from local ets and sqlite3 database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(DBPid::pid()) -> {ok, Tid::tid()}).

init(DBPid)->
    {ok, Tid} = create_tables(DBPid),
    restore_table(DBPid, Tid),
    {ok, Tid}.    

-spec(create_tables(DBPid::pid()) -> {ok, Tid::tid()}).

create_tables(DBPid)->  
    Tid = ets:new(mentions_db, [ordered_set, {keypos, #message_index.id}]),
    ok = create_sqlite3_tables(DBPid),
    {ok, Tid}.

-spec(create_sqlite3_tables(DBPid::pid()) -> ok).

create_sqlite3_tables(DBPid) ->
    case lists:member(mentions, sqlite3:list_tables(DBPid)) of
	true -> ok;
	false ->
	    sqlite3:sql_exec(DBPid, 
			     "create table mentions (
                                id INTEGER PRIMARY KEY,
                                message_id INTEGER NOT NULL)")
    end.

-spec(restore_table(DBPid::pid(), Tid::tid()) -> ok).

restore_table(DBPid, Tid)->
    MessageMaxSizeOnMemory = 
        message_box2_config:get(message_max_size_on_memory),

    SqlResults = sqlite3:sql_exec(DBPid,
				  "select * from mentions
                                     order by id desc limit :max",
                                 [{':max', MessageMaxSizeOnMemory}]),
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
-spec(save_message_id(Tid::tid(), DBPid::pid(), MessageId::integer()) -> 
             {ok, MessageId::integer()}).

save_message_id(Tid, DBPid, MessageId) ->
    Id = get_max_id(DBPid) - 1,
    MessageIndex = #message_index{id=Id, message_id=MessageId},

    MessageMaxSizeOnMemory = 
        message_box2_config:get(message_max_size_on_memory),

    ets:insert(Tid, MessageIndex),
    insert_message_to_sqlite3(DBPid, MessageIndex),
    util:shurink_ets(Tid, MessageMaxSizeOnMemory),
    {ok, MessageId}.

%%--------------------------------------------------------------------
%% @private
%%
%% @doc get timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_timeline(Tid::tid(), DBPid::pid(), Count::integer()) -> [#message{}]).

get_timeline(Tid, DBPid, Count) ->
    Pid = self(),
    MessageIds = get_timeline_ids(Tid, DBPid, Count),

    Fun = fun(Id) ->
		  MessageIndex = case ets:lookup(Tid, Id) of
                                     [Index] -> Index;
                                     [] -> get_message_from_db(DBPid, Id)
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
-spec(get_message_from_db(DBPid::pid(), Id::integer()) -> [integer()]).

get_message_from_db(DBPid, Id) ->
    SqlResults = sqlite3:sql_exec(DBPid, 
                                  "select * from mentions where id = :id",
                                  [{':id', Id}]),
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
%% @doc get message id list from local ets and sqlite3 database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_timeline_ids(Tid::tid(), DBPid::pid(), Count::integer()) ->
             [integer()]).

get_timeline_ids(Tid, DBPid, Count) ->
    MessageIdsFromEts = 
	case ets:first(Tid) of
	    '$end_of_table' -> [];
	    First -> util:get_timeline_ids(Tid, Count, First, [First])
	end,

    if length(MessageIdsFromEts) < Count -> 
	    get_message_ids_from_db(DBPid, Count, MessageIdsFromEts);
       true -> MessageIdsFromEts
    end.    

-spec(get_message_ids_from_db(DBPid::pid(), Count::integer(), 
                              MessageIdsFromEts::[integer()]) ->
             [integer()]).

get_message_ids_from_db(_DBPid, _Count, []) -> [];
get_message_ids_from_db(DBPid, Count, MessageIdsFromEts) ->
	    SqlResult = 
		sqlite3:sql_exec(DBPid, "select * from mentions
                                           where id > :id
                                           order by id limit :limit",
				 [{':id', lists:max(MessageIdsFromEts)},
				  {':limit', 
				   Count - length(MessageIdsFromEts)}]),
	    
	    Ids = lists:map(fun(Record) -> Record#message_index.id end,
			    parse_message_records(SqlResult)),
	    MessageIdsFromEts ++ Ids.    

-spec(get_max_id(DBPid::pid()) -> integer()).

get_max_id(DBPid) ->
    Result = sqlite3:sql_exec(DBPid, "select * from mentions 
                                           order by id limit 1"),
    case parse_message_records(Result) of
	[] -> 0;
	[LastRecord] -> LastRecord#message_index.id
    end.

-spec(parse_message_records(Result::[term()]) -> [#message{}]).

parse_message_records(Result) ->
    [{columns, _ColumnList}, {rows, RowList}] = Result,
    parse_message_records(RowList, []).

parse_message_records(RowList, RecordList) ->
    case RowList of
	[] -> lists:reverse(RecordList);
	[Row | Tail] -> 
	    {Id, MsgId} = Row,
	    Record = #message_index{id = Id, message_id = MsgId},
	    parse_message_records(Tail, [Record | RecordList])
    end.

insert_message_to_sqlite3(DBPid, MessageIndex) ->
    sqlite3:sql_exec(DBPid,
		    "insert into mentions (id, message_id)
                        values(:id, :message_id)",
		    [{':id'        , MessageIndex#message_index.id},
		     {':message_id', MessageIndex#message_index.message_id}]).
