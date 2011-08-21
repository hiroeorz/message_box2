%% File : message_db.erl
%% Description : database for user messages

-module(message_db).
-include_lib("eunit/include/eunit.hrl").
-include("../include/message_box.hrl").
-include("../include/message.hrl").
-include("../include/user.hrl").

-export([init/1, close_tables/1]).
-export([save_message/4, get_message/4, get_sent_timeline/4, 
         get_latest_message/1]).

%%--------------------------------------------------------------------
%%
%% @doc initialize.
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
    Tid = ets:new(message, [ordered_set, {keypos, #message.id}]),
    create_sqlite3_tables(DBPid),
    {ok, Tid}.

-spec(create_sqlite3_tables(DBPid::pid()) -> ok).

create_sqlite3_tables(DBPid) ->
    case lists:member(messages, sqlite3:list_tables(DBPid)) of
	true -> ok;
	false ->
	    sqlite3:sql_exec(DBPid, 
			     "create table messages (
                                id INTEGER PRIMARY KEY,
                                message_id INTEGER NOT NULL,
                                text TEXT, 
                                datetime INTEGER)")
    end.    

-spec(restore_table(DBPid::pid(), Tid::tid()) -> ok).

restore_table(DBPid, Tid) ->
    MessageMaxSizeOnMemory = 
        message_box2_config:get(message_max_size_on_memory),

    SqlResults = sqlite3:sql_exec(DBPid,
				  "select * from messages
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
%%
%% @doc close ets table.
%%
%% @end
%%--------------------------------------------------------------------
-spec(close_tables(Tid::tid()) -> true).

close_tables(Tid)->
    ets:delete(Tid).

%%--------------------------------------------------------------------
%%
%% @doc save message to ets and sqlite3 database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_message(Tid::tid(), DBPid::pid(), User::#user{}, Msg::binary()) -> 
             {ok, MessageId::integer()} ).

save_message(Tid, DBPid, User, Msg) ->
    Id = get_max_id(DBPid) - 1,
    MessageId = get_message_id(User#user.id, Id),
    Message = #message{id = Id, 
                       message_id = MessageId, 
                       text = Msg, 
		       datetime={date(), time()}},

    MessageMaxSizeOnMemory = 
        message_box2_config:get(message_max_size_on_memory),

    ets:insert(Tid, Message),
    insert_message_to_sqlite3(DBPid, Message),
    util:shurink_ets(Tid, MessageMaxSizeOnMemory),
    {ok, MessageId}.

%%--------------------------------------------------------------------
%%
%% @doc get latest message.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_latest_message(Tid::tid()) -> #message{}|{error, no_message_exist}).

get_latest_message(Tid)->
    case ets:first(Tid) of
	'$end_of_table' -> {error, no_message_exist};
	Id -> 
	    [Message] = ets:lookup(Tid, Id),
	    Message
    end.


%%--------------------------------------------------------------------
%%
%% @doc get message from database.
%%
%%--------------------------------------------------------------------
-spec(get_message(Tid::tid(), DBPid::pid(), 
                  User::#user{}, MessageId::integer()) -> #message{} ).

get_message(Tid, _DBPid, User, MessageId)->
    MessagePattern = #message{id='$1', message_id=MessageId, text='_', 
			      datetime='_'},
    case ets:match(Tid, MessagePattern) of
	[] -> {error, not_found};
	[[Id]] -> 
	    case ets:lookup(Tid, Id) of
		[Message0] ->
		    Message1 = Message0#message{user=User},
		    {ok, Message1};
		Other -> {error, Other}
	    end
    end.

%%
%% @doc get sent timeline, max length is Count.
%%
-spec(get_sent_timeline(Tid::tid(), DBPid::pid(), User::#user{}, 
                        Count::integer()) -> [#message{}] ).

get_sent_timeline(Tid, DBPid, User, Count)->
    First = ets:first(Tid),

    case ets:first(Tid) of
	'$end_of_table' -> [];
	First ->
	    MessageIds = util:get_timeline_ids(Tid, Count, First, [First]),
	    lists:map(fun(Id) -> 
                              case ets:lookup(Tid, Id) of
                                  [Msg] -> 
                                      Msg#message{user=User};
                                  [] -> 
                                      Msg = get_message_from_db(DBPid, Id),
                                      Msg#message{user=User}
                              end
                      end,
		      MessageIds)
    end.

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%%
%% @doc get message from sqlite3
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_message_from_db(DBPid::pid(), Id::integer) -> 
             #message{} | {error, not_found}).

get_message_from_db(DBPid, Id) ->
    SqlResults = sqlite3:sql_exec(DBPid, 
                                  "select * from messages where id = :id",
                                  [{':id', Id}]),
    case SqlResults of
        [] ->
            {error, not_found};
        Records ->
            [Msg] = parse_message_records(Records),
            Msg
    end.

-spec(get_max_id(DBPid::pid()) -> integer()).

get_max_id(DBPid)->
    Result = sqlite3:sql_exec(DBPid, "select * from messages 
                                           order by id limit 1"),
    
    case parse_message_records(Result) of
	[] -> 0;
	[LastRecord] -> LastRecord#message.id
    end.

-spec(get_message_id(UserId::integer(), Id::integer()) -> integer()).

get_message_id(UserId, Id) ->
    FormattedUserId = util:formatted_number(UserId, 9),
    FormattedId = util:formatted_number(abs(Id), 9),
    list_to_integer(string:concat(FormattedUserId, FormattedId)).

%%--------------------------------------------------------------------
%%
%% @doc parse message record from sqlite3 to erlang record.
%%
%% @end
%%--------------------------------------------------------------------
-spec(parse_message_records(list()) -> list(#message{}) ).

parse_message_records(Result) ->
    [{columns, _ColumnList}, {rows, RowList}] = Result,
    parse_message_records(RowList, []).

parse_message_records(RowList, RecordList) ->
    case RowList of
	[] -> lists:reverse(RecordList);
	[Row | Tail] -> 
	    {Id, MsgId, TextBin, Sec} = Row,
	    Datetime = calendar:gregorian_seconds_to_datetime(Sec),
	    Record = #message{id = Id, message_id = MsgId,
			      text = TextBin,
			      datetime = Datetime},
	    parse_message_records(Tail, [Record | RecordList])
    end.

insert_message_to_sqlite3(DBPid, Message) ->
    Sec = calendar:datetime_to_gregorian_seconds(Message#message.datetime),
    sqlite3:sql_exec(DBPid,
		    "insert into messages (id, message_id, text, datetime)
                        values(:id, :message_id, :text, :datetime)",
		    [{':id'        , Message#message.id},
		     {':message_id', Message#message.message_id},
		     {':text'      , Message#message.text},
		     {':datetime'  , Sec}]).
