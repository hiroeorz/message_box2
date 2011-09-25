%% File : message_db.erl
%% Description : database for user messages

-module(message_db).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

-define(DBName, "messages").

-export([init/1, close_tables/1]).
-export([save_message/3, get_message/3, get_sent_timeline/3, 
         get_latest_message/1]).

%%--------------------------------------------------------------------
%%
%% @doc initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(User::#user{}|integer()) -> {ok, Tid::tid()}).

init(UserId) when is_integer(UserId) ->
    {ok, User} = message_box2_user_db:lookup_id(UserId),
    init(User);

init(User)->
    {ok, Tid} = create_tables(User),
    restore_table(User, Tid),
    {ok, Tid}.    

-spec(create_tables(User::#user{}) -> ok).

create_tables(User)->  
    Tid = ets:new(message, [ordered_set, {keypos, #message.id}]),
    create_mysql_tables(User#user.id),
    {ok, Tid}.

-spec(create_mysql_tables(UserId::integer()) -> ok).

create_mysql_tables(UserId) ->
    TableName = mmysql:users_table(UserId, ?DBName),
    case lists:member(TableName, mmysql:users_table_list(UserId)) of
	true -> ok;
	false ->
	    ?OK_PACKET = mmysql:execute("create table ~s (
                                            id INTEGER PRIMARY KEY,
                                            message_id INTEGER NOT NULL,
                                            text TEXT, 
                                            datetime INTEGER)",
                                        [TableName])
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
-spec(save_message(Tid::tid(), UserId::integer(), Msg::binary()) -> 
             {ok, MessageId::integer()} ).

save_message(Tid, UserId, Msg) ->
    Id = get_max_id(UserId) - 1,
    MessageId = get_message_id(UserId, Id),
    Message = #message{id = Id, 
                       message_id = MessageId, 
                       text = Msg, 
		       datetime={date(), time()}},

    MessageMaxSizeOnMemory = 
        message_box2_config:get(message_max_size_on_memory),

    ets:insert(Tid, Message),
    insert_message_to_mysql(UserId, Message),
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
-spec(get_message(Tid::tid(), User::#user{}, MessageId::integer()) -> 
             #message{} ).

get_message(Tid, User, MessageId)->
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
-spec(get_sent_timeline(Tid::tid(), User::#user{}, Count::integer()) -> 
             [#message{}] ).

get_sent_timeline(Tid, User, Count)->
    UserId = User#user.id,
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
                                      Msg = get_message_from_db(UserId, Id),
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
-spec(get_message_from_db(UserId::integer(), Id::integer) -> 
             #message{} | {error, not_found}).

get_message_from_db(UserId, Id) ->
    TableName = mmysql:users_table(UserId, ?DBName),
    SqlResults = mmysql:execute("select * from ~s where id = ~w",
                                [TableName, Id]),
    case SqlResults of
        [] ->
            {error, not_found};
        Records ->
            [Msg] = parse_message_records(Records),
            Msg
    end.

-spec(get_max_id(UserId::integer()) -> integer()).

get_max_id(UserId)->
    TableName = mmysql:users_table(UserId, ?DBName),
    Result = mmysql:execute("select * from ~s order by id limit 1",
                           [TableName]),

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
    parse_message_records(Result#result_packet.rows, []).

parse_message_records(RowList, RecordList) ->
    case RowList of
	[] -> lists:reverse(RecordList);
	[Row | Tail] -> 
	    [Id, MsgId, TextBin, Sec] = Row,
	    Datetime = calendar:gregorian_seconds_to_datetime(Sec),
	    Record = #message{id = Id, message_id = MsgId,
			      text = TextBin,
			      datetime = Datetime},
	    parse_message_records(Tail, [Record | RecordList])
    end.

-spec(insert_message_to_mysql(UserId::integer(), Message::#message{}) -> ok).

insert_message_to_mysql(UserId, Message) ->
    TableName = mmysql:users_table(UserId, ?DBName),
    Sec = calendar:datetime_to_gregorian_seconds(Message#message.datetime),
    ?OK_PACKET = mmysql:execute("insert into ~s (id, message_id, text, datetime)
                                   values(~w, ~w, '~s', ~w)",
                                [TableName,
                                 Message#message.id,
                                 Message#message.message_id,
                                 Message#message.text,
                                 Sec
                                ]),
    ok.
