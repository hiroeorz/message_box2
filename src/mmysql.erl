%% File : mmysql.erl
%% Description : database handelr

-module(mmysql).

-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

-define(DEFAULT_MYSQL_POOL, message_box_mysql).

-export([init/0]).
-export([execute/1, execute/2, execute/3, users_table/2,
         table_list/2, table_list/1, users_table_list/1]).

init() ->
    init(?DEFAULT_MYSQL_POOL).

init(Pool) ->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(Pool, 10, 
                    "message_box", "message_box", "localhost", 3306,
                    "message_box", utf8).

%%--------------------------------------------------------------------
%%
%% @doc exec sql
%%
%% @end
%%--------------------------------------------------------------------
-spec(execute(SqlFormat::string()) -> #result_packet{}).

execute(SqlFormat) when is_list(SqlFormat) ->
    execute(?DEFAULT_MYSQL_POOL, SqlFormat, []).

-spec(execute(DBPool::atom(), SqlFormat::string(), Args::list()) ->
             #result_packet{}).

execute(DBPool, SqlFormat, Args) when is_atom(DBPool) and 
                                      is_list(SqlFormat) and
                                      is_list(Args) ->
    Sql = format_sql(SqlFormat, Args),
    emysql:execute(DBPool, Sql).

-spec(execute(SqlFormat::string()|atom(), Args::list()) ->
             #result_packet{}).

execute(SqlFormat, Args) when is_list(SqlFormat) and is_list(Args) ->
    execute(?DEFAULT_MYSQL_POOL, SqlFormat, Args);

execute(DBPool, SqlFormat) when is_atom(DBPool) and is_list(SqlFormat) ->
    execute(DBPool, SqlFormat, []).

%%--------------------------------------------------------------------
%% @doc get table list
%%
%% @end
%%--------------------------------------------------------------------
-spec(table_list(DBPool::atom(), Pattern::string()) -> list(string()) ).

table_list(DBPool, Pattern) when is_atom(DBPool) ->
    Result = execute(DBPool, "show tables like '~s'", [Pattern]),
    lists:map(fun(Row) -> 
                      [Bin] = Row,
                      binary_to_list(Bin)
              end, 
              Result#result_packet.rows).

-spec(table_list(Pattern::string()) -> list(string()) ).

table_list(Pattern) when is_list(Pattern) ->
    table_list(?DEFAULT_MYSQL_POOL, Pattern).

%%--------------------------------------------------------------------
%% @doc get users table list on database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(users_table_list(UserId::integer()) -> list(string()) ).

users_table_list(UserId) when is_integer(UserId) ->
    Pattern = "user_" ++ integer_to_list(UserId) ++ "_%",
    table_list(Pattern).

%%--------------------------------------------------------------------
%% @doc get table name for added user.
%%
%% @end
%%--------------------------------------------------------------------
-spec(users_table(UserId::integer(), TableName::string()) -> string() ).

users_table(UserId, TableName) ->
    "user_" ++ integer_to_list(UserId) ++ "_" ++ TableName.


%%--------------------------------------------------------------------
%% @private
%% @doc create sql binary
%%
%% @end
%%--------------------------------------------------------------------
-spec(format_sql(Str::string(), List::list()) -> binary()).

format_sql(Str, List) ->
    list_to_binary(lists:flatten(io_lib:format(Str, List))).
