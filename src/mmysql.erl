%% File : mmysql.erl
%% Description : database handler

-module(mmysql).

-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

%% @todo move to configuration.
-define(DEFAULT_MYSQL_POOL,       message_box_mysql).
-define(DEFAULT_MYSQL_POOL_COUNT, 10).
-define(DEFAULT_MYSQL_DATABASE,   "message_box").
-define(DEFAULT_MYSQL_USER,       "message_box").
-define(DEFAULT_MYSQL_PASSWORD,   "message_box").
-define(DEFAULT_MYSQL_HOST,       "localhost").
-define(DEFAULT_MYSQL_PORT,       3306).

%% @doc for test.
-define(TEST_MYSQL_POOL,       ?DEFAULT_MYSQL_POOL).
-define(TEST_MYSQL_POOL_COUNT, ?DEFAULT_MYSQL_POOL_COUNT).
-define(TEST_MYSQL_DATABASE,   "message_box_test").
-define(TEST_MYSQL_USER,       ?DEFAULT_MYSQL_USER).
-define(TEST_MYSQL_PASSWORD,   ?DEFAULT_MYSQL_PASSWORD).
-define(TEST_MYSQL_HOST,       ?DEFAULT_MYSQL_HOST).
-define(TEST_MYSQL_PORT,       ?DEFAULT_MYSQL_PORT).

-export([init/0]).
-export([execute/1, execute/2, execute/3, users_table/2,
         table_list/2, table_list/1, users_table_list/1]).

init() ->
    init(?DEFAULT_MYSQL_POOL).

init_for_test() ->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(?TEST_MYSQL_POOL, 
                    ?TEST_MYSQL_POOL_COUNT, 
                    ?TEST_MYSQL_USER, ?TEST_MYSQL_PASSWORD, 
                    ?TEST_MYSQL_HOST, ?TEST_MYSQL_PORT,
                    ?TEST_MYSQL_DATABASE, utf8).

init(Pool) ->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(Pool, 
                    ?DEFAULT_MYSQL_POOL_COUNT, 
                    ?DEFAULT_MYSQL_USER, ?DEFAULT_MYSQL_PASSWORD, 
                    ?DEFAULT_MYSQL_HOST, ?DEFAULT_MYSQL_PORT,
                    ?DEFAULT_MYSQL_DATABASE, utf8).

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
    Sql = lists:flatten(io_lib:format(Str, List)),
    ?debugVal(Sql),
    list_to_binary(Sql).
