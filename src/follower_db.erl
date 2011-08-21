%% File : follow_db.erl
%% Description : user follow relationship database.

-module(follower_db).
-include_lib("eunit/include/eunit.hrl").
-include("../include/message_box.hrl").
-include("../include/message.hrl").
-include("../include/user.hrl").

-export([init/1]).
-export([close_tables/2, save_follower_user/3, delete_follower_user/3,
        get_follower_ids/1, map_do/2, is_follower/2]).

%%--------------------------------------------------------------------
%%
%% @doc load follower users from dets to ets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(UserName::atom()) -> {ok, EtsPid::pid()}).

init(UserName) ->
    process_flag(trap_exit, true),

    DB_DIR = message_box2_config:get(database_dir),
    case file:make_dir(DB_DIR ++ atom_to_list(UserName)) of
        ok -> ok;
        {error, eexist} -> ok
    end,

    {ok, EtsPid} = create_tables(UserName),
    restore_table(EtsPid, UserName),
    {ok, EtsPid}.

    
%%--------------------------------------------------------------------
%%
%% @doc create table ets and dets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_tables(Device::atom()) -> {ok, EtsPid::pid(), DetsPid::pid()}).

create_tables(UserName) ->  
    EtsPid = ets:new(follower, [ordered_set, {keypos, #follower.id}]),
    {DiscName, FileName} = dets_info(UserName),
    dets:open_file(DiscName, [{file, FileName}, {keypos, #follower.id}]),
    {ok, EtsPid}.

%%--------------------------------------------------------------------
%%
%% @doc load follower users from dets to ets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(restore_table(EtsPid::pid(), UserName::atom()) -> ok).

restore_table(EtsPid, UserName) ->
    Insert = fun(#follower{id=_Id, datetime=_DateTime} = Follower)->
		     ets:insert(EtsPid, Follower),
		     continue
	     end,

    {Dets, _} = dets_info(UserName),
    dets:traverse(Dets, Insert),
    ok.

%%--------------------------------------------------------------------
%%
%% @doc close ets and dets database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(close_tables(EtsPid::pid(), UserName::atom()) -> 
             ok | {error, Reason::term()}).

close_tables(EtsPid, UserName) ->
    {Dets, _} = dets_info(UserName),
    ets:delete(EtsPid),
    dets:close(Dets).

%%--------------------------------------------------------------------
%%
%% @doc save user to follower database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_follower_user(EtsPid::pid(), User::#user{}, Id::integer()) ->
             ok | {error, already_followering}).

save_follower_user(EtsPid, User, Id) ->
    Follower = #follower{id=Id, datetime={date(), time()}},

    case is_followering(User, Id) of
	true -> {error, already_followering};
	false ->
            {Dets, _} = dets_info(User#user.name),
	    ets:insert(EtsPid, Follower),
	    dets:insert(Dets, Follower),
	    ok
    end.

%%--------------------------------------------------------------------
%%
%% @doc delet user from follower database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(delete_follower_user(EtsPid::pid(), User::#user{}, Id::integer()) -> 
             {ok, deleted} | {error, not_followering}).

delete_follower_user(EtsPid, User, Id) ->
    case is_followering(EtsPid, Id) of
	true ->
	    {Dets, _} = dets_info(User#user.name),
	    ets:delete(EtsPid, Id),
	    dets:delete(Dets, Id),
	    {ok, deleted};
	false -> {error, not_followering}
    end.

%%--------------------------------------------------------------------
%%
%% @doc get all follower users id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_follower_ids(EtsPid::pid()) -> [#follower{}]).

get_follower_ids(EtsPid) ->
    case ets:first(EtsPid) of
	'$end_of_table' -> [];
	First -> collect_id(EtsPid, First, [First])
    end.

%%--------------------------------------------------------------------
%%
%% @doc do function to all users.
%%
%% @end
%%--------------------------------------------------------------------
-spec(map_do(EtsPid::pid(), Fun::fun()) -> ok).

map_do(EtsPid, Fun) ->
    case ets:first(EtsPid) of
	'$end_of_table' ->
	    ok;
	First ->
	    [Follower] = ets:lookup(EtsPid, First),
	    Fun(Follower),
	    map_do(EtsPid, Fun, First)
    end.

%%--------------------------------------------------------------------
%%
%% @doc check followerin user or not.
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_follower(EtsPid::pid(), UserId::integer()) -> true|false).

is_follower(EtsPid, UserId) ->
    case ets:lookup(EtsPid, UserId) of
        [_FolloweringUser] -> true;
        [] -> false
    end.

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

-spec(collect_id(EtsPid::pid(), Before::integer(), Result::[integer()]) ->
             [integer()]).

collect_id(EtsPid, Before, Result) ->
    case ets:next(EtsPid, Before) of
	'$end_of_table' -> Result;
	FollowerId -> collect_id(EtsPid, FollowerId, [FollowerId | Result])
    end.	    

-spec(is_followering(EtsPid::pid(), Id::integer()) -> true | false).

is_followering(EtsPid, Id) ->
    case ets:lookup(EtsPid, Id) of
	[_Follower] -> true;
	[] -> false
    end.    

-spec(dets_info(UserName::atom()) -> {Dets::atom(), FileName::string()}).

dets_info(UserName)->
    Dets = list_to_atom(atom_to_list(UserName) ++ "_FollowerDisc"),
    DB_DIR = message_box2_config:get(database_dir),
    FileName = DB_DIR ++ atom_to_list(UserName) ++ "follower",
    {Dets, FileName}.

-spec(map_do(EtsPid::pid(), Fun::fun(), Entry::integer()) -> term()).

map_do(EtsPid, Fun, Entry) ->
    case ets:next(EtsPid, Entry) of
	'$end_of_table' ->
	    ok;
	Next ->
	    [Follower] = ets:lookup(EtsPid, Next),
	    Fun(Follower),
	    map_do(EtsPid, Fun, Next)
    end.
