%% File : follow_db.erl
%% Description : user follow relationship database.

-module(follow_db).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

-export([save_follow_user/2, delete_follow_user/2,
         get_follow_ids/1, get_follower_ids/1, map_do/2, is_following/2, 
         get_follows/1, get_followers/1]).

%%--------------------------------------------------------------------
%%
%% @doc save user to follow database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_follow_user(User::#user{}, Id::integer()) ->
             ok | {error, already_following}).

save_follow_user(User, Id) ->
    Follow = #follow{user_id=User#user.id, id=Id, datetime={date(), time()}},
    
    case is_following(User, Id) of
	true -> {error, already_following};
	false ->
            mnesia:activity(transaction, fun() -> mnesia:write(Follow) end)
    end.

%%--------------------------------------------------------------------
%%
%% @doc delet user from follow database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(delete_follow_user(User::#user{}, Id::integer()) -> 
             {ok, deleted} | {error, not_following}).

delete_follow_user(User, Id) ->
    case is_following(User, Id) of
	true ->
            UserId = User#user.id,
            Pattern = #follow{user_id = UserId, id = Id, datetime = '_'},
            mnesia:activity(transaction, 
                            fun() ->
                                    [Follow] = mnesia:match_object(Pattern),
                                    mnesia:delete_object(Follow) 
                            end);
	false -> {error, not_following}
    end.

%%--------------------------------------------------------------------
%%
%% @doc get all follow users id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_follow_ids(User::integer()) -> [#follow{}]).

get_follow_ids(User) ->
    UserId = User#user.id,
    Pattern = #follow{user_id = UserId, id = '_', datetime = '_'},
    FollowList = mnesia:activity(async_dirty, 
                                 fun() -> mnesia:match_object(Pattern) end),

    lists:map(fun(Follow) -> Follow#follow.id end, FollowList).

%%--------------------------------------------------------------------
%%
%% @doc get all follow users id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_follower_ids(User::integer()) -> [#follow{}]).

get_follower_ids(User) ->
    UserId = User#user.id,
    Pattern = #follow{user_id = '_', id = UserId, datetime = '_'},
    FollowerList = mnesia:activity(async_dirty, 
                                   fun() -> mnesia:match_object(Pattern) end),

    lists:map(fun(Follower) -> Follower#follow.id end, FollowerList).

%%--------------------------------------------------------------------
%%
%% @doc get all follow users id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_follows(User::integer()) -> [#follow{}]).

get_follows(User) ->
    UserId = User#user.id,
    Pattern = #follow{user_id = UserId, id = '_', datetime = '_'},
    FollowList = mnesia:activity(async_dirty, 
                                 fun() -> mnesia:match_object(Pattern) end),

    lists:map(fun(Follow) -> 
                      message_box2_user_db:lookup_id(Follow#follow.id)
              end, 
              FollowList).

%%--------------------------------------------------------------------
%%
%% @doc get all follow users id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_followers(User::integer()) -> [#follow{}]).

get_followers(User) ->
    UserId = User#user.id,
    Pattern = #follow{user_id = '_', id = UserId, datetime = '_'},
    FollowerList = mnesia:activity(async_dirty, 
                                   fun() -> mnesia:match_object(Pattern) end),

    lists:map(fun(Follower) -> 
                      message_box2_user_db:lookup_id(Follower#follow.user_id)
              end, 
              FollowerList).

%%--------------------------------------------------------------------
%%
%% @doc do function to all users.
%%
%% @end
%%--------------------------------------------------------------------
-spec(map_do(User::#user{}, Fun::fun()) -> ok).

map_do(User, Fun) ->
    UserId = User#user.id,
    Pattern = #follow{user_id = UserId, id = '_', datetime = '_'},
    FollowList = mnesia:activity(async_dirty, 
                                 fun() -> mnesia:match_object(Pattern) end),
    lists:map(Fun, FollowList),
    ok.

%%--------------------------------------------------------------------
%%
%% @doc check followin user or not.
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_following(UserId::#user{}, Id::integer()) -> true | false).

is_following(User, Id) ->
    UserId = User#user.id,
    Pattern = #follow{user_id = UserId, id = Id, datetime = '_'},

    case mnesia:activity(async_dirty, 
                         fun() -> mnesia:match_object(Pattern) end) of
	[_Follow] -> true;
	[] -> false
    end.    


