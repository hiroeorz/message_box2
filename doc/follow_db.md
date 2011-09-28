Module follow_db
================


<h1>Module follow_db</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_follow_user-2">delete_follow_user/2</a></td><td>delet user from follow database.</td></tr><tr><td valign="top"><a href="#get_follow_ids-1">get_follow_ids/1</a></td><td>get all follow users id.</td></tr><tr><td valign="top"><a href="#get_follower_ids-1">get_follower_ids/1</a></td><td>get all follow users id.</td></tr><tr><td valign="top"><a href="#get_followers-1">get_followers/1</a></td><td>get all follow users id.</td></tr><tr><td valign="top"><a href="#get_follows-1">get_follows/1</a></td><td>get all follow users id.</td></tr><tr><td valign="top"><a href="#is_following-2">is_following/2</a></td><td>check followin user or not.</td></tr><tr><td valign="top"><a href="#map_do-3">map_do/3</a></td><td>do function to all users.</td></tr><tr><td valign="top"><a href="#save_follow_user-2">save_follow_user/2</a></td><td>save user to follow database.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="delete_follow_user-2"></a>

<h3>delete_follow_user/2</h3>





<pre>delete_follow_user(User::#user{id = undefined | non_neg_integer(), status = atom(), pid = undefined | pid(), name = undefined | binary(), mail = undefined | binary(), password = undefined | binary()}, Id::integer()) -> ok | {error, not_following}</pre>
<br></br>




delet user from follow database.
<a name="get_follow_ids-1"></a>

<h3>get_follow_ids/1</h3>





<pre>get_follow_ids(User::integer()) -> [#follow{user_id = undefined | non_neg_integer(), id = undefined | non_neg_integer(), datetime = undefined | <a href="calendar.md#type-t_now">calendar:t_now()</a>}]</pre>
<br></br>




get all follow users id.
<a name="get_follower_ids-1"></a>

<h3>get_follower_ids/1</h3>





<pre>get_follower_ids(User::integer()) -> [#follow{user_id = undefined | non_neg_integer(), id = undefined | non_neg_integer(), datetime = undefined | <a href="calendar.md#type-t_now">calendar:t_now()</a>}]</pre>
<br></br>




get all follow users id.
<a name="get_followers-1"></a>

<h3>get_followers/1</h3>





<pre>get_followers(User::integer()) -> [#follow{user_id = undefined | non_neg_integer(), id = undefined | non_neg_integer(), datetime = undefined | <a href="calendar.md#type-t_now">calendar:t_now()</a>}]</pre>
<br></br>




get all follow users id.
<a name="get_follows-1"></a>

<h3>get_follows/1</h3>





<pre>get_follows(User::integer()) -> [#follow{user_id = undefined | non_neg_integer(), id = undefined | non_neg_integer(), datetime = undefined | <a href="calendar.md#type-t_now">calendar:t_now()</a>}]</pre>
<br></br>




get all follow users id.
<a name="is_following-2"></a>

<h3>is_following/2</h3>





<pre>is_following(User::#user{id = undefined | non_neg_integer(), status = atom(), pid = undefined | pid(), name = undefined | binary(), mail = undefined | binary(), password = undefined | binary()}, Id::integer()) -> true | false</pre>
<br></br>




check followin user or not.
<a name="map_do-3"></a>

<h3>map_do/3</h3>





<pre>map_do(X1::follow | follower, User::#user{id = undefined | non_neg_integer(), status = atom(), pid = undefined | pid(), name = undefined | binary(), mail = undefined | binary(), password = undefined | binary()}, Fun::function()) -> ok</pre>
<br></br>




do function to all users.
<a name="save_follow_user-2"></a>

<h3>save_follow_user/2</h3>





<pre>save_follow_user(User::#user{id = undefined | non_neg_integer(), status = atom(), pid = undefined | pid(), name = undefined | binary(), mail = undefined | binary(), password = undefined | binary()}, Id::integer()) -> ok | {error, already_following}</pre>
<br></br>




save user to follow database.
