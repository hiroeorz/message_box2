Module follow_db
================


<h1>Module follow_db</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close_tables-2">close_tables/2</a></td><td>close ets and dets database.</td></tr><tr><td valign="top"><a href="#delete_follow_user-3">delete_follow_user/3</a></td><td>delet user from follow database.</td></tr><tr><td valign="top"><a href="#get_follow_ids-1">get_follow_ids/1</a></td><td>get all follow users id.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>load follow users from dets to ets.</td></tr><tr><td valign="top"><a href="#is_follow-2">is_follow/2</a></td><td>check followin user or not.</td></tr><tr><td valign="top"><a href="#map_do-2">map_do/2</a></td><td>do function to all users.</td></tr><tr><td valign="top"><a href="#save_follow_user-3">save_follow_user/3</a></td><td>save user to follow database.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="close_tables-2"></a>

<h3>close_tables/2</h3>





<pre>close_tables(EtsPid::pid(), UserName::atom()) -> ok | {error, Reason::term()}</pre>
<br></br>




close ets and dets database.
<a name="delete_follow_user-3"></a>

<h3>delete_follow_user/3</h3>





<pre>delete_follow_user(EtsPid::pid(), User::#user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}, Id::integer()) -> {ok, deleted} | {error, not_following}</pre>
<br></br>




delet user from follow database.
<a name="get_follow_ids-1"></a>

<h3>get_follow_ids/1</h3>





<pre>get_follow_ids(EtsPid::pid()) -> [#follow{id = undefined | integer(), datetime = undefined | term()}]</pre>
<br></br>




get all follow users id.
<a name="init-1"></a>

<h3>init/1</h3>





<pre>init(UserName::atom()) -> {ok, EtsPid::pid()}</pre>
<br></br>




load follow users from dets to ets.
<a name="is_follow-2"></a>

<h3>is_follow/2</h3>





<pre>is_follow(EtsPid::pid(), UserId::integer()) -> true | false</pre>
<br></br>




check followin user or not.
<a name="map_do-2"></a>

<h3>map_do/2</h3>





<pre>map_do(EtsPid::pid(), Fun::function()) -> ok</pre>
<br></br>




do function to all users.
<a name="save_follow_user-3"></a>

<h3>save_follow_user/3</h3>





<pre>save_follow_user(EtsPid::pid(), User::#user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}, Id::integer()) -> ok | {error, already_following}</pre>
<br></br>




save user to follow database.
