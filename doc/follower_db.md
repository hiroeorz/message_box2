Module follower_db
==================


<h1>Module follower_db</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)







<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-tid">tid()</a></h3>




<pre>tid() = integer()</pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close_tables-2">close_tables/2</a></td><td>close ets and dets database.</td></tr><tr><td valign="top"><a href="#delete_follower_user-3">delete_follower_user/3</a></td><td>delet user from follower database.</td></tr><tr><td valign="top"><a href="#get_follower_ids-1">get_follower_ids/1</a></td><td>get all follower users id.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>load follower users from dets to ets.</td></tr><tr><td valign="top"><a href="#is_follower-2">is_follower/2</a></td><td>check followerin user or not.</td></tr><tr><td valign="top"><a href="#map_do-2">map_do/2</a></td><td>do function to all users.</td></tr><tr><td valign="top"><a href="#save_follower_user-3">save_follower_user/3</a></td><td>save user to follower database.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="close_tables-2"></a>

<h3>close_tables/2</h3>





<pre>close_tables(Tid::<a href="#type-tid">tid()</a>, UserName::atom()) -> ok | {error, Reason::term()}</pre>
<br></br>




close ets and dets database.
<a name="delete_follower_user-3"></a>

<h3>delete_follower_user/3</h3>





<pre>delete_follower_user(Tid::<a href="#type-tid">tid()</a>, User::#user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}, Id::integer()) -> {ok, deleted} | {error, not_followering}</pre>
<br></br>




delet user from follower database.
<a name="get_follower_ids-1"></a>

<h3>get_follower_ids/1</h3>





<pre>get_follower_ids(Tid::<a href="#type-tid">tid()</a>) -> [#follower{id = undefined | integer(), datetime = undefined | term()}]</pre>
<br></br>




get all follower users id.
<a name="init-1"></a>

<h3>init/1</h3>





<pre>init(UserName::atom()) -> {ok, Tid::<a href="#type-tid">tid()</a>}</pre>
<br></br>




load follower users from dets to ets.
<a name="is_follower-2"></a>

<h3>is_follower/2</h3>





<pre>is_follower(Tid::<a href="#type-tid">tid()</a>, UserId::integer()) -> true | false</pre>
<br></br>




check followerin user or not.
<a name="map_do-2"></a>

<h3>map_do/2</h3>





<pre>map_do(Tid::<a href="#type-tid">tid()</a>, Fun::function()) -> ok</pre>
<br></br>




do function to all users.
<a name="save_follower_user-3"></a>

<h3>save_follower_user/3</h3>





<pre>save_follower_user(Tid::<a href="#type-tid">tid()</a>, User::#user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}, Id::integer()) -> ok | {error, already_followering}</pre>
<br></br>




save user to follower database.
