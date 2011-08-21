Module message_box2_user_db
===========================


<h1>Module message_box2_user_db</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_user-3">add_user/3</a></td><td>add new user to system.</td></tr><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td>lookup user pid.</td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td>lookup user from id.</td></tr><tr><td valign="top"><a href="#lookup_name-1">lookup_name/1</a></td><td>lookup user from name(string).</td></tr><tr><td valign="top"><a href="#lookup_pid-1">lookup_pid/1</a></td><td>lookup user from pid.</td></tr><tr><td valign="top"><a href="#map_do-1">map_do/1</a></td><td>exec fun to each element of user list.</td></tr><tr><td valign="top"><a href="#save_pid-2">save_pid/2</a></td><td>save user pid.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#update_user-1">update_user/1</a></td><td>add new user to system.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="add_user-3"></a>

<h3>add_user/3</h3>





<pre>add_user(Name::atom(), Mail::string(), Password::string()) -> {ok, #user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}} | {error, already_exist}</pre>
<br></br>




add new user to system.
<a name="get_pid-1"></a>

<h3>get_pid/1</h3>





<pre>get_pid(UserName_OR_Id::integer() | atom() | string()) -> {ok, #user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}} | {error, not_found}</pre>
<br></br>




lookup user pid.
<a name="lookup_id-1"></a>

<h3>lookup_id/1</h3>





<pre>lookup_id(Id::integer()) -> {ok, #user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}} | {error, not_found}</pre>
<br></br>




lookup user from id.
<a name="lookup_name-1"></a>

<h3>lookup_name/1</h3>





<pre>lookup_name(Name::atom() | string()) -> {ok, #user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}} | {error, not_found}</pre>
<br></br>




lookup user from name(string).
<a name="lookup_pid-1"></a>

<h3>lookup_pid/1</h3>





<pre>lookup_pid(Pid::pid()) -> {ok, #user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}} | {error, not_found}</pre>
<br></br>




lookup user from pid.
<a name="map_do-1"></a>

<h3>map_do/1</h3>





<pre>map_do(Fun::function()) -> ok</pre>
<br></br>




exec fun to each element of user list.
<a name="save_pid-2"></a>

<h3>save_pid/2</h3>





<pre>save_pid(Id::integer(), Pid::pid()) -> ok | {error, not_found}</pre>
<br></br>




save user pid.
<a name="start_link-0"></a>

<h3>start_link/0</h3>





<pre>start_link() -> {ok, Pid} | ignore | {error, Error}</pre>
<br></br>





Starts the server
<a name="update_user-1"></a>

<h3>update_user/1</h3>





<pre>update_user(User::#user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}) -> {ok, #user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}} | {error, not_found}</pre>
<br></br>




add new user to system.
