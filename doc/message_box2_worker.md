Module message_box2_worker
==========================


<h1>Module message_box2_worker</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_user-4">create_user/4</a></td><td>
Get Message from user process.</td></tr><tr><td valign="top"><a href="#follow-4">follow/4</a></td><td>
Follow other user.</td></tr><tr><td valign="top"><a href="#get_home_timeline-3">get_home_timeline/3</a></td><td>
Get home timeline list.</td></tr><tr><td valign="top"><a href="#get_mentions_timeline-3">get_mentions_timeline/3</a></td><td>
Get mentions timeline list.</td></tr><tr><td valign="top"><a href="#get_message-2">get_message/2</a></td><td>
Get Message from user process.</td></tr><tr><td valign="top"><a href="#get_sent_timeline-3">get_sent_timeline/3</a></td><td>
Get sent timeline list.</td></tr><tr><td valign="top"><a href="#is_following-3">is_following/3</a></td><td>
Check follow state.</td></tr><tr><td valign="top"><a href="#send_message-4">send_message/4</a></td><td>
Send Message.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#unfollow-4">unfollow/4</a></td><td>
Remove other user.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="create_user-4"></a>

<h3>create_user/4</h3>





`create_user(Pid, Name, Mail, Password) -> any()`




Get Message from user process.
<a name="follow-4"></a>

<h3>follow/4</h3>





<pre>follow(Pid::pid(), UserId::integer(), Password::string(), FollowUserId::integer()) -> ok | {error, already_following} | {error, user_not_found}</pre>
<br></br>





Follow other user.
<a name="get_home_timeline-3"></a>

<h3>get_home_timeline/3</h3>





<pre>get_home_timeline(Pid::pid(), UserId::integer(), Count::integer()) -> {ok, [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]} | {error, user_not_found}</pre>
<br></br>





Get home timeline list.
<a name="get_mentions_timeline-3"></a>

<h3>get_mentions_timeline/3</h3>





<pre>get_mentions_timeline(Pid::pid(), UserId::integer(), Count::integer()) -> {ok, [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]} | {error, user_not_found}</pre>
<br></br>





Get mentions timeline list.
<a name="get_message-2"></a>

<h3>get_message/2</h3>





<pre>get_message(Pid::pid(), MessageId::integer()) -> #message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}</pre>
<br></br>





Get Message from user process.
<a name="get_sent_timeline-3"></a>

<h3>get_sent_timeline/3</h3>





<pre>get_sent_timeline(Pid::pid(), UserId::integer(), Count::integer()) -> {ok, [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]} | {error, user_not_found}</pre>
<br></br>





Get sent timeline list.
<a name="is_following-3"></a>

<h3>is_following/3</h3>





<pre>is_following(Pid::pid(), UserId::integer(), FollowUserId::integer()) -> true | false | {error, not_found} | {error, user_not_found}</pre>
<br></br>





Check follow state.
<a name="send_message-4"></a>

<h3>send_message/4</h3>





<pre>send_message(Pid::pid(), UserId::integer(), Password::string(), TextBin::binary()) -> {ok, MessageId::integer()}</pre>
<br></br>





Send Message.
<a name="start_link-0"></a>

<h3>start_link/0</h3>





<pre>start_link() -> {ok, Pid} | ignore | {error, Error}</pre>
<br></br>





Starts the server
<a name="unfollow-4"></a>

<h3>unfollow/4</h3>





<pre>unfollow(Pid::pid(), UserId::integer(), Password::string(), FollowUserId::integer()) -> ok | {error, not_following} | {error, user_not_found}</pre>
<br></br>





Remove other user.
