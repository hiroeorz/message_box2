Module message_box2
===================


<h1>Module message_box2</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_user-3">create_user/3</a></td><td>
Get Message from user process.</td></tr><tr><td valign="top"><a href="#follow-3">follow/3</a></td><td>
Follow other user.</td></tr><tr><td valign="top"><a href="#get_home_timeline-2">get_home_timeline/2</a></td><td>
Get home timeline list.</td></tr><tr><td valign="top"><a href="#get_mentions_timeline-2">get_mentions_timeline/2</a></td><td>
Get mentions timeline list.</td></tr><tr><td valign="top"><a href="#get_message-1">get_message/1</a></td><td>
Get Message from user process.</td></tr><tr><td valign="top"><a href="#get_sent_timeline-2">get_sent_timeline/2</a></td><td>
Get sent timeline list.</td></tr><tr><td valign="top"><a href="#is_following-2">is_following/2</a></td><td>
Check follow state.</td></tr><tr><td valign="top"><a href="#send_message-3">send_message/3</a></td><td>
Send Message.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
Boot file_box system.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>
Shut down file_box system.</td></tr><tr><td valign="top"><a href="#unfollow-3">unfollow/3</a></td><td>
Remove other user.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="create_user-3"></a>

<h3>create_user/3</h3>





<pre>create_user(Name::string(), Mail::string(), Password::string()) -> {ok, #user{id = undefined | non_neg_integer(), status = atom(), pid = undefined | pid(), name = undefined | binary(), mail = undefined | binary(), password = undefined | binary()}} | {error, already_exist}</pre>
<br></br>





Get Message from user process.
<a name="follow-3"></a>

<h3>follow/3</h3>





<pre>follow(UserId::integer(), Password::string(), FollowUserId::integer()) -> ok | {error, already_following} | {error, user_not_found}</pre>
<br></br>





Follow other user.
<a name="get_home_timeline-2"></a>

<h3>get_home_timeline/2</h3>





<pre>get_home_timeline(UserId::integer(), Count::integer()) -> {ok, [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]} | {error, user_not_found}</pre>
<br></br>





Get home timeline list.
<a name="get_mentions_timeline-2"></a>

<h3>get_mentions_timeline/2</h3>





<pre>get_mentions_timeline(UserId::integer(), Count::integer()) -> {ok, [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]} | {error, user_not_found}</pre>
<br></br>





Get mentions timeline list.
<a name="get_message-1"></a>

<h3>get_message/1</h3>





<pre>get_message(MessageId::integer()) -> #message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}</pre>
<br></br>





Get Message from user process.
<a name="get_sent_timeline-2"></a>

<h3>get_sent_timeline/2</h3>





<pre>get_sent_timeline(UserId::integer(), Count::integer()) -> {ok, [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]} | {error, user_not_found}</pre>
<br></br>





Get sent timeline list.
<a name="is_following-2"></a>

<h3>is_following/2</h3>





<pre>is_following(UserId::integer(), FollowUserId::integer()) -> true | false | {error, not_found} | {error, user_not_found}</pre>
<br></br>





Check follow state.
<a name="send_message-3"></a>

<h3>send_message/3</h3>





<pre>send_message(UserId::integer(), Password::string(), TextBin::binary()) -> {ok, MessageId::integer()}</pre>
<br></br>





Send Message.
<a name="start-0"></a>

<h3>start/0</h3>





<pre>start() -> ok</pre>
<br></br>





Boot file_box system.
<a name="stop-0"></a>

<h3>stop/0</h3>





<pre>stop() -> ok</pre>
<br></br>





Shut down file_box system.
<a name="unfollow-3"></a>

<h3>unfollow/3</h3>





<pre>unfollow(UserId::integer(), Password::string(), FollowUserId::integer()) -> ok | {error, not_following} | {error, user_not_found}</pre>
<br></br>





Remove other user.
