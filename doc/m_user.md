Module m_user
=============


<h1>Module m_user</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`shin@u652212.xgsfmg23.imtp.tachikawa.mopera.net`](mailto:shin@u652212.xgsfmg23.imtp.tachikawa.mopera.net)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#follow-3">follow/3</a></td><td>
Follow other user.</td></tr><tr><td valign="top"><a href="#get_home_timeline-2">get_home_timeline/2</a></td><td>
Get home timeline list.</td></tr><tr><td valign="top"><a href="#get_mentions_timeline-2">get_mentions_timeline/2</a></td><td>
Get home timeline list.</td></tr><tr><td valign="top"><a href="#get_message-2">get_message/2</a></td><td>
Get Message from user process.</td></tr><tr><td valign="top"><a href="#get_sent_timeline-2">get_sent_timeline/2</a></td><td>
Get sent timeline list.</td></tr><tr><td valign="top"><a href="#is_following-2">is_following/2</a></td><td>
Check follow state.</td></tr><tr><td valign="top"><a href="#save_to_home-3">save_to_home/3</a></td><td>
Save to users home table.</td></tr><tr><td valign="top"><a href="#save_to_mentions-2">save_to_mentions/2</a></td><td>
Save to users mentions table.</td></tr><tr><td valign="top"><a href="#send_message-3">send_message/3</a></td><td>
Send Message.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#unfollow-3">unfollow/3</a></td><td>
Remove other user.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="follow-3"></a>

<h3>follow/3</h3>





<pre>follow(Pid::pid(), Password::string(), UserId::integer()) -> ok | {error, already_following} | {error, not_found}</pre>
<br></br>





Follow other user.
<a name="get_home_timeline-2"></a>

<h3>get_home_timeline/2</h3>





<pre>get_home_timeline(Pid::pid(), Count::integer()) -> [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]</pre>
<br></br>





Get home timeline list.
<a name="get_mentions_timeline-2"></a>

<h3>get_mentions_timeline/2</h3>





<pre>get_mentions_timeline(Pid::pid(), Count::integer()) -> [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]</pre>
<br></br>





Get home timeline list.
<a name="get_message-2"></a>

<h3>get_message/2</h3>





<pre>get_message(Pid::pid(), MessageId::integer()) -> #message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}</pre>
<br></br>





Get Message from user process.
<a name="get_sent_timeline-2"></a>

<h3>get_sent_timeline/2</h3>





<pre>get_sent_timeline(Pid::pid(), Count::integer()) -> [#message{id = undefined | integer(), message_id = undefined | integer(), text = undefined | binary(), datetime = undefined | tuple(), user = any()}]</pre>
<br></br>





Get sent timeline list.
<a name="is_following-2"></a>

<h3>is_following/2</h3>





<pre>is_following(Pid::pid(), UserId::integer()) -> true | false</pre>
<br></br>





Check follow state.
<a name="save_to_home-3"></a>

<h3>save_to_home/3</h3>





<pre>save_to_home(Pid::pid(), MessageId::integer(), IsReplyTo::{{true, #user{id = undefined | non_neg_integer(), status = atom(), pid = undefined | pid(), name = undefined | binary(), mail = undefined | binary(), password = undefined | binary()}} | {false, nil}}) -> ok</pre>
<br></br>





Save to users home table.
This function called from other m_user process.
<a name="save_to_mentions-2"></a>

<h3>save_to_mentions/2</h3>





<pre>save_to_mentions(Pid::pid(), MessageId::integer()) -> {ok, MessageId::integer()}</pre>
<br></br>





Save to users mentions table.
This function called from other m_user process.
<a name="send_message-3"></a>

<h3>send_message/3</h3>





<pre>send_message(Pid::pid(), Password::string(), TextBin::binary()) -> {ok, MessageId::integer()}</pre>
<br></br>





Send Message.
<a name="start_link-1"></a>

<h3>start_link/1</h3>





<pre>start_link(UserId::integer()) -> {ok, Pid::pid()} | ignore | {error, Error::atom()}</pre>
<br></br>





Starts the server
<a name="unfollow-3"></a>

<h3>unfollow/3</h3>





<pre>unfollow(Pid::pid(), Password::string(), UserId::integer()) -> {ok, deleted} | {error, not_following} | {error, not_found}</pre>
<br></br>





Remove other user.
