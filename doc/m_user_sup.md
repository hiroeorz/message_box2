Module m_user_sup
=================


<h1>Module m_user_sup</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_all_users-0">start_all_users/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the supervisor.</td></tr><tr><td valign="top"><a href="#start_user-1">start_user/1</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="start_all_users-0"></a>

<h3>start_all_users/0</h3>





`start_all_users() -> any()`

<a name="start_link-0"></a>

<h3>start_link/0</h3>





<pre>start_link() -> {ok, Pid} | ignore | {error, Error}</pre>
<br></br>





Starts the supervisor
<a name="start_user-1"></a>

<h3>start_user/1</h3>





`start_user(User) -> any()`

