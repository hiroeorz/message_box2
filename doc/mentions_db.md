Module mentions_db
==================


<h1>Module mentions_db</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)







<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-tid">tid()</a></h3>




<pre>tid() = integer()</pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_timeline-3">get_timeline/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#insert_message_to_sqlite3-2">insert_message_to_sqlite3/2</a></td><td></td></tr><tr><td valign="top"><a href="#save_message_id-3">save_message_id/3</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="get_timeline-3"></a>

<h3>get_timeline/3</h3>





`get_timeline(Tid, DBPid, Count) -> any()`

<a name="init-1"></a>

<h3>init/1</h3>





`init(DBPid) -> any()`

<a name="insert_message_to_sqlite3-2"></a>

<h3>insert_message_to_sqlite3/2</h3>





`insert_message_to_sqlite3(DBPid, MessageIndex) -> any()`

<a name="save_message_id-3"></a>

<h3>save_message_id/3</h3>





<pre>save_message_id(Tid::<a href="#type-tid">tid()</a>, DBPid::pid(), MessageId::integer()) -> {ok, MessageId::integer()}</pre>
<br></br>


