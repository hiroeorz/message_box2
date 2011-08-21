Module message_db
=================


<h1>Module message_db</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)







<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-tid">tid()</a></h3>




<pre>tid() = integer()</pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close_tables-1">close_tables/1</a></td><td>close ets table.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>initialize.</td></tr><tr><td valign="top"><a href="#save_message-4">save_message/4</a></td><td>save message to ets and sqlite3 database.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="close_tables-1"></a>

<h3>close_tables/1</h3>





<pre>close_tables(Tid::<a href="#type-tid">tid()</a>) -> true</pre>
<br></br>




close ets table.
<a name="init-1"></a>

<h3>init/1</h3>





<pre>init(DBPid::pid()) -> {ok, Tid::<a href="#type-tid">tid()</a>}</pre>
<br></br>




initialize.
<a name="save_message-4"></a>

<h3>save_message/4</h3>





<pre>save_message(Tid::<a href="#type-tid">tid()</a>, DBPid::pid(), User::#user{id = undefined | integer(), status = atom(), pid = undefined | atom(), name = undefined | term(), mail = undefined | string(), password = undefined | string()}, Msg::binary()) -> {ok, MessageId::integer()}</pre>
<br></br>




save message to ets and sqlite3 database.
