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



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#insert_message_to_mysql-2">insert_message_to_mysql/2</a></td><td></td></tr><tr><td valign="top"><a href="#restore_table-2">restore_table/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="insert_message_to_mysql-2"></a>

<h3>insert_message_to_mysql/2</h3>





`insert_message_to_mysql(UserId, MessageIndex) -> any()`

<a name="restore_table-2"></a>

<h3>restore_table/2</h3>





<pre>restore_table(User::#user{id = undefined | non_neg_integer(), status = atom(), pid = undefined | pid(), name = undefined | binary(), mail = undefined | binary(), password = undefined | binary()}, Tid::<a href="#type-tid">tid()</a>) -> ok</pre>
<br></br>


