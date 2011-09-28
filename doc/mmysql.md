Module mmysql
=============


<h1>Module mmysql</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute-1">execute/1</a></td><td>exec sql.</td></tr><tr><td valign="top"><a href="#execute-2">execute/2</a></td><td></td></tr><tr><td valign="top"><a href="#execute-3">execute/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td>for test.</td></tr><tr><td valign="top"><a href="#table_list-1">table_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#table_list-2">table_list/2</a></td><td>get table list.</td></tr><tr><td valign="top"><a href="#users_table-2">users_table/2</a></td><td>get table name for added user.</td></tr><tr><td valign="top"><a href="#users_table_list-1">users_table_list/1</a></td><td>get users table list on database.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="execute-1"></a>

<h3>execute/1</h3>





<pre>execute(SqlFormat::string()) -> #result_packet{}</pre>
<br></br>




exec sql
<a name="execute-2"></a>

<h3>execute/2</h3>





<pre>execute(SqlFormat::string() | atom(), Args::list()) -> #result_packet{}</pre>
<br></br>


<a name="execute-3"></a>

<h3>execute/3</h3>





<pre>execute(DBPool::atom(), SqlFormat::string(), Args::list()) -> #result_packet{}</pre>
<br></br>


<a name="init-0"></a>

<h3>init/0</h3>





`init() -> any()`



for test.<a name="table_list-1"></a>

<h3>table_list/1</h3>





<pre>table_list(Pattern::string()) -> [string()]</pre>
<br></br>


<a name="table_list-2"></a>

<h3>table_list/2</h3>





<pre>table_list(DBPool::atom(), Pattern::string()) -> [string()]</pre>
<br></br>




get table list
<a name="users_table-2"></a>

<h3>users_table/2</h3>





<pre>users_table(UserId::integer(), TableName::string()) -> string()</pre>
<br></br>




get table name for added user.
<a name="users_table_list-1"></a>

<h3>users_table_list/1</h3>





<pre>users_table_list(UserId::integer()) -> [string()]</pre>
<br></br>




get users table list on database.
