<apply template="default">
<bind tag="page-title">Lupo Admin</bind>
<h1>Lupo Admin</h1>
<h2>Entries</h2>
<p><a href="/admin/new">New Entry</a></p>
<table id="entries-table">
<tr><th class="date">Date</th><th>Title</th><th class="operation"></th></tr>
<lupo:entries-list/>
</table>
</apply>