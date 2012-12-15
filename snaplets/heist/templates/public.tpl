<apply template="default"><div id="header">
<h1><a href="${lupo:top-page-path}"><lupo:site-title/></a></h1>
<form id="search" action="${lupo:search-path}" method="get">
<label>Search: <input id="search-field" type="text" name="word"/></label>
</form>
</div>
<div id="main">
<lupo:main-body/>
</div>
<div id="footer">
<lupo:footer-body/>
</div></apply>