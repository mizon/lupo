<apply template="default"><div id="header">
<h1><a href="${lupo:top-page-path}"><lupo:site-title/></a> <a href="${lupo:feed-path}"><img src="${lupo:feed-icon-path}" alt="Feed" width="14" height="14"/></a></h1>
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