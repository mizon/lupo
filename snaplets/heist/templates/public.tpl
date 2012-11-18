<apply template="default">
  <div id="header">
    <a href="/"><h1><lupo:header-title/></h1></a>
    <form id="search" action="/search" method="get">
      <input id="search-field" type="text" name="word"/>
      <input id="search-button" type="submit" value="Search"/>
    </form>
    <div class="navigation">
      <lupo:page-navigation/>
    </div>
  </div>
  <div id="main">
    <lupo:main-body/>
  </div>
  <div id="footer">
    <div class="navigation">
      <lupo:page-navigation/>
    </div>
    <lupo:footer-body/>
  </div>
</apply>
