<apply template="default">
  <div id="header">
    <a href="/"><h1><header-title/></h1></a>
    <form id="search" action="/search" method="get">
      <input id="search-field" type="text" name="word"/>
      <input id="search-button" type="submit" value="Search"/>
    </form>
  </div>
  <div id="main">
    <main-body/>
  </div>
  <div id="footer">
    <footer-body/>
  </div>
</apply>
