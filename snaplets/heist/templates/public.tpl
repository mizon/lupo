<apply template="default">
  <div id="header">
    <h1><header-title/></h1>
    <div class="navigation">
      <page-navigation/>
      <form id="search" action="/search" method="get">
        <input id="search-field" type="text" name="word"/>
        <input id="search-button" type="submit" value="Search"/>
      </form>
    </div>
  </div>
  <div id="main">
    <main-body/>
  </div>
  <div id="footer">
    <div class="navigation">
      <page-navigation/>
    </div>
    <footer-body/>
  </div>
</apply>
