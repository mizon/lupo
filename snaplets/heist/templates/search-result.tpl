<apply template="default">
  <div id="header">
    <h1>Lupo Web Dairy</h1>
    <form action="/search" method="get">
      <input type="text" name="word"/>
      <input type="submit" value="Search"/>
    </form>
  </div>
  <table>
    <search-results/>
  </table>
</apply>
