<lupo:page-navigation/>
<div class="day">
  <h2><lupo:day-title/></h2>
  <lupo:entries/>
  <h3><lupo:comments-title/></h3>
  <comments/>
  <form id="new-comment" method="post" action="${lupo:new-comment-url}">
    <input type="text" name="name" value="${lupo:comment-name}"/>
    <textarea name="body"><lupo:comment-body/></textarea>
    <input type="submit" value="Submit"/>
  </form>
</div>
<lupo:page-navigation/>
