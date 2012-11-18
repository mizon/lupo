<lupo:page-navigation/>
<div class="day">
  <h2><lupo:day-title/></h2>
  <lupo:entries/>
</div>
<lupo:comments/>
<div id="new-comment">
  <h3><lupo:new-comment-caption/></h3>
  <form method="post" action="${lupo:new-comment-url}">
    <label>
      <lupo:name-label/><br/>
      <input type="text" name="name" value="${lupo:comment-name}" size="50"/><br/>
    </label>
    <label>
      <lupo:content-label/><br/>
      <textarea class="comment-content" name="body" rows="8"><lupo:comment-body/></textarea><br/>
    </label>
    <div class="submit">
      <input class="submit-button" type="submit" value="Submit"/>
    </div>
  </form>
</div>
<lupo:page-navigation/>
