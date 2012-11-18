<div class="day">
  <h2><day-title/></h2>
  <entries/>
  <h3><comments-title/></h3>
  <comments/>
  <form id="new-comment" method="post" action="${new-comment-url}">
    <input type="text" name="name" value="${comment-name}"/>
    <textarea name="body"><comment-body/></textarea>
    <input type="submit" value="Submit"/>
  </form>
</div>
