<apply template="default">
  <bind tag="page-title"><lupo:edit-title/></bind>
  <h1><lupo:edit-title/></h1>
  <form id="entry-editor" action="${lupo:submit-path}" method="post">
    <label>Title: <input type="text" name="title" value="${lupo:default-title}"/></label><br/>
    <textarea name="body"><lupo:default-body/></textarea><br/>
    <div class="buttons">
      <input type="submit" name="action" value="Preview"/>
      <input type="submit" name="action" value="Submit"/>
    </div>
  </form>
</apply>
