<apply template="default">
  <h1><edit-title/></h1>
  <form id="entry-editor" action="${submit-path}" method="post">
    <label>Title: <input type="text" name="title" value="${default-title}"/></label><br/>
    <textarea name="body"><default-body/></textarea><br/>
    <div class="buttons">
      <input type="submit" name="action" value="Preview"/>
      <input type="submit" name="action" value="Submit"/>
    </div>
  </form>
</apply>
