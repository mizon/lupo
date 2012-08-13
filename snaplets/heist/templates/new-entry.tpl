<apply template="default">
  <h1>New Entry</h1>
  <form id="entry-editor" action="/admin/new" method="post">
    <label>Title: <input type="text" name="title"/></label><br/>
    <textarea name="body"/>
    <div class="buttons">
      <input type="submit" name="preview" value="Preview"/>
      <input type="submit" value="Submit"/>
    </div>
  </form>
</apply>
