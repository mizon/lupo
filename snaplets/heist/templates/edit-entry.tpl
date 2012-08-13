<apply template="default">
  <h1>Edit Entry</h1>
  <form id="entry-editor" action="/admin/${entry-id}/edit" method="post">
    <label>Title: <input type="text" name="title" value="${default-title}"/></label><br/>
    <textarea name="body"><default-body/></textarea><br/>
    <div class="buttons">
      <input type="submit" name="preview" value="Preview"/>
      <input type="submit" value="Submit"/>
    </div>
  </form>
</apply>
