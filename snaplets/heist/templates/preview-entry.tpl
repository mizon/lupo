<apply template="default">
  <h1>Preview</h1>
  <h2><entry-title/></h2>
  <entry-body/>
  <form id="entry-editor" action="/admin/${entry-id}/edit" method="post">
    <div class="buttons">
      <input type="submit" name="preview" value="Edit"/>
      <input type="submit" value="Submit"/>
    </div>
  </form>
</apply>
