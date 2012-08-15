<apply template="default">
  <bind tag="page-title"><preview-title/></bind>
  <h1><preview-title/></h1>
  <h2><entry-title/></h2>
  <rendered-body/>
  <form id="entry-editor" action="${submit-path}" method="post">
    <div class="buttons">
      <input type="hidden" name="title" value="${entry-title}"/>
      <input type="hidden" name="body" value="${entry-body}"/>
      <input type="submit" name="action" value="Edit"/>
      <input type="submit" name="action" value="Submit"/>
    </div>
  </form>
</apply>
