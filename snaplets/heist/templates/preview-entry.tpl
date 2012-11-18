<apply template="default">
  <bind tag="page-title"><lupo:preview-title/></bind>
  <h1><lupo:preview-title/></h1>
  <h2><lupo:entry-title/></h2>
  <lupo:rendered-body/>
  <form id="entry-editor" action="${lupo:submit-path}" method="post">
    <div class="buttons">
      <input type="hidden" name="title" value="${lupo:entry-title}"/>
      <input type="hidden" name="body" value="${lupo:entry-body}"/>
      <input type="submit" name="action" value="Edit"/>
      <input type="submit" name="action" value="Submit"/>
    </div>
  </form>
</apply>
