<apply template="public">
  <bind tag="main-body">
    <div class="day">
      <day-title/>
      <entries/>
      <comments/>
      <form id="new-comment" method="post" action="${new-comment-url}">
        <input type="text" name="name" value="${comment-name}"/>
        <textarea name="body"><comment-body/></textarea>
        <input type="submit" value="Submit"/>
      </form>
    </div>
  </bind>
</apply>
