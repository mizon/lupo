<h2><lupo:preview-title/></h2>
<h2>[Preview]</h2>
<div class="day">
<lupo:preview-body/>
</div>
<form id="entry-editor" method="post" action="${lupo:edit-path}">
<dl>
<dd class="submit">
<input type="hidden" name="title" value="${lupo:entry-title}"/>
<input type="hidden" name="body" value="${lupo:entry-body}"/>
<input type="submit" name="action" value="Edit"/>
<input type="submit" name="action" value="Submit"/>
</dd>
</dl>
</form>