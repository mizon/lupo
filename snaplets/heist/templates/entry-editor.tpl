<h2><lupo:editor-title/></h2>
<form id="entry-editor" method="post" action="${lupo:edit-path}">
<dl>
<label>
<dt>Title</dt>
<dd><input type="text" name="title" value="${lupo:entry-title}"/></dd>
</label>
<label>
<dt>Content</dt>
<dd><textarea name="body"><lupo:entry-body/></textarea></dd>
</label>
<dd class="submit"><input type="submit" name="action" value="Submit"/></dd>
</dl>
</form>