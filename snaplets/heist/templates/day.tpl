<lupo:page-navigation/>
<div class="day">
<h2><lupo:day-title/></h2>
<lupo:entries/>
</div>
<lupo:if-commented><div id="comments">
<h3><lupo:comments-caption/></h3>
<dl>
<lupo:comments/>
</dl>
</div>
</lupo:if-commented><div id="new-comment">
<h3><lupo:new-comment-caption/></h3>
<lupo:new-comment-errors/><form method="post" action="${lupo:new-comment-url}">
<dl>
<label>
<dt><lupo:name-label/></dt>
<dd><input type="text" name="name" value="${lupo:comment-name}" size="50"/></dd>
</label>
<label>
<dt><lupo:content-label/></dt>
<dd>
<textarea class="comment-content" name="body" rows="8"><lupo:comment-body/></textarea>
</dd>
</label>
<dd class="submit">
<input type="submit" value="Submit"/>
</dd>
</dl>
</form>
</div>
<lupo:new-comment-notice/>
<lupo:page-navigation/>