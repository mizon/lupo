<lupo:js-libs/>
<div id="login-form">
<h2>Login</h2>
<form method="post" action="${lupo:login-url}" onsubmit="lupo.createResponse();">
<dl>
<input id="login-challenge" type="hidden" value="${lupo:challenge}"/>
<input id="login-pass-crypto" type="hidden" name="pass"/>
<label><dt>Pass</dt><dd><input id="login-pass-input" type="password"/></dd></label>
<dd class="submit"><button type="submit">Login</button></dd>
</dl>
</form>
</div>