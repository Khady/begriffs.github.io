---
title: Directional Quotes
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
<br />Nesting ambidextrous quotes requires either escaping&nbsp;<strong>"he said \"hi\""</strong>&nbsp;or alternating with single quotes&nbsp;<strong>"he said 'hi'"</strong>. Wouldn't using book-style quotes&nbsp;<strong><code>“</code>he said&nbsp;<code>“</code>hi<code>””</code></strong>&nbsp;be easier? If this all seems unimportant, think of it this way: imagine how awkward it is to use the pipe character '<strong>|</strong>' as ambidextrous parentheses.&nbsp;<strong>|2*\|3+4\|| = 14</strong>.<br /><br />Not only does it seem easier to use left and right quotes, it might even improve application security. SQL injection often uses quoting tricks, which are easily unmasked when properly expressed. Injecting value =<br /><blockquote>anything' OR 'x'='x</blockquote>in<br /><blockquote>SELECT * FROM table WHERE column = 'value'</blockquote>will create an unrestricted listing of the table. However, that malicious value's quotes fail to balance when expressed as left and right quotes:<br /><blockquote>anything<code>”</code>&nbsp;OR&nbsp;<code>“</code>x<code>”</code>=<code>“</code>x</blockquote>
</div>
{% endraw %}
