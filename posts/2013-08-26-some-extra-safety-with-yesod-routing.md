---
title: Some extra safety with Yesod routing
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
Today I played with adding some routes to my sample chat API server. I have nothing original to report, it's all just stuff in the <a href="http://www.yesodweb.com/book/routing-and-handlers">Yesod Book</a>. But one interesting thing I noticed about routing is that you add constraints by using types -- either the standard ones or your own. This allows you to restrict the valid URLs for your app without much boilerplate code.<br /><br />For instance, imagine a user view in your app, which needs a numerical user id passed in the route. Add a line in <span style="font-family: Courier New, Courier, monospace;">config/routes</span> like<br /><br /><span style="font-family: Courier New, Courier, monospace;">/user/#Integer UserR GET</span><br /><div><br /></div><div>If I then hit the app at <span style="font-family: Courier New, Courier, monospace;">/user/hi</span> it gives me a 404 immediately. It doesn't have to run any of my code which would probably try to run a database query. It just knows that the route is hopeless because it can't turn "hi" into an Integer.</div><div><br /></div><div>Using your own types and <a href="http://www.haskell.org/haskellwiki/Smart_constructors">smart constructors</a> you can easily make validations which will be used both in your controllers (called <i>handlers</i> in Yesod land) and in the generated routes. Talk about <a href="https://en.wikipedia.org/wiki/Don't_repeat_yourself">DRY</a>, this single source of truth coordinates several pieces of your app. You know what's also dry about Yesod? Before sending requests off to your handlers it goes through a round of URL parsing to make the paths canonical. If someone hits <span style="font-family: Courier New, Courier, monospace;">/user//1/</span> the framework will actually send a 301 redirect to <span style="font-family: Courier New, Courier, monospace;">/user/1</span>, ensuring your search engine juice stays juicy.</div><div><br /></div><div>What other types might be useful for routing validation? Maybe something like ModernYear that ensures a year after 1900 for people's birthdays.</div><div><br /></div><div>Anyway, while thinking about it, I came across a crazy bit of Haskell research called <i>type-level natural numbers</i>. The implementations so far look super awkward but the idea is cool: to allow programmers to restrict the numbers used in constructing a type and to allow the compiler to enforce the restriction. So using the wrong values or even creating a situation which might use the wrong values could be stopped before you run the program. Yeah, that sucked up lots of my time tonight and didn't lead to any blogworthy epiphanies.</div>
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/00339132814373626134">
  <div class="css-comment-name js-comment-name">
    Adam Baker
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-29T23:05:12.549Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    http://www.willamette.edu/~fruehr/haskell/evolution.html<br /><br />Check out Static Haskell programmer for more type-level natural numbers.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05440774752453573594">
  <div class="css-comment-name js-comment-name">
    Joe Nelson
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-29T23:17:28.933Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Ha, yeah exactly. I think I&#39;d rather live with runtime uncertainty than go down that route.
  </div>
  <br/>
</div>
</div>
{% endraw %}
