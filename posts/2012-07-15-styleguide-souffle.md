---
title: Styleguide soufflé
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
<br /><span style="background-color: white;">Adam Braus (<a href="https://twitter.com/ajbraus">@ajbraus</a>) designs software with a clear vision of what he wants to accomplish, and especially what real value he will create for users. He was immediately interested in the ideas presented <a href="http://blog.begriffs.com/2012/07/dont-play-css-tetris.html">here</a>&nbsp;but still yearned for something a little simpler, a little lighter...a styleguide soufflé.</span><br /><br />Recently he has been immersed in Rails development with Mike Fenchel (<a href="https://twitter.com/mfenchel">@mfenchel</a>) creating a social scheduling web app. They are beginning to define its CSS styles and want to try doing inside-out design with a styleguide.<br /><br />Adam and I searched for a simple Rails-specific styleguide template, but didn't find any. Wouldn't it be natural and nice to have a Rails generator for this?<br /><br /><code>&nbsp;&nbsp;rails generate styleguide<br /></code><br />We certainly thought so, and we made a gem called styleguide_rails. It is now <a href="https://rubygems.org/gems/styleguide_rails">released on RubyGems</a> and provides a generator to scaffold a controller, a view, and routes for your guide. To add a style module, create a partial in <code>app/views/styleguide/</code> and visit <code>http://your_app/styleguide</code>. You'll see the rendered element and its raw HTML next to one another. It's a convenient way to document your preferred HTML snippet to create various modules like login forms.<br /><br />The basic functionality is there, but there are some obvious ways to improve the gem. Among other things, it could use these features:<br /><ul><li><span style="background-color: white;">&nbsp;syntax highlighting for the HTML snippets</span></li><li><span style="background-color: white;">&nbsp;hiding the /styleguide route in production</span></li><li><span style="background-color: white;">&nbsp;handsome style for the guide itself</span></li><li><span style="background-color: white;">&nbsp;ERB evaluation to include one module in another</span></li></ul>I'll work on addressing these issues, but feel free to fork the code <a href="https://github.com/begriffs/styleguide_rails">on GitHub</a> and send a pull request.
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/17987139810545770983">
  <div class="css-comment-name js-comment-name">
    bendycode
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2012-07-16T11:23:05.243Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Great idea!<br /><br />Very nice.
  </div>
  <br/>
</div>
</div>
{% endraw %}
