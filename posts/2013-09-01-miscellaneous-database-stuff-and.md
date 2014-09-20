---
title: Miscellaneous database stuff and an interesting book
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
I was wrong when I said before that the Groundhog ORM lacks documentation. It's on Hackage, although the docs for the migration DSL are in the <span style="font-family: Courier New, Courier, monospace;">groundhog-th </span>package. So tonight I sent a pull request to the Groundhog repo to add a readme with my simple usage example and links to the other docs. Might be helpful for people who stumble on the project through Github rather than Hackage.<br /><br />Given the extensive docs for Groundhog, I really don't need to detour to make tutorials. I should probably keep focusing on following the Yesod book. The Yesod section on persistence actually mentions Groundhog,<br /><blockquote class="tr_bq">"Earlier versions of Persistent made much heavier usage of Template Haskell. Starting with 0.6, there is a new architecture inspired by the groundhog package. This approach uses phantom types to carry a lot of the burden."</blockquote>Apparently much of the Groundhog goodness is now incorporated into Yesod's persistence library. Tonight I started playing with the models in <span style="font-family: Courier New, Courier, monospace;">config/models</span> but I have nothing interesting to report. It's just the stuff you will find in the Yesod book.<br /><br />However, speaking of books, somebody recommended one to me in my earlier post about using mathematical structures in code. It's called <a href="http://amzn.com/032163537X">Elements of Programming</a> and now it's being shipped to my house. The authors discuss general algorithms with actions and orbits (you might remember these from group theory). ordered structures, rearrangements, partitioning, "coordinate" structures...general ideas that use the taxonomy of abstract algebra. The examples in the book are written in C++, but think how smoothly they will work implemented in Haskell. It's going to be beautiful.
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/13809187507726378890">
  <div class="css-comment-name js-comment-name">
    Boris Lykah
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-09-03T19:14:45.311Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Thank you, reading your Groundhog experience was inspiring. Persistent had adopted the GADT field constructors and operators, but the relational goodness - composite keys, indexes, circular references, etc. is specific to Groundhog. While there are Hackage docs and I will publish a tutorial soon, it would be a huge win if you share your experience about using Groundhog together with Yesod.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/16208489588423816254">
  <div class="css-comment-name js-comment-name">
    Manuel Gómez
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-09-11T06:12:00.303Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Boris, your work on Groundhog is indeed wonderful and quite essential for proper use of relational databases — I use it in my day job and it’s been wonderful so far.  Thanks!<br /><br />On your last point, I myself have struggled with Yesod integration, and although I’m getting there, there’s still a few bits that I haven’t been able to figure out.  Are there any examples of Yesod integration with Groundhog beyond the monad integration example in Git?  If there aren’t any, I’ll be happy to take a shot at writing a tutorial or somesuch once I finish figuring it out myself.  In that case, do you —and Joe, and anybody else— have any specific suggestions for such a guide?
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/13809187507726378890">
  <div class="css-comment-name js-comment-name">
    Boris Lykah
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-09-12T15:51:03.205Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Glad to hear this!<br /><br />I don&#39;t have much of experience with Yesod, but if you have any Groundhog questions, don&#39;t hesitate to contact me. Integration tutorial is a great idea.
  </div>
  <br/>
</div>
</div>
{% endraw %}
