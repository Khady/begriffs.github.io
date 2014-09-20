---
title: Try GHCi on Acid and watch function arguments melt away
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
<div class="separator" style="clear: both; text-align: center;"><a href="http://3.bp.blogspot.com/-hTj4AbvTKuU/UhLJCxsDE_I/AAAAAAAAADg/3DoRPfKlOUw/s1600/trippy.jpg" imageanchor="1" style="clear: right; float: right; margin-bottom: 1em; margin-left: 1em;"><img border="0" height="150" src="http://3.bp.blogspot.com/-hTj4AbvTKuU/UhLJCxsDE_I/AAAAAAAAADg/3DoRPfKlOUw/s200/trippy.jpg" width="200" /></a></div>Day 5. I'm doing exercises from <a href="http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems">H-99</a>, and challenging myself to make answers that don't use named function arguments. Although arguments can help describe a function's purpose they are often just clutter. In fact uniformly renaming a variable in a function has no effect on the function's output, a phenomenon known as&nbsp;α-congruence in lambda calculus.<br /><br />Because this is Haskell we're talking about, my idle preoccupation (finding a point-free representation of functions) is of course already someone's PhD thesis. I want to show you a cool tool that will help you strip out extraneous variables and examine the interesting results.<br /><br />To see our code melt down our monitors and have the variables fall out like loose teeth we need to use GHCi on Acid. If you're using a mac then&nbsp;turn on, tune in, and drop out.<br /><br />The installation procedure is slightly different than what I read in the docs. Assuming your machine has <a href="http://brew.sh/">homebrew</a> and <a href="http://www.haskell.org/cabal/">cabal</a> installed, run this:<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6277338"></code> That will take a while. Now create a <span style="font-family: Courier New, Courier, monospace;">~/.ghci</span> file with the following<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6277330"></code> Your&nbsp;<span style="font-family: Courier New, Courier, monospace;">setLambdabotHome</span> will differ from mine. You may only need to change your home directory and the ghc version number to make it work. Worst case just search for the <span style="font-family: Courier New, Courier, monospace;">bin</span> folder of <span style="font-family: Courier New, Courier, monospace;">lambdabot</span>.<br /><br />Now, if everything was successful, your GHCi prompt should say <span style="font-family: Courier New, Courier, monospace;">Prelude,&nbsp;GOA&gt;</span> and we can begin using the trippy <i>pl</i> command. Let's start with a low dose and eliminate a variable that we all know shouldn't be there<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6276908"></code> It detected that our anonymous function which takes a value only to pass it to <span style="font-family: Courier New, Courier, monospace;">succ</span> behaves no differently from <span style="font-family: Courier New, Courier, monospace;">succ</span> itself. The Greek alphabet lovers among you will be happy to know this is called η-conversion. Now a harder example. Let's make a function similar to <span style="font-family: Courier New, Courier, monospace;">(!!)</span> but which uses 1-based indexing. I can think of one that takes two arguments, but let's let :pl attack it.<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6276983"></code> What an interesting result. Do you think it's an improvement? I kind of like it, built as it is purely from simple functions. However its meaning doesn't jump out at me. Let's think it through.<br /><br />Fist, what kind of a function is <span style="font-family: Courier New, Courier, monospace;">(. pred)</span><span style="font-family: Times, Times New Roman, serif;"> and how is it suitable to compose with </span><span style="font-family: Courier New, Courier, monospace;">(!!)</span><span style="font-family: Times, Times New Roman, serif;">?</span><br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6277069"></code> Starting with the latter, it's a function which takes a list and returns a new function -- which is expecting an Integer argument and will give back a list item. Now <span style="font-family: Courier New, Courier, monospace;">(. pred)</span> takes a function, and given this one it will return a function with the same domain. In this case a list. And what is the range? Functions from Int to list items. Hey, that's the what we wanted originally with our 1-based element getter.<br /><br />Honestly I can't say it's easy for me to think through this strange composition, but many of the point-free results do look pretty and symmetric. For instance, check out how to modify a function's second or first argument<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6277290"></code> It looks natural this way. Now the <span style="font-family: Courier New, Courier, monospace;">:pl</span> command goes beyond stringing together composition and can summon strange functions I don't know about yet. For instance, look how it rewrites a function that checks if a list is a palindrome.<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6277305"></code> In either direction we get some crazy moon language! Do any of you, my dear readers, have ways to make sense of the point-free idioms? Perhaps I shouldn't use the exotic ones and should stick to old fashioned named arguments.<br /><br /><br />
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/16208489588423816254">
  <div class="css-comment-name js-comment-name">
    Manuel Gómez
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-09-11T04:43:19.283Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    `pl` makes use of the Monad instance for functions, often called the “reader” instance, since it works precisely like a Reader monad.  I found this article very useful when I stumbled upon this concept:<br />http://blog.ezyang.com/2010/07/implicit-parameters-in-haskell/<br /><br />Manually converting functions into the pointfree style is mostly pointless for doing actual programming (although it can be useful to avoid code clutter at times), but it’s extremely helpful for developing intuition about Haskell, its copious use of higher-order functions, and the type system — it’s a great way to learn to interpret type errors.<br /><br />A very useful resource for this sort of experiment is this collection of combinators:<br />http://hackage.haskell.org/package/data-aviary-0.2.3
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/12619328155543685238">
  <div class="css-comment-name js-comment-name">
    David Hussey
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2014-05-25T12:24:52.603Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Writing is an art form that reaches a multitude of people from all walks of life, different cultures, and age group. As a writer, it is not about what you want. <a href="http://www.coreforceworldwide.com/are-you-trying-to-figure-out-the-words" rel="nofollow">dictionary of idioms</a>
  </div>
  <br/>
</div>
</div>
{% endraw %}
