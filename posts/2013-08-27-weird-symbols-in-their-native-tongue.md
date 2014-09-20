---
title: Weird symbols in their native tongue
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
Ever play charades? You need to make your audience understand the word in your head by acting it out. Well that's what we're going to do in a series of blog posts. Except the words I am thinking of don't exist in English. Yeah, people give them pseudo-math names, but those are uninformative too.<br />It doesn't help to name concepts with weird squiggles like "<span style="color: #999999; font-family: Trebuchet MS, sans-serif;">a</span><span style="font-size: xx-small;"><span style="color: #999999; font-family: Trebuchet MS, sans-serif;">p</span><span style="font-family: Helvetica Neue, Arial, Helvetica, sans-serif;"><i>p</i></span></span><i><span style="font-family: Helvetica Neue, Arial, Helvetica, sans-serif;">l</span><span style="font-family: 'Trebuchet MS', sans-serif; font-size: large;">i</span></i><span style="font-family: Trebuchet MS, sans-serif;">c</span><span style="font-family: Times, Times New Roman, serif;"><span style="font-size: x-small;">a</span><span style="color: #cccccc; font-size: x-large;">t</span></span><span style="font-family: 'Trebuchet MS', sans-serif; font-size: x-large;">i</span><span style="font-family: Trebuchet MS, sans-serif;">v</span><span style="font-family: 'Trebuchet MS', sans-serif; font-size: large;"><u>e</u> </span><span style="font-family: Arial, Helvetica, sans-serif;"><span style="font-size: large;">f</span><span style="font-size: x-small;">u</span></span><i><span style="font-family: Trebuchet MS, sans-serif;">n</span><span style="font-family: 'Trebuchet MS', sans-serif; font-size: large;">c</span></i><span style="color: #444444;"><span style="font-family: Georgia, Times New Roman, serif; font-size: large;">t</span><span style="font-family: 'Trebuchet MS', sans-serif; font-size: xx-small;">o</span></span><span style="font-family: Trebuchet MS, sans-serif;"><b>r</b></span>". If you look at old-fashioned math the names are always simple and are meant to suggest an analogy, because the primary function of math is to communicate precise intuition. So I'm going to find examples of <span style="font-family: Courier New, Courier, monospace;">&lt;$&gt;</span> and <span style="font-family: Courier New, Courier, monospace;">&lt;*&gt;</span> in action and see if we can spot the general pattern of how they work.<br /><br />But I simply don't have time to finish it tonight. I've spent my day after work struggling through <a href="http://www.soi.city.ac.uk/~ross/papers/Applicative.pdf">this paper</a> and looking for examples online. I have a few examples to work with now but not enough to for you (or me!) to solve the charades puzzle.<br /><br /><b>Want to help me out?</b> Comment on this article with some of your favorite, idiomatic, useful examples using applicative functors acting on various types.
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03541458411457497126">
  <div class="css-comment-name js-comment-name">
    Alexander Q
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-27T07:45:45.442Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Prelude&gt; (+) &lt;$&gt; [1,2,3] &lt;*&gt; [5,6,7]<br />[6,7,8,7,8,9,8,9,10]
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03541458411457497126">
  <div class="css-comment-name js-comment-name">
    Alexander Q
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-27T07:52:01.761Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    And another one<br /><br />-- add 5 to Maybe Int<br />f :: Maybe Int -&gt; Maybe Int<br />f x = (+5) &lt;$&gt; x<br /><br />Prelude&gt; f (Just 4)<br />Just 9<br /><br />I find myself writing<br />Prelude&gt; (+5) `fmap` (Just 4)<br />more often for such things.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03097657129811885396">
  <div class="css-comment-name js-comment-name">
    Chris
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-27T16:56:30.015Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    The intuition for &lt;$&gt; is that it is &quot;spelled&quot; similarly to its cousin, $. It also shares similar behavior:<br /><br />add1 $ 1 == 2<br /><br />add1 &lt;$&gt; [1] == [2]<br /><br />(&lt;$&gt;) &quot;lifts&quot; a normal function (a -&gt; b) so that it can be used on functors.<br /><br />You have to squint a little to see how &lt;*&gt; fits in here, but I think of it as applicative composition:<br /><br />add &lt;$&gt; [1]<br /><br />creates a function like f (Int -&gt; Int), though in this case it is really specialized to be [Int -&gt; Int]. The &lt;*&gt; function is a little adapter that lets you call a function &quot;inside&quot; the functor on another argument inside a functor:<br /><br />f (a -&gt; b) -&gt; f a ...<br /><br />Hope that helps.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05140850890065532317">
  <div class="css-comment-name js-comment-name">
    David
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-28T19:00:15.479Z
  </div>
  </div>
  <div class="css-comment-title js-comment-title">
    Parsers.


fooP :: Parser Foo
fooP = Foo &lt;$&gt;...
  </div>
  <div class="css-comment-content js-comment-content">
    Parsers.<br /><br /><br />fooP :: Parser Foo<br />fooP = Foo &lt;$&gt; someP &lt;*&gt; otherP<br /><br />
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
    2013-08-29T05:10:30.397Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    What does this mean? Can you show it in context?
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
    2013-08-29T05:11:27.755Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Yeah it does, thanks. I wrote another post where I try out more of these kind of examples.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03097657129811885396">
  <div class="css-comment-name js-comment-name">
    Chris
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-30T01:00:37.279Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    That sort of thing tripped me up for a long time. It uses the same sort of trickery as this:<br /><br />-- binary int addition, totally normal (nothing up my sleeve)<br />add :: Int -&gt; Int -&gt; Int<br /><br />Next you apply it to a single int:<br /><br />-- application &quot;removes&quot; one of the ints, the new type is still a function, but<br />-- from (Int -&gt; Int) now.<br />add 1 :: Int -&gt; Int<br /><br />Finally, the second application yields an Int<br />add 1 2 :: Int<br /><br />One last thing, though it doesn&#39;t seem to matter, the associativity is vitally important. Haskell understands &#39;add 1 2&#39; like this:<br /><br />((add 1) 2) -- this&#39;ll be important later<br /><br />If you&#39;re okay with the above the functor version works the same way.<br /><br />-- partially apply add to &quot;functorized&quot; (in a list) Int<br />add &lt;$&gt; [1] :: [Int -&gt; Int]<br /><br />Now to apply that thing to the remaining [Int]. Here&#39;s the one and only twist. You now have this sort of thing (in type-world):<br /><br />[Int -&gt; Int] ??? [Int]<br /><br />Which by looking at it, you realize that it *has* to be a function that does this:<br /><br />[Int -&gt; Int] -&gt; [Int] -&gt; [Int]<br /><br />If I had to define it, it&#39;d look like this:<br /><br />(f:[]) ??? (a:[]) = [f a]<br /><br />Functors are more general, but that&#39;s the idea.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03097657129811885396">
  <div class="css-comment-name js-comment-name">
    Chris
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-30T01:04:20.233Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    This comment has been removed by the author.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03097657129811885396">
  <div class="css-comment-name js-comment-name">
    Chris
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-30T01:05:47.334Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Oops, I left out what the &quot;it&quot; was. The thing that does:<br /><br />f (a -&gt; b) -&gt; f a -&gt; f b<br /><br />is called &lt;*&gt;.
  </div>
  <br/>
</div>
</div>
{% endraw %}
