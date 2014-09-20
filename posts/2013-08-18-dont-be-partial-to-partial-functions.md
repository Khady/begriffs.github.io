---
title: Don't be partial to partial functions
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
Day 2. To be honest, today I tried basic stuff like implementing fizzbuzz in Haskell. I'll probably learn most by writing as many actual programs as I can. I'm sure all this typesystem stuff will fade into the background as I concentrate on writing useful things.<br /><br />However, who really wants to read a blog post showing a fizzbuzz implementation? I'm guessing you want something more thought-provoking as you wait for my basic language skills to get a little stronger.<br /><br />So here's one observation: I was reading about lists in <u>Learn You a Haskell</u> and I came across this example<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6259753"></code> <br /><div>This is an example of one of the <i>partial functions</i> defined in the standard library. It's called partial because it cannot handle all the values in its domain.</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6259756"></code> <br /><div>GHCI reports that the empty list is indeed in the domain of <span style="font-family: Courier New, Courier, monospace;">head</span> (namely, lists). Unlike Ruby or Clojure, Haskell does not commit Tony Hoare's "<a href="http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare">billion dollar mistake</a>" of allowing null values so there's no option when taking the head of an empty list but to throw an exception.</div><div><br /></div><div>The Haskell docs suggest avoiding partial functions altogether. The language provides alternative constructions. Let's look at one. Rather than test for an empty list and conditionally take its <span style="font-family: Courier New, Courier, monospace;">head</span>, the docs suggest we use a <span style="font-family: Courier New, Courier, monospace;">case</span> statement.</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6259759"></code> <br /><div>Only this doesn't entirely protect us at runtime! Consider this snippet where I "forget" to list a pattern in the case statement.</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6259763"></code> <br /><div>I would have liked the type system to notice my sloppiness ahead of time. After jumping on #haskell I learned that GHCI does actually notice something is awry, but I have to coax it to tell me.</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6259768"></code> <br /><div>The reason that a missing case is not considered an error is that it is not always feasible to determine all possibilities. Sometimes a function parameter can come from another module, and GHC does not do whole-program analysis. Some people defensively write a default case like this</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6259772"></code> <div>In the future I'll be on the lookout for partial functions inside conditionals, and will try to replace them with pattern matching.<br /><br />Oh, and coming back down to earth... would anyone like to share their most elegant implementation of fizzbuzz? :)</div>
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03871052333371767279">
  <div class="css-comment-name js-comment-name">
    Matthew Wraith
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-18T06:03:41.443Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    FizzBuzz, A Deep Navel to Gaze Into<br />http://dave.fayr.am/posts/2012-10-4-finding-fizzbuzz.html<br /><br />This is a fantastic post about FizzBuzz in Haskell.
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
    2013-08-19T02:21:38.417Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    data NEList a = NC a (NEList a) | NE a<br /><br />safeHead :: NEList a -&gt; a<br />safeHead (NC a as) = a<br />safeHead (NE a)    = a<br /><br />safeTail :: NEList a -&gt; NEList a<br />safeTail (NC a as) = as<br />safeTail (NE a) = NE a<br /><br />safeMap :: (a -&gt; b) -&gt; NEList a -&gt; NEList b<br />safeMap f (NC a as) = NC (f a) $ safeMap f as<br />safeMap f (NE a)    = NE (f a)<br /><br />showNEList :: Show a =&gt; NEList a -&gt; String<br />showNEList as = &quot;[&quot; ++ showNEList&#39; as ++ &quot;]&quot;<br />  where<br />    showNEList&#39; (NC a as) = show a ++ &quot;, &quot; ++ (showNEList&#39; as)<br />    showNEList&#39; (NE a)    = show a<br /><br />instance (Show a) =&gt; Show (NEList a) where<br />  show xs = showNEList xs<br /><br />lst = NC 1 (NC 2 (NC 3 (NE 4)))<br />
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
    2013-08-19T02:23:35.112Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    My preface to that kinda got eaten. I wrote this a while back when I was playing around with the idea of lists not having an empty case. The NEList (non-empty list) &quot;bottoms&quot; out with one element.<br /><br />I don&#39;t know if it makes sense, but it seems to work. You can map over it and do head/tail.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/14421199759390781840">
  <div class="css-comment-name js-comment-name">
    Andre
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-19T02:37:17.418Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    PArtial functions can also refer to partially applied functions, which is an entirely different concept. I have never heard of this definition of partial function before, at least in a Haskell context.
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
    2013-08-19T03:01:05.385Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Andre, in some languages, though not Haskell, you have to handle every possible input of a function. It would be like if I had a stoplight datatype and a broken &quot;next&quot; function:<br /><br />data Stoplight = Red | Yellow | Green<br /><br />next :: Stoplight -&gt; Stoplight<br />next Red = Green<br />next Green = Yellow<br /><br />And that&#39;s it. Notice that I don&#39;t ever describe how to handle the &quot;Yellow&quot; case:<br /><br />*Main&gt; next Yellow<br />*** Exception: :18:5-41: Non-exhaustive patterns in function next<br /><br />Ouch! So even though Haskell is saying &quot;next&quot; is from Stoplight to Stoplight, it is not. Not really.
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
    2013-08-19T04:20:25.535Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Andre, good to know. I used the word in a mathy way, and I wasn&#39;t aware of the other connotation of being partially applied. Do Haskellers have another word for a function that can&#39;t handle all its input?<br /><br />https://en.wikipedia.org/wiki/Partial_function
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/08373779241465143945">
  <div class="css-comment-name js-comment-name">
    Trenton Cronholm
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-23T07:57:01.802Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    More fizzbuzzes for your perusal here. http://rosettacode.org/wiki/FizzBuzz#Haskell<br />Full disclosure: I wrote the third one down, and it is a simplified version of the navel gazing article, except it uses the list monad rather than the maybe monad comprehension, so no language extensions are required.<br /><br />main = mapM_ (putStrLn . fizzbuzz) [1..100]<br /> <br />fizzbuzz n = <br />    show n &lt;|&gt; [fizz| n `mod` 3 == 0] ++ [buzz| n `mod` 5 == 0]<br /><br /><br />d &lt;|&gt; [] = d<br />_ &lt;|&gt; x = concat x<br />infixr 0 &lt;|&gt;<br />fizz = &quot;Fizz&quot;<br />buzz = &quot;Buzz&quot;
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
    2013-09-11T04:19:38.535Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    It certainly works!  The `semigroups` package defines an isomorphic type called NonEmpty, and it can be quite useful to model data.  It’s an instance of Monad and many other common type classes very much like regular lists, so it’s not too hard to work with, although it’s certainly less comfortable than regular lists with their syntactic sugar.  Check it out at .
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
    2013-09-11T04:20:48.798Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    It certainly works! The `semigroups` package defines an isomorphic type called NonEmpty, and it can be quite useful to model data. It’s an instance of Monad and many other common type classes very much like regular lists, so it’s not too hard to work with, although it’s certainly less comfortable than regular lists with their syntactic sugar. Check it out at http://hackage.haskell.org/packages/archive/semigroups/latest/doc/html/Data-List-NonEmpty.html
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
    2013-09-11T04:29:46.304Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    “Partial function” is pretty standard functional programming terminology, and it’s of course common in Haskell.  See, for example, http://www.haskell.org/haskellwiki/Partial_functions and http://www.haskell.org/haskellwiki/Avoiding_partial_functions<br /><br />The notion is especially present in languages that enforce totality.  See, for example, http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.Totality
  </div>
  <br/>
</div>
</div>
{% endraw %}
