---
title: Tricking Haskell into being dynamic
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
Day 1. What do outsiders think of this language? Speaking for myself, I've always heard it has a strict type system that people learn to love. Today I want to use a simple example to bend the rules and get to the bottom of it.<br /><div><br /></div><div>I began&nbsp;<a href="http://learnyouahaskell.com/">Learn You a Haskell</a>, and started typing in mundane stuff.<br /><div><br /></div><div><span style="font-size: xx-small;">(Note that I am displaying code in embedded Gists, which may not appear correctly in RSS. Let me know if it's a problem.)</span></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6251728"></code> <br /><div><div>The language of course gently steps in and protects me. Comparing <span style="font-family: Courier New, Courier, monospace;">Num</span><span style="font-family: inherit;">s</span> and a list of <span style="font-family: Courier New, Courier, monospace;">Char</span>s is nonsense. Yet...what if we wanted Haskell to evaluate this comparison and return false? I'm fully aware this is a bad idea, but it might teach us about how Haskell's type system really works.</div><div><br /></div><div>So let's unwind how the type error above originates. What does the operator <span style="font-family: Courier New, Courier, monospace;">==</span> expect?</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6252551"></code> <br /><div>This is really the crux of the problem, it grabs onto a type (which incidentally must be an instance of the <span style="font-family: Courier New, Courier, monospace;">Eq</span> type class) and expects it for both arguments. I guess we have to give up, because a number and a string simply cannot be simultaneously substituted for <span style="font-family: Courier New, Courier, monospace;">a</span> in the type signature above unless we can somehow make a new type which tricks Haskell. That would be magical, wouldn't it? Something like</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6252569"></code> <br /><div>Luckily most types are instances of <span style="font-family: Courier New, Courier, monospace;">Show</span>, and provide a <span style="font-family: Courier New, Courier, monospace;">show</span> function to represent themselves as strings. Turns out this is what the repl does when it prints values back. So we could compare <span style="font-family: Courier New, Courier, monospace;">Show</span> instances by comparing their string representation.</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6252581"></code> <br /><div>Maybe this is good enough. But maybe we can put it inside a magical type and partially obscure the implementation. We could make different data constructors and write all kinds of messy combinations for implementing equality tests.</div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6252603"></code> <br /><div>I'm sorry to inflict that code on you. Let's erase it from our minds using <i>existential types</i>. In quest of the <span style="font-family: Courier New, Courier, monospace;">Magic</span> recipe, I jumped on #haskell and things got crazy. It's filled with friendly and alarmingly smart people.</div><div><br /></div><div>Here's what I learned.&nbsp;<span style="font-family: Times, Times New Roman, serif;">If we leave the&nbsp;<span style="background-color: white; font-size: 16px; line-height: 24px;">Haskell98</span></span><span style="background-color: white; line-height: 24px;"><span style="font-family: Times, Times New Roman, serif;"> standard behind, we can open up a trap door in GHCI by starting it with the</span><span style="font-family: inherit; font-size: 16px;">&nbsp;</span></span><span style="background-color: #f9f9f9; font-family: monospace, Courier; font-size: 16px; line-height: 24px;">-XExistentialQuantification</span></div><div><span style="background-color: white; line-height: 23.99147605895996px;"><span style="font-family: Times, Times New Roman, serif;">option. This enables existential type extensions.</span></span></div><div><br /></div><div>We can define a constructor for Magic which accepts anything that can be shown, then define equality by comparing string values.</div></div></div><div><br /></div><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6252615"></code> <div>The last one is false because show 5 is "5" whereas show "5" is "\"5\"". Nonetheless it's what we wanted.</div><div><br /></div><div>This really is my first day learning Haskell, so please comment and set me straight if I'm doing things wrong. Also, what is a more realistic use of forall?</div>
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/01917800488530923694">
  <div class="css-comment-name js-comment-name">
    Gabriel Gonzalez
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-17T00:03:22.263Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Note that wrapping the type in the `Magic` constructor is (almost) exactly identical to wrapping the type in `show`, at least for the use cases you were interested in:<br /><br />&gt;&gt;&gt; Magic 5 == Magic &quot;the gathering&quot;<br />False<br />&gt;&gt;&gt; show 5 == show &quot;the gathering&quot;<br />False<br /><br />However, there are other cases where the existential quantification is useful and cannot be trivially be replaced by a function.<br />
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/06543407828539972377">
  <div class="css-comment-name js-comment-name">
    Orkan
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-17T00:28:00.720Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    &quot; Also, what is a more realistic use of forall?&quot;<br /><br />For example, I was simulating a card game. I wanted different players to have different strategies, and for that they need to have states that may have different types. So you do something like:<br /><br />data Player a = forall state. Player Cards state (Strategy state)<br />data Strategy s = (s -&gt; MyCards -&gt; CardsPlayedSinceMyLastMove -&gt; (MyMove, s) )<br /><br />Where Cards, MyCards, CardsPlayedSinceMyLastMove and MyMove are appropiate types. So Player holds the cards, an arbitrary state, and a strategy that uses the state and other information to make a move and modify the state.<br /><br />This can&#39;t be done without existential quantification, AFAIK.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/14267910391550235126">
  <div class="css-comment-name js-comment-name">
    Singpolyma
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-17T01:03:16.882Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    You can also get fully-Dynamic behaviour by using the Data.Dynamic module
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/14267910391550235126">
  <div class="css-comment-name js-comment-name">
    Singpolyma
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-17T01:04:46.659Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    You could just store the strategy partially applied to the state already...
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
    2013-08-17T02:50:15.520Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Yeah as I was writing about Magic I got the feeling that it wasn&#39;t an improvement. Can you tell me more about the best uses of existential quantification?
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/09187236945238786712">
  <div class="css-comment-name js-comment-name">
    Lennart
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-17T06:35:51.911Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    You should try writing a (==) that takes arguments of of different types, returns False if they are not the same type, and uses regular comparison otherwise.<br /><br />Not that I would ever recommend using such a function, but it&#39;s a lot better than comparing strings.  :)
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/01917800488530923694">
  <div class="css-comment-name js-comment-name">
    Gabriel Gonzalez
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-17T15:19:41.471Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Existential quantification works well when the behavior you want to encapsulate cannot be easily packaged into a single function or record of partially-applied functions.  I can&#39;t think of an example off the top of my head, but there is a well-known post that described the typical scenario where you want to avoid the existential type classes:<br /><br />http://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/<br /><br />Basically, any time you can get away with a record of partially applied functions it is usually beneficial to do so, but sometimes you can&#39;t and that&#39;s when you need ExistentialQuantification.<br /><br />Also, keep in mind that you can also use existential quantification to abstract away type variables that are not constrained by a type class.  For example, check out this Stack Overflow answer from today that uses it to abstract away the internal accumulator of a fold:<br /><br />http://stackoverflow.com/a/18289075/1026598<br /><br />
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
    2013-08-17T17:23:18.610Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    That&#39;s what I wanted to do initially and couldn&#39;t figure out how. The definition of Eq seems to prevent it. Can you give me a hint? Do I use Data.Dynamic like Singpolyma suggests?
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/17949151208834305980">
  <div class="css-comment-name js-comment-name">
    AyeGill
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-17T21:47:40.084Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Another advantage of using `show` rather than `Magic` is that Magic is only good for comparison(unless you manually extend it to do other things), whereas show lets you do anything you can do with strings.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/16208489588423816254">
  <div class="css-comment-name js-comment-name">
    Targen
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-17T23:10:45.959Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    This comment has been removed by the author.
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
    2013-08-17T23:18:34.352Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Although in time you’ll find that you simply won’t need dynamic types at all, Data.Dynamic is likely to be the best approach for doing this sort of thing, as many things are already implemented.<br /><br />If you want to take a slightly lower level approach to get a handle of how things work, check out the Typeable typeclass — you can write the comparison function Lennart suggested using `Data.Typeable.cast`.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/09187236945238786712">
  <div class="css-comment-name js-comment-name">
    Lennart
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-18T08:12:58.739Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Define a new operator (===) for heterogenous equality.  You can never use the Prelude (==) because of its type. 
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/02239068589033148700">
  <div class="css-comment-name js-comment-name">
    Cale Gibbard
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-18T08:16:35.960Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    To elaborate on what Singpolyma is suggesting, you could have:<br /><br />data Player = P Cards Strategy<br /><br />data Strategy = S (Cards -&gt; CardsPlayed -&gt; (Move, Strategy))<br /><br />A function which constructs a strategy would then take any extra state it might need as a parameter, and update that state by applying itself recursively to new values when producing the Strategy in its result.
  </div>
  <br/>
</div>
</div>
{% endraw %}
