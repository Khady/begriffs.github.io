---
title: Getting dirty - cabal dependencies, string types, JSON
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
Tonight we leave the calm of the stratosphere and descend into the babbling of human language and strife of cabal dependencies. In short, I tried to do a regular programming thing.<br /><br />JSON. It's passing messages the world over, and I'd like my program to speak it too. Some Googling reveals that <a href="https://github.com/bos/aeson">bos/aseson</a> is the thing to use. I try to install it with cabal and apparently all the other things I had previously installed have put me in "cabal hell" and I have contradictory dependencies. No problem, blew all my installation away with <span style="font-family: Courier New, Courier, monospace;">rm -fr ~/.ghc ~/.cabal</span> and installed&nbsp;<a href="https://github.com/Paczesiowa/hsenv">hsenv</a>. For you Ruby people, gem = cabal and rvm = hsenv. Even though doing so takes more space on your disk you should use hsenv.<br /><br />So let's take an example from the Aeson docs and parse a JSON array.<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6290716"></code> Wait, what's wrong with my string? Turns out that we often mean several things by "string" and these things are disambiguated in Haskell. One meaning is an array of Chars, which are big fat UTF-32 characters all in memory at once. Sadly this is wrong for JSON in two ways. First we often want to decode JSON from a stream, aka lazily. Second JSON is UTF-8. Enter&nbsp;<span style="font-family: Courier New, Courier, monospace;">Data.ByteString.Lazy.Char8</span>, a representation of a lazy sequence of bytes which is an instance of the <span style="font-family: Courier New, Courier, monospace;">IsString</span> typeclass. Perfect for slurping UTF-8 in from a socket, and exactly what Aeson uses.<br /><br />We could explicitly convert our string literals to this type, but there's a GHC extension to make this nicer. Enabling it will infer the type of string needed from context and use the function <span style="font-family: Courier New, Courier, monospace;">fromString</span> (derived from <span style="font-family: Courier New, Courier, monospace;">IsString</span>) to convert a string literal to the right type as needed. Once we import the needed type and enable the GHC extension our example works like we would expect.<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6291103"></code> Tomorrow I'll continue toward making a real API web service in Haskell. It will power an in-app messaging ajax widget for other web sites. I met some people last week here in SF who want to add this type of feature to their site so I thought it would be a good way to put my Haskell to the test and power a JS widget. Who knows, maybe I can learn about compiling Haskell to JS to make the widget. All kinds of fun ahead!
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/10725680599856600394">
  <div class="css-comment-name js-comment-name">
    Brian McKenna
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-21T21:12:16.965Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Instead of exiting ghci, you can dynamically enable flags:<br /><br />:set -XOverloadedStrings<br /><br />Easy! :)
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
    2013-08-22T14:38:29.854Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    I&#39;ve been trying out a little fay:<br /><br />module Main where<br /><br />import Prelude<br />import FFI<br /><br />mymax :: Double -&gt; Double -&gt; Double<br />mymax = ffi &quot;Math.round(%1, %2)&quot;<br /><br />main = putStrLn . show $ mymax 10 23<br /><br />now compile that:<br /><br />fay foo.hs<br /><br />and run the javascript:<br /><br />node foo.js
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
    2013-09-11T05:27:01.632Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    You’ve probably found this by now, but the latest version of `cabal-install` (the package whence comes the `cabal` executable), released about a week ago now along with the latest version of the Cabal library, now support sandboxing in a way that effectively replaces tools like `hsenv` and `cabal-dev`.  See some release notes at<br />http://coldwa.st/e/blog/2013-08-21-Cabal-1-18.html<br /><br />The type called `ByteString` in the `Data.ByteString.Char8` module is actually the same as the one in `Data.ByteString`, the difference being that the former module contains functions that interpret the bytes in the ByteString as 8-bit characters.  Note that these ByteStrings are strict: they are either not evaluated at all, or fully evaluated and in memory all at the same time.<br /><br />&gt; One meaning is an array of Chars, which are big fat UTF-32 characters all in memory at once.<br /><br />This is actually not a proper description of the `String` type.  `String` is just an alias for `[Char]`, which is a lazy linked list whose elements are lazily evaluated numeric representations of characters.  The representation of `Char` (in GHC, at least, and I venture it’s the same in every extant implementation) is simply the code point number itself stored as a number, and indeed that’s the same as UTF‐32 in practical terms.  However, if you have a value of type `String`, lazy evaluation means any of the actual character values in the list may be unevaluated, so they certainly aren’t bound to be all in memory at once.<br /><br />The nicest example of this is, of course, an infinite `String`:<br /><br />&gt; repeat &#39;x&#39; ++ &quot;end&quot;<br /><br />That value is, of course, a bit too large to be in memory all at once ;)  Yet it’s a perfectly reasonable value of the `String` type and you can operate on it as long as you don’t try to get to the bottom of the list.<br /><br />This cannot be done with a strict ByteString, which *does* correspond to the notion of an array all in memory at once — although its elements aren’t arbitrary Unicode code points, but simple bytes.  However, it’s fine with the lazy ByteStrings from the module `Data.ByteString.Lazy`.  The two types have the same name, but lazy ByteStrings are represented internally as lazy lists of smaller strict ByteStrings, each called a “chunk”.<br /><br />Do note that the `repeat` function is unavailable for strict ByteStrings (as it’s impossible to define in any useful way), but lazy ByteStrings do have such a function.<br /><br />The exact data structure you described —an array of Unicode code points all in memory at once, each represented as a 32‐bit integer— is actually available in several ways.  There is a multitude of array‐like containers in the `Data.Array` and `Data.Vector` bits of the module namespace.  As far as I can tell, the general recommendation nowdays is to avoid `Data.Array` (except for things like Repa, but that’s a whole different story), and use `Vector`s instead, and almost always prefer more natively functional data structures such as regular lists and sequences (see `Data.Sequence`).  Check out this StackOverflow question for details:<br />http://stackoverflow.com/questions/9611904/haskell-lists-arrays-vectors-sequences
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
    2013-09-11T05:30:24.991Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Oh, and I forgot to mention — the types named `Text` in `Data.Text` and `Data.Text.Lazy` are the most common and practical solution for manipulation of strings of text in Haskell.  I’m sure you know this by now, of course :)  Just a few pointers for whoever wanders here looking to learn about these bits of the language.
  </div>
  <br/>
</div>
</div>
{% endraw %}
