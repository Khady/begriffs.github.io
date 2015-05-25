---
title: Filling Haskell's Type Holes Like It's Agda
video: true
---

[Kirstin Rhys](https://github.com/kirstin-rhys) gives a live coding
demo of interactively building Haskell functions by filling type
holes.  This allows her to write abstract things which would be
quite a challenge for the unaided mind. Interactive type holes are
used in provers like Agda and Idris and are now supported by GHC
7.8.

<div class="flowplayer" data-embed="false">
  <video type="video/mp4"
         src="http://player.vimeo.com/external/118981555.hd.mp4?s=6059f3a381a241aa0608a37fe6e9c095"
         poster="https://i.vimeocdn.com/video/506146633.jpg?mw=700"
  ></video>
</div>

### Summary

* First a demo in Agda to show how that system feels
    * Building the `filter` function as a warm-up
* Next doing it to Haskell with ghc-mod
    * Rewriting `filter` to compare
    * That was easy, so let's try functions that are not intuitively clear
    * like deriving the Functor instance for the free monad
    * ...and then the Applicative instance
    * Success! Look how hard that would have been without help
