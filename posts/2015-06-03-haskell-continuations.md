---
title: Continuation Passing Style in Haskell
video: true
videoUrl: http://player.vimeo.com/external/129301223.hd.mp4?s=7d047e5e32eb41d71cfad82086ee188f
---

[Ryan Orendorff](https://github.com/rdodesigns) presents a tutorial
about using continuation passing style in Haskell. He starts with
an introduction that looks a lot like how other functional languages
do continuations, then refactors the example to use monads. The
continuation monad has been called the "mother of all monads" because
of its flexibility. See it in action in the video below.

<video poster="https://i.vimeocdn.com/video/520812183.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="http://player.vimeo.com/external/129301223.hd.mp4?s=7d047e5e32eb41d71cfad82086ee188f" type="video/mp4">
</video>

### Summary

* The basics of Continuation Passing Style (CPS)
* CPS can be made into a monad, and an operator, “call with current
  continuation”
* Motivating the style using the calculation of the area of a circle
* Creating a suspended computation with `cpsify`
* Manual chaining of suspended computations is a pain
* The pattern can be seen as a monad, with `cpsify` as `return` and
  `chain` as `>>=`
* The `do` notation provides nice sugar for CPS
* We can create suspended computations, chain them together, and
  use a type `Cont` to help prevent mistakes
* callCC
    - Allows exotic control structures, anything you can imagine:
      Exceptions, Coroutines, Generators, Iterators
    - An example of an “eject button” that short-circuits computation
* Despite their promise, continuations are generally confusing and
  haven’t provided practical control structures
