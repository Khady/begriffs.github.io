---
title: Thinking with Laziness
video: true
videoUrl: https://player.vimeo.com/external/130846004.hd.mp4?s=86274ac953fba73bbb7ecad0359a298d
---

[Tikhon Jelvis](http://jelv.is/) gave this talk at [Bayhac
2015](http://bayhac.org/).  He describes how lazy evaluation supports
deeper kinds of program modularity and suggests we embrace it for
what it really is. [[slides](http://jelv.is/talks/thinking-with-laziness/#slide1)]

<video poster="https://i.vimeocdn.com/video/522904112.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/130846004.hd.mp4?s=86274ac953fba73bbb7ecad0359a298d" type="video/mp4">
</video>

### Summary

* Stop thinking of Haskell like a strict language which happens to be lazy
* Laziness ia a new sort of modularity that we’re not used to from other languages
* It separates the definition of something from how it gets evaluated
* It lets us think of control flow the way we would think of data structures
* Deal with arbitrary precision (like vector graphics vs raster graphics)
* Shortcuts for free — `take 5 $ sort xs` actually stops the sorting after five elements are found. No break statement needed inside the sort function.
* Another example: alpha beta pruning game trees. We can use a simple tree structure and just choose not to evaluate branches and that does pruning.
* In Haskell lists stand in for loops. Control flow can be manipulated as data.
* Lazy structures don’t necessarily need to fully exist in memory.
* Convenient nondeterministic programming
    * variables can take many combinations of values from which we can later choose
    * map coloring example
* Lazy data structures is like precision on demand
    * it’s like the advantage of vector graphics over raster
    * exact real arithmetic
    * infinite quadtrees
* Laziness allows memoization below the level of abstraction
    * we can rely on it without having to do it ourselves
    * similar to garbage collection in this way, improves modularity
* Some people mistakenly believe Haskell does memoization automatically everywhere
    * that’s actually not feasible but there are packages to help: [data-memocombinators](https://hackage.haskell.org/package/data-memocombinators) and [MemoTrie](https://hackage.haskell.org/package/MemoTrie)
* Dynamic programming
    * no need to initialize everything or worry about reading one subproblem too early
    * just declare your array as containing all subproblems and the fact that it’s lazy ensures everything is evaluated in the correct order and at most once
* We saw four perspectives that turned out to be interrelated: modularity, control, precision, and memorization
* Q&A
