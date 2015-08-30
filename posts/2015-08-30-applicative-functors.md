---
title: Applicatives in Math vs Code
video: true
twitpic: https://i.vimeocdn.com/video/532751406.jpg?mw=700
desc: How a mathematician motivates applicatives
---

[Matt DeLand](https://twitter.com/deland) contrasts the mathematical
and software ways of thinking about applicative functors. Coding
in Haskell and reasoning in category theory are two routes that
lead to the same concept, and Matt shows how someone in either
discipline would arrive at applicatives. He provides examples of
applicative functors and -- interestingly -- a counterexample.

<video poster="https://i.vimeocdn.com/video/532751406.jpg?mw=700" class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/137713584.hd.mp4?s=d6620b136f72d5d0c7ded459883b4096" type="video/mp4">
</video>

### Summary

- Haskell as category theory
    - The category Hask of types and functions between them
    - This is called the “idealized” Haskell category because it
      ignores possibilities like functions blowing up (exceptions etc)
    - Arrows in Hask agree only if they agree on all values
    - Some extra structure: for any type T there is an arrow from
      T to unit `()`, i.e. unit is a terminal object
    - Values of a type T can be identified as arrows from unit to
      `a`. Each arrow picks out an element as it were
    - The category is also monoidal
    - "Part of category theory is giving long names to ideas with no
      content. We’re going to follow in a long tradition!"
    - How functors work
    - Examples are Maybe, [], ( Int, _ )
    - One counter-example is `a -> (a -> Int)`
- How might a category theorist come to the notion of applicative functor?
    - Hask has some structure beyond the bare requirements of being
      a category
    - It has a final object (unit) monoidal pairing.
    - What functors behave nicely with respect to that relationship?
    - In category land they would be called “lax monoidal” functors
- The programmer’s take
    - What applicative functors provide
    - The standard typeclass definition is a bit overspecified
      (axiomatically the two standard properties imply the third)
- Proof that category theory version implies programmer version
- Proof that programmer’s version implies category theory version
