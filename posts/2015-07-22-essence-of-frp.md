---
title: The Essence of FRP
video: true
twitpic: https://i.vimeocdn.com/video/527644244.jpg?mw=700
desc: The original formulation of functional reactive programming
---

[Conal Elliott](http://conal.net/blog/) proposed functional reactive
programming twenty years ago with a clear denotational semantics.
Over time the idea gained popularity but the original conception
became blurred. In this video Conal explains FRP's original formulation
and its benefits.

<video poster="https://i.vimeocdn.com/video/527644244.jpg?mw=700" class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/134223272.hd.mp4?s=970e3b0ce3453570227910fa68302737" type="video/mp4">
</video>

### Summary

- FRP is is receiving more interest now but has become misunderstood
    - The notion of FRP was very precisely defined 20 years ago
    - It allows us to reason precisely and simply. It is misapplied by Elm, Bacon, and Reactive Extensions
- The true essence is shaped by two fundamental ideas
    - Continuous time (its non-strictness enables modularity, see Why Functional Programming Matters)
    - Simple denotation allows dependable reasoning
- Reasons for continuous rather than discrete
- Approximations compose badly, so postpone until the end
- We don’t need to have an opinion about what FRP means! Use math.
    - There is one datatype: `Behavior a`
    - Then a meaning, or `m`, of a behavior maps a behavior to a function on time. `m :: Behavior a -> (Reals -> a)`
    - The original formulation of FRP from 1994 lacked the modern vocabulary.
- The modernized formulation
    - Behaviors are Functors, Applicatives, and Monoids
    - If `a` is a Monoid then so is `Event a`
    - Comparing the original semantics of Behavior with  typeclass instances
    - Event is just Behavior composed with the list constructor
