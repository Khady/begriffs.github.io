---
title: The Design of Purescript Halogen
video: true
---

[Phil Freeman](http://functorial.com/), the author of Purescript,
explains the design of [Halogen](https://github.com/slamdata/purescript-halogen),
a declarative, type-safe UI library.

<video poster="https://i.vimeocdn.com/video/525705269.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/132859139.hd.mp4?s=5cecdf40c05e5562be23e38fdc97021a" type="video/mp4">
</video>

### Summary

- Purescript-halogen
    - a toolkit for building reactive web apps in purescript
    - built on signal functions and a virtual dom
- Signals are values which change over time
    - they are functors and applicatives
- The “stateful” combinator creates a new signal from an old one,
  adding a stateful input. It is called foldp in Elm or stepper
    - for example if a signal wraps up a monoid then you can define
      a combine function which produces a new signal of the latest
      mconcat’d values
- Halogen provides a DOM templating DSL which builds a type `HTML`
- A user interface can be considered a `Signal HTML`
    - when the signal changes, push the diffs to the DOM
- Halogen goes beyond such simple Signals to Signal functions, i.e.
  `type UI input = Signal input -> Signal (HTML input)`
- Signal functions do have a mathematical denotation
- The concrete representation of signal functions in Halogen
- Signal functions also have Functor and Applicative instances
  (slides have a diagram)
- But signal functions have some additional type class instances:
  Profunctor, Strong, Choice
- Signal functions also have a stateful combinator
- Explanation of the merge combinator
- Putting this all together, the signal functions and combinators
  combine to create graphs of data flow
- In Halogen you get a lot of composability for free, which works
  well when defining web components
- Components actually have a type interface: `type Component m i o
  = SF i (HTML (m o))`
- Mixins become easy inside the graph of pure functions
    - Undo/redo
    - Routing
    - Sitemap / breadcrumbs
- How to handle
    - Ajax
    - External input
    - Third-party code
    - Canvas, SVG, WebGL
