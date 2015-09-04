---
title: FP Graph Algorithms
video: true
twitpic: https://i.vimeocdn.com/video/533642391.jpg?mw=700
desc: Step aside imperative coding, Haskell does graphs.
---

Manipulating graphs has traditionally been seen as a place where
imperative programming reigns supreme. [Tikhon Jelvis](http://jelv.is/)
begs to differ, and shows a trick where graph traversal can be
expressed by a form of pattern matching. After explaining the
approach he simulates how it works on a sample graph.

<video poster="https://i.vimeocdn.com/video/533642391.jpg?mw=700" class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/138339869.hd.mp4?s=e29a8ef4fa7370f4b2e13b753e4e9fd9" type="video/mp4">
</video>

### Summary

- Traditionally graph algorithms are seen as one of the things that
  are really hard in functional programming
- To some extent it is because the graph algorithms and representations
  you’ve learned have a very imperative bent
- One insight into making a graph algorithm functional is to find
  a way to use pattern matching on a graph structure
    - What does this even mean?
    - Start with what we already know: pattern matching is easy on lists or trees
    - Translates to any algebraic data type really
    - But there is no algebraic data type for graphs
    - Graphs are not inductive — there is more than one way to build
      them up or take them apart
- But we can pretend! We can treat graphs as if they were inductive
- So how do we break a graph apart in a pattern match?
    - We’ll use a a “view”
    - `match :: Node -> Graph -> Maybe View`
- Depth first search
    - We don’t update flags on nodes to say which we have visited
    - Our recursion just happens on a subgraph that does not re-visit nodes
    - Example of the operation on a specific graph
- You can find the implementation in the [Functional Graph Library](http://hackage.haskell.org/package/fgl)
- Slides for this talk are available [here](http://jelv.is/talks/inductive-graphs-at-wagon/#/sec-title-slide)
- For an example of generating mazes using functional programming,
  check out this [blog post](http://jelv.is/blog/Generating-Mazes-with-Inductive-Graphs/)
- Q&A
