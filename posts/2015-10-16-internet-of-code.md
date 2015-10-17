---
title: The Internet of Code
video: true
twitpic: https://i.vimeocdn.com/video/539948884.jpg?mw=700
desc: Distributing typed program fragments over the network
---

[Gabriel Gonzalez](http://www.haskellforall.com/) explains how to
compile a subset of Haskell into typed lambda calculus and share
the pieces safely online. He believes that rather than distributing
packages or modules we can distibute typed program fragments. This
allows programs to be built safely and flexibly. Watch him give an
actual demo of building programs from this code ocean.

<video poster="https://i.vimeocdn.com/video/539948884.jpg?mw=700" class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/142572841.hd.mp4?s=97d89c76d0e85b440a909f4c1044e701" type="video/mp4">
</video>

### Summary

- Key idea: we can make it much easier to share code fragments and lower the barrier to entry in coding
- Background of the project
    - it did not begin with the goal of distributing code over the internet
    - it began as an optimization project
    - Gabriel’s [pipes] library was in a performance war with other streaming libraries
    - One common theme of fast Haskell research: the GHC compiler is ridiculously good at optimizing non-recursive code
    - But using recursion hits the “20ns” barrier
    - You can write algorithms non-recursively for a very very large class of problems
- Examples of non-recursive Haskell
    - Lists and map
    - Typically done with both a recursive data type and recursive function
- How to take advantage of the non-recursive property?
    - you can optimize things you couldn’t otherwise
    - ghc cannot try certain optimizations in recursive code because it could hang
    - Gabriel wanted a compiler that could always beta- and eta-reduce lambda calculus expressions for optimization
- He created a simple compiler called Morte
    - it is very limited: it has Pi types, and function application. That is IT.
    - No strings, datatypes, no recursion
    - But you can transform normal Haskell programs into this lower level lambda calculus
- Annah is an intermediate language which compiles to Morte
- A digression on universally quantified types
- Now for the INTERESTING PART!
    - At this point we have that nothing is recursive, everything is explicitly typed and self contained (aka closed, aka no free variables)
    - These expressions are easy to distribute, in fact the smallest granularity possible
    - You can reference expression by network endpoints
    - Demo of bools and if statements
    - Assembling code from the network
    - IO in Annah
- Morte is the perfect sandbox for distributed code
    - The lambda calculus leaves no possibility for unsafe actions
    - IO is encoded as a list of things to be done
    - If you trust the interpreter then you trust the program
- In the future Gabriel predicts we will be distributing typed program fragments rather than packages or modules
