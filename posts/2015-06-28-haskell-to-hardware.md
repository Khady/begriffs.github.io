---
title: From Haskell to Hardware
video: true
---

[Conal Elliott](http://conal.net/blog/) uses a reformulated
interpretation of lambda calculus to transform Haskell programs
into highly parallelized circuits. He gave this talk at [Bayhac
2015](http://bayhac.org/). His approach allows him to structure
parallel programming using a richer set of data structures than the
usual array-based thinking.
([slides](https://github.com/conal/talk-2015-haskell-to-hardware))

<video poster="https://i.vimeocdn.com/video/524462436.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/131952196.hd.mp4?s=7038f182348196c3cc0e87bf7d4170bf" type="video/mp4">
</video>

### Summary

- We turn Haskell into a tree structure of operations and thence to Verilog
- The same code generates different circuits depending on which
  types we specify for the polymorphism
- Guiding intuition: overloading lambda and application by using type classes
- The technical steps
    - Compile Haskell to the Core language
    - Monomorphize
    - Convert to abstract vocabulary
    - Interpret as circuits
    - Synthesize with existing HDL machinery
- It is within the abstract vocabulary that lambdas are overloaded
- The abstract form is not combinatory logic, it is cartesian closed
  categories
- The idea comes from J. Lambek who showed that there are interpretations
  of the lambda calculus other than the standard one of functions
- Examples of Haskell programs and the circuits they generate
- Going beyond arrays for parallel programming
    - uniform pairs
    - vectors of known length
    - depth-typed trees, bottom-up and top-down
- Implementing dot products and matrix multiplication
- Generalizing them by relaxing type constraints
    - Generates more parallelism, log depth trees rather than linear depth
- Implementing bitonic sort
    - Generates fantastically complicated circuits but ones which
      are certainly correct
- Implementing parallel scan
- Implementing polynomial evaluation in log time
- Combinational circuits have no state, but we can generate stateful ones too
    - Using a data type that models a Mealy machine
