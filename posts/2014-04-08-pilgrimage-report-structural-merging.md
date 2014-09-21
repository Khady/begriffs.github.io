---
title: Tikhon Jelvis' ideas about Structural Merging
---

This afternoon I paired with Tikhon. He's a Haskeller, researcher,
and the organizer of the SF Types, Theorems, and Programming Languages
group. His [project](https://github.com/TikhonJelvis/Cow) is to
extend the unix commands `diff` and `merge` to understand and work
better on JavaScript code.

As a product of the Unix tradition the standard `diff` program
operates line by line between files. This affects not just diff
itself but programs like git that rely on it. Have you ever changed
a program in a way that does not affect its operation such as
changing indentation and then been forced to make a big git commit?
Have you ever changed the name of a variable and caused a big
fragmented commit? Tikhon believes that small changes of meaning
should appear as small diffs and the reason that they currently
don't is that we still think in terms of teletypes rather than
syntax.

Most importantly Tikhon realized that operating crudely on lines
can create merge conflicts when there needn't be any. For instance,
consider this original file:

```javascript
function foo (a, b) {
  return a + b;
}

function foo2 (a, b) {
  return a - b;
}
```

One person edits it by moving one function inside the scope of the
other.

```javascript
function foo (a, b) {
  return a + b;

  function foo2 (a, b) {
    return a - b;
  }
}
```

Another edits it by changing variable names.

```javascript
function foo (a, b) {
  return a + b;
}

function foo2 (c, d) {
  return c - d;
}
```

The merge fails! Resolution requires accepting one version and
manually adding the changes from the other.

```diff
function foo (a, b) {
  return a + b;

<<<<<<< move.js
  function foo2 (a, b) {
    return a - b;
  }
=======
function foo2 (c, d) {
  return c - d;
>>>>>>> rename.js
}
```

His solution: a *structural* merge. A traditional diff sees each
of these changes as many lines, whereas each of the files being
merged differs by only a single structural change, and those changes
can be harmlessly resolved. In terms of syntax, the first change
(moving foo2 inside foo) looks like this

![Moving foo](/images/tree-1.png)

The second (renaming the variables) looks like this

![Renaming variables](/images/tree-2.png)

These representations are created using the Zhang-Shasha tree
edit-distance algorithm. It indicates "tree diff" in terms of the
node operations *move*, *relabel*, *add*, and *delete*. The algorithm
finds the minimum number of applications of these rules to transform
one tree into another.

Interestingly if we create a tree diff of tree diffs themselves we
can use it to display more meaningful merge conflicts. The diff of
the two diff trees above looks like this

![Tree diff](/images/tree-diff.png)

A second pass with a simplifying algorithm shows there is exactly
one edit operation introduced by each change. A structural merge
program can interactively ask the user which edit operation to apply
(and can do them both if requested).

Tikhon's big hurdle is to make his tree diff fast. As he quipped,
"[it runs in] exponential time...I'm not a fan." The solution is
dynamic programming, and in a lazy language like Haskell with
immutable data structures it only takes a tiny change in a program
to automatically memoize functions and enable dynamic programming.
We spent the day investigating how to do it for his tree diff
function, but began by playing with it in the simpler problem of
string edit distance.

Let me show you the trick first. It uses laziness and co-recursion
to make the function and its lookup table always keep one step ahead
of each other in a magical circle. Observe how it is used to generate
Fibonacci numbers.

```haskell
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
```

Let's see a naive implementation of string edit distance and how
to transform it with The Trick. It's a Haskell implementation of
the&nbsp;Wagner–Fischer algorithm which recursively calculates the
edit distance of every initial segment of the two strings eventually
working up to the original strings. Using the edit operations insert,
delete, and substitute it can be expressed succinctly as

![Wagner fischer](/images/wagner-fischer.png)

Translated to Haskell it becomes

```haskell
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = dist m n
  where
  (m,n) = (length xs, length ys)
  x = array (1,m) (zip [1..] xs)
  y = array (1,n) (zip [1..] ys)

  dist :: Int -> Int -> Int
  dist 0 j = j
  dist i 0 = i
  dist i j = minimum [
      (dist (i-1) j    ) + 1,
      (dist i     (j-1)) + 1,
      if x ! i == y ! j then     dist (i-1) (j-1)
                        else 1 + dist (i-1) (j-1)
    ]
```

The trick to make it fast is to co-recursively fill in a lookup
table with the edit distances of initial segments, and to calculate
edit distances...by referencing the table. Mind = blown.

After implementing the function above that returns merely the minimum
edit distance, we augmented it to return an array of the actual
edit actions needed. Got into some performance problems of repeatedly
calculating the length of those arrays when checking for the minimum,
but found a way around that problem.

```haskell
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = table ! (m,n)
  where
  (m,n) = (length xs, length ys)
  x     = array (1,m) (zip [1..] xs)
  y     = array (1,n) (zip [1..] ys)

  table :: Array (Int,Int) Int
  table = array bnds [(ij, dist ij) | ij <- range bnds]
  bnds  = ((0,0),(m,n))

  dist (0,j) = j
  dist (i,0) = i
  dist (i,j) = minimum [
      table ! (i-1,j) + 1,
      table ! (i,j-1) + 1,
      if x ! i == y ! j then     table ! (i-1,j-1)
                        else 1 + table ! (i-1,j-1)
    ]
```

What remains is to translate this nice memoized string edit distance
to trees using tree edit operations rather than string operations.
Notice the lookup table we used above is a two-dimensional array
indexed by the length of segments. To translate the lookup table
strategy to trees we need a way to uniquely name partial-traversals,
which we could do by choosing the number of hops along the traversal
to be the "index." (We experimented with using a Haskell Map keyed
off the trees but that was really slow.) Ultimately we did not
complete the refactor to make the tree diff sub-exponential speed,
but we discovered how it will be done.
