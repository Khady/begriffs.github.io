---
title: Don't be partial to partial functions
---

Day 2. To be honest, today I tried basic stuff like implementing
fizzbuzz in Haskell. I'll probably learn most by writing as many
actual programs as I can. I'm sure all this typesystem stuff will
fade into the background as I concentrate on writing useful things.

However, who really wants to read a blog post showing a fizzbuzz
implementation? I'm guessing you want something more thought-provoking
as you wait for my basic language skills to get a little stronger.

So here's one observation: I was reading about lists in <u>Learn
You a Haskell</u> and I came across this example

```haskell
h> head []
*** Exception: Prelude.head: empty list
```

This is an example of one of the *partial functions* defined in the
standard library. It's called partial because it cannot handle all
the values in its domain.

```haskell
h> :t head
head :: [a] -> a
h> :t []
[] :: [a]
```

GHCI reports that the empty list is indeed in the domain of `head`
(namely, lists). Unlike Ruby or Clojure, Haskell does not commit
Tony Hoare's "[billion dollar
mistake](http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare)"
of allowing null values so there's no option when taking the head
of an empty list but to throw an exception.

The Haskell docs suggest avoiding partial functions altogether. The
language provides alternative constructions. Let's look at one.
Rather than test for an empty list and conditionally take its `head`,
the docs suggest we use a `case` statement.

```haskell
-- replace this
if null xs
  then g
  else h (head xs) (tail xs)

-- with this
case xs of
  [] -> g
  y:ys -> h y ys
```

Only this doesn't entirely protect us at runtime! Consider this
snippet where I "forget" to list a pattern in the case statement.

```haskell
h> case [] of x:xs -> x
*** Exception: <interactive>:9:1-20: Non-exhaustive patterns in case
```

I would have liked the type system to notice my sloppiness ahead
of time. After jumping on #haskell I learned that GHCI does actually
notice something is awry, but I have to coax it to tell me.

```haskell
h> :set -Wall                -- warn about everything
h> :set -Werror              -- treat warnings as errors
h> case [] of [] -> "empty"  -- this could sort of work
 
<interactive>:15:1: Warning:
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: _ : _
 
<no location info>:
Failing due to -Werror.
```

The reason that a missing case is not considered an error is that
it is not always feasible to determine all possibilities. Sometimes
a function parameter can come from another module, and GHC does not
do whole-program analysis. Some people defensively write a default
case like this

```haskell
h> case [] of x:_ -> x ; _ -> "some info about the function"
"some info about the function"
```

In the future I'll be on the lookout for partial functions inside
conditionals, and will try to replace them with pattern matching.

Oh, and coming back down to earth... would anyone like to share
their most elegant implementation of fizzbuzz? :)
