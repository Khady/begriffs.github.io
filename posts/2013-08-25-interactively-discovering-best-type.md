---
title: Interactively discovering the best type classes for Haskell functions
---

Haskell's type classes exist to make make your functions more general
while still keeping you safe. In this post I will work with GHC to
arrive at the right signature for a function. The "weaker" the types
we choose the more situations a function can handle. It's instructive
to omit types entirely and see how GHC guides us toward the most
general thing that can work. I see a parallel between trying to
compile in a language like Haskell and running tests in a language
like Ruby.

```haskell
equal_steps :: a -> a -> [a]
equal_steps x y = [ (a - smaller)/distance | a <- [smaller..larger] ]
  where smaller  = min x y
        larger   = max x y
        distance = abs (y - x)
```

What does this function do, and what inputs are suitable? One way
to approach the question is to use duck typing in a dynamic language.
Just throw various values in and see if they quack. Write tests to
handle a few cases and cross your fingers.

How about asking the compiler which inputs work best? It can examine
the type classes it knows and suggest a medley. Let's try to load
this thing in GHC and see what it suggests.

```
equal_steps.hs:2:34:
    No instance for (Fractional a) arising from a use of `/'
    Possible fix:
      add (Fractional a) to the context of
        the type signature for equal_steps :: a -> a -> [a]
    In the expression: (a - smaller) / distance
    In the expression:
      [(a - smaller) / distance | a <- [smaller .. larger]]
    In an equation for `equal_steps':
        equal_steps x y
          = [(a - smaller) / distance | a <- [smaller .. larger]]
          where
              smaller = min x y
              larger = max x y
              distance = abs (y - x)

equal_steps.hs:2:51:
    No instance for (Enum a)
      arising from the arithmetic sequence `smaller .. larger'
    Possible fix:
      add (Enum a) to the context of
        the type signature for equal_steps :: a -> a -> [a]
    In the expression: [smaller .. larger]
    In a stmt of a list comprehension: a <- [smaller .. larger]
    In the expression:
      [(a - smaller) / distance | a <- [smaller .. larger]]

equal_steps.hs:4:20:
    No instance for (Ord a) arising from a use of `max'
    Possible fix:
      add (Ord a) to the context of
        the type signature for equal_steps :: a -> a -> [a]
    In the expression: max x y
    In an equation for `larger': larger = max x y
    In an equation for `equal_steps':
        equal_steps x y
          = [(a - smaller) / distance | a <- [smaller .. larger]]
          where
              smaller = min x y
              larger = max x y
              distance = abs (y - x)

equal_steps.hs:5:20:
    No instance for (Num a) arising from a use of `abs'
    Possible fix:
      add (Num a) to the context of
        the type signature for equal_steps :: a -> a -> [a]
    In the expression: abs (y - x)
    In an equation for `distance': distance = abs (y - x)
    In an equation for `equal_steps':
        equal_steps x y
          = [(a - smaller) / distance | a <- [smaller .. larger]]
          where
              smaller = min x y
              larger = max x y
              distance = abs (y - x)
```

It identifies the unknown functions "/", "..", "max" and "abs."
What inputs would be appropriate for its arguments? Certainly
something numeric. Yeah, we could choose `Float -> Float -> [Float]`,
but we don't want to be too specific and arbitrary. The language
will hold us at our word. If it helps at all to think about the
Haskell98 standard options you can examine the handy chart.

![Typeclasses](/images/typeclasses.gif)

An arrow pointing from one box to another means that the target box
can be used in all the situations that the source can.

Based on the operations we used in our function the compiler suggests
restricting the input to being an instance of Fractional, Enum, Ord
and Num. This is a powerful way to think about how our function
operates. It manipulates a value that can be sensibly divided,
enumerated, ordered and acted upon with arithmetic. Let's add the
restriction.

```haskell
equal_steps :: (Fractional a, Enum a, Ord a, Num a) => a -> a -> [a]
equal_steps x y = [ (a - smaller)/distance | a <- [smaller..larger] ]
  where smaller  = min x y
        larger   = max x y
        distance = abs (y - x)
```

But surely this is redundant! Some of these type classes include
the others (in fact we know this is true by consulting the chart
above). No need to speculate, just ask GHCi.

```haskell
ghci> :t equal_steps
equal_steps :: (Enum a, Fractional a, Ord a) => a -> a -> [a]
```

OK, this is simplified a little bit. But you may ask why must we
specify both Fractional and Ord? We learned to compare fractions
as children. How can this stupid language standard not even consult
grade-school math? Well, unlike some languages Haskell was not
slapped together in a week in 1995 during a frenzied browser cashlust
arms-race to turn a document presentation language into an interactive
app platform.

Nope, hearken back to 1927 when Emil Artin defined the concept of
an _ordered field_ in mathematics. This is a structure that a
Haskeller could think of as Fractional + Ord. Of course the
mathematical definition is all about the relationships of the various
operations and the Haskell type classes do not enforce those axioms.
But it's the intent of those classes, and Artin produced an example
of a field which could not be ordered to suit the axioms: the complex
numbers.

In an ordered field the order interacts with the other operations
in a way that people find pleasant and fertile with theorems. One
property enforced is that if \\(a\\), \\(b > 0\\) then \\(ab > 0\\)
too. However in complex numbers the element \\(i \\neq 0\\). The
order in an ordered field is _total_ which means that in this case
either \\(i > 0\\) or \\(i < 0\\). Can't be the former since \\(i^2
= -1 < 0\\). And can't be the latter since \\(\(-i\)^2 = -1 < 0\\).
And that's a problem.

So there you have it, that's why the Fractional class doesn't include
the `<` operator. OK, fine, but why must we specify both Fractional
and Enum? Can't we step through fractions one at a time from lower
to higher? Well, depends on the order. In the usual ordering we use
for fractions the answer is no. In math terms Enum would be called
a well-ordering, because it means there is a single unique successor
after each value. However no fraction has a "next" one, You can
keep splitting finer and finer between any two fractions. Incidentally
there are a few ways to well-order the rational numbers (fractions
of integers) if you're willing to invent a weird order that doesn't
play well with the other arithmetic operations. Georg Cantor
discovered one way

![diagonal mapping](/images/diagonal-mapping.jpg)

And that's why Fractional doesn't include a range-walking operator.
Yeah, it's like the Haskell98 committee actually put some educated
thought into what they were building.
