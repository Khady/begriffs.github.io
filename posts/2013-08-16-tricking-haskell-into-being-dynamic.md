---
title: Tricking Haskell into being dynamic
---

Day 1. What do outsiders think of this language? Speaking for myself,
I've always heard it has a strict type system that people learn to
love. Today I want to use a simple example to bend the rules and
get to the bottom of it.

I began [Learn You a Haskell](http://learnyouahaskell.com/), and
started typing in mundane stuff.

```haskell
ghci> -- yawn
ghci> 5 == 5
true
ghci> 5 == "a string"
 
<interactive>:3:1:
    No instance for (Num [Char]) arising from the literal `5'
    Possible fix: add an instance declaration for (Num [Char])
    In the first argument of `(==)', namely `5'
    In the expression: 5 == "a string"
    In an equation for `it': it = 5 == "a string"
```

The language of course gently steps in and protects me. Comparing
`Num`s and a list of `Char`s is nonsense. Yet...what if we wanted
Haskell to evaluate this comparison and return false? I'm fully
aware this is a bad idea, but it might teach us about how Haskell's
type system really works.

So let's unwind how the type error above originates. What does the operator `==` expect?

```haskell
ghci> :t (==)
(==) :: Eq a => a -> a -> Bool
```

This is really the crux of the problem, it grabs onto a type (which
incidentally must be an instance of the `Eq` type class) and expects
it for both arguments. I guess we have to give up, because a number
and a string simply cannot be simultaneously substituted for `a`
in the type signature above unless we can somehow make a new type
which tricks Haskell. That would be magical, wouldn't it? Something
like

```haskell
ghci> -- our goal
ghci> Magic 5 == Magic "a string"
false
```

Luckily most types are instances of `Show`, and provide a `show`
function to represent themselves as strings. Turns out this is what
the repl does when it prints values back. So we could compare `Show`
instances by comparing their string representation.

```haskell
ghci> show 5 == show "a string"
false
```

Maybe this is good enough. But maybe we can put it inside a magical
type and partially obscure the implementation. We could make different
data constructors and write all kinds of messy combinations for
implementing equality tests.

```haskell
data Magic = MagicString String | MagicInt Int

instance Eq Magic where
    (MagicString x) == (MagicString y) = x == y
    (MagicInt x) == (MagicInt y) = x == y
    (MagicInt x) == (MagicString y) = (show x) == y
    (MagicString x) == (MagicInt y) = x == (show y)
```

I'm sorry to inflict that code on you. Let's erase it from our minds
using *existential types*. In quest of the `Magic` recipe, I jumped
on #haskell and things got crazy. It's filled with friendly and
alarmingly smart people.

Here's what I learned. If we leave the `Haskell98` standard behind,
we can open up a trap door in GHCI by starting it with the
`-XExistentialQuantification` option. This enables existential type
extensions.

We can define a constructor for Magic which accepts anything that
can be shown, then define equality by comparing string values.

```haskell
hci> data Magic = forall a. (Show a) => Magic a
ghci> instance Eq Magic where (Magic x) == (Magic y) = show x == show y
ghci> Magic 5 == Magic 5
true
ghci> Magic 5 == Magic "a string"
false
ghci> Magic 5 == Magic "5"
false
```

The last one is false because show 5 is "5" whereas show "5" is
"\"5\"". Nonetheless it's what we wanted.

This really is my first day learning Haskell, so please comment and
set me straight if I'm doing things wrong. Also, what is a more
realistic use of forall?
