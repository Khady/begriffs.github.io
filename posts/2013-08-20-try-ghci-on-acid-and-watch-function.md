---
title: Try GHCi on Acid and watch function arguments melt away
---

<img src="/images/trippy.jpg" class="right" />
Day 5. I'm doing exercises from
[H-99](http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems),
and challenging myself to make answers that don't use named function
arguments. Although arguments can help describe a function's purpose
they are often just clutter. In fact uniformly renaming a variable
in a function has no effect on the function's output, a phenomenon
known as α-congruence in lambda calculus.

Because this is Haskell we're talking about, my idle preoccupation
(finding a point-free representation of functions) is of course
already someone's PhD thesis. I want to show you a cool tool that
will help you strip out extraneous variables and examine the
interesting results.

To see our code melt down our monitors and have the variables fall
out like loose teeth we need to use GHCi on Acid. If you're using
a mac then turn on, tune in, and drop out.

The installation procedure is slightly different than what I read
in the docs. Assuming your machine has [homebrew](http://brew.sh/)
and [cabal](http://www.haskell.org/cabal/) installed, run this:

```bash
brew install pcre
cabal install lambdabot
cabal isntall goa
```

That will take a while. Now create a `~/.ghci` file with the following

```haskell
:m - Prelude
:m + GOA
setLambdabotHome "/Users/j/Library/Haskell/ghc-7.6.3/lib/lambdabot-4.3/bin"
:def bs        lambdabot "botsnack"
:def pl        lambdabot "pl"
:def unpl      lambdabot "unpl"
:def redo      lambdabot "redo"
:def undo      lambdabot "undo"
:def index     lambdabot "index"
:def docs      lambdabot "docs"
:def instances lambdabot "instances"
:def hoogle    lambdabot "hoogle"
:def source    lambdabot "fptools"
:def where     lambdabot "where"
:def version   lambdabot "version"
:def src       lambdabot "src"
```

Your `setLambdabotHome` will differ from mine. You may only need
to change your home directory and the ghc version number to make
it work. Worst case just search for the `bin` folder of `lambdabot`.

Now, if everything was successful, your GHCi prompt should say
`Prelude, GOA>` and we can begin using the trippy *pl* command.
Let's start with a low dose and eliminate a variable that we all
know shouldn't be there

```haskell
Prelude GOA> :pl \x -> succ x
 succ
```

It detected that our anonymous function which takes a value only
to pass it to `succ` behaves no differently from `succ` itself. The
Greek alphabet lovers among you will be happy to know this is called
η-conversion. Now a harder example. Let's make a function similar
to `(!!)` but which uses 1-based indexing. I can think of one that
takes two arguments, but let's let :pl attack it.

```haskell
Prelude GOA> :pl \xs i -> xs !! (pred i)
 (. pred) . (!!)
```

What an interesting result. Do you think it's an improvement? I
kind of like it, built as it is purely from simple functions. However
its meaning doesn't jump out at me. Let's think it through.

Fist, what kind of a function is `(. pred)` and how is it suitable
to compose with `(!!)`?

```haskell
Prelude GOA> :t (. pred)
(. pred) :: Enum b => (b -> c) -> b -> c
Prelude GOA> :t (!!)
(!!) :: [a] -> Int -> a
Prelude GOA> :t (.pred) . (!!)
(.pred) . (!!) :: [c] -> Int -> c
```

Starting with the latter, it's a function which takes a list and
returns a new function -- which is expecting an Integer argument
and will give back a list item. Now `(. pred)` takes a function,
and given this one it will return a function with the same domain.
In this case a list. And what is the range? Functions from Int to
list items. Hey, that's the what we wanted originally with our
1-based element getter.

Honestly I can't say it's easy for me to think through this strange
composition, but many of the point-free results do look pretty and
symmetric. For instance, check out how to modify a function's second
or first argument

```haskell
-- modify the second argument
Prelude GOA> :pl \a b -> f(a (g b))
 (f .) . (. g)

-- modify the first argument
Prelude GOA> :pl \a b -> f((g a) b)
 (f .) . g
```

It looks natural this way. Now the `:pl` command goes beyond stringing
together composition and can summon strange functions I don't know
about yet. For instance, look how it rewrites a function that checks
if a list is a palindrome.

```haskell
Prelude GOA> :pl \xs -> xs == reverse xs
 ap (==) reverse
Prelude GOA> :pl \xs -> reverse xs == xs
 (==) =<< reverse
```

In either direction we get some crazy moon language! Do any of you,
my dear readers, have ways to make sense of the point-free idioms?
Perhaps I shouldn't use the exotic ones and should stick to old
fashioned named arguments.
