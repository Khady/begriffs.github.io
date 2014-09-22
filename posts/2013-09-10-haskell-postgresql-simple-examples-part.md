---
title: Haskell postgresql-simple examples, part 1
---

I've been working on a simple api server, and getting confused by
the sheer number of new Haskell things I need to learn. There are
many combinations of http servers, frameworks, database libraries,
and json parsing to try. So I'm taking a different route. I'm
learning each piece in isolation so later I can put them together
easily. Starting with database access.

Let's take the magic out of talking to Postgres. How do you connect
to an existing database and read/write data? The most direct way
appears to be the `postgresql-simple` package. There's an even lower
level library called `postgresql-libpq` that wraps the C-based
libpq, but I don't relish the idea of managing a database connection
and other low-level details.

Can't get simpler than this: add two and two in sql. No table access needed.

```haskell
module Main where

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

main = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "haskell"
  }

  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )
```

Some notes about this code. Notice how you can extend `defaultConnectInfo`.
It's more future-proof to do this than trying to specify an entire
`ConnectInfo` yourself. This is Haskell, after all, you can't just
omit a field and the library might expect more fields in future
versions. Extending the defaults will be fine because the new default
will cover the new type.

Also, other docs for this library show the use of `query`, but you
need `query_` for queries which take no arguments. Speaking of docs
in general, be careful what Google tells you. I was looking at the
Hackage docs for version 0.1.5 of this library for a long time
because that is what Google had indexed. The newest version as of
this writing is 0.3.6! (Hackage should really display a glaring
warning when you look at old versions of a package.)

<div class="alert alert-info" role="alert">
  <h4>Typing details and conventions -- safe to ignore.</h4>

  The naming style of `query_` flouts Haskell conventions where a
  trailing underscore means that the function suppresses a return
  type. For instance, we're using `mapM_` in the code. Look how it
  differs from its scoreless brother `MapM` is just map that burrows
  along through a monad. The `print` function returns `IO ()` and
  `mapM_` is compatible with that. The `query_` function returns `IO`
  of a certain type of array, which is not compatible to be chained
  with `print`s.
</div>

On to the next query.

```haskll
putStrLn "3 + 5"
mapM_ print =<< ( query conn "select ? + ?" (3 :: Int, 5 :: Int) :: IO [Only Int] )
```

What is this nasty `IO [Only Int]` annotation? Might make you yearn
for a looser language. It's an instance of a general Haskell
principle. The less you _do_ with a value, the more you have to
describe its construction. All we're doing with our query results
is looping through its rows (one row it turns out), and printing
it. All print wants is an instance of Show. That's all the compiler
can infer. The SQL string is kind of opaque. It's a foreign language
to the compiler, so the compiler cannot see we are adding numbers.

In fact here's another digression. SQL is like a foreign function
interface isn't it? One based on string interpolation. If we instead
built SQL expressions as Haskell datatypes then the compiler could
know what we want. Hmm, I wonder what part two of this article will
be about...

So our annotation of `IO` and `Int` is clear. What about this `Only`
thing? It's some terminology created by `postgresql-simple` to help
protect you from injection. The `query` function takes a final
argument of type `FromRow`, which is built out of `FromField`s.
Specifically arrays, vectors, or tuples of `FromField` -- or `Only
FromField` for a single one.

<div class="alert alert-info" role="alert">
  <h4>Digression</h4>

  In Haskell I don't think you can properly make recursive definitions
  with tuples. This library in particular allows you to make `FromRow`
  on `FromField` tuples of length two through ten. They actually
  specify the constructors for each of these eight sizes. Kind of
  crazy, isn't it? From what I can gather it's a flaw in Haskell's
  design that violates the [Zero, One, Infinity
  Rul](https://en.wikipedia.org/wiki/Zero_one_infinity_rule)e and
  is remedied in Agda using dependent types. To construct bigger
  `FromRow` tuples you can combine them with the library-defined
  `(:.)` operator.
</div>

So much for queries with and without arguments. How about commands?
(This example assumes an existing table called `words` with a column
named `word`.)

```haskell
putStrLn "Enter a word"
word <- getLine
execute conn "insert into words (word) values (?)" $ Only word
```

Not much type annotation needed here, because the way we gather
input tells the compiler what type it must be. Notice we use the
`execute` function now. I first tried `query`, but it gives a runtime
error when you try using destructive operations from `query`. Another
level of safety I guess.

There you go, some pretty "normal" code for doing SQL. In part two
we'll see a more streamlined way to interact with the database, and
one that helps the compiler infer more and save you typing. But
it's nice to see the non-magical way first, isn't it?
