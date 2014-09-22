---
title: Haskell postgresql-simple examples, part 2
---

[Part 1](2013-09-10-haskell-postgresql-simple-examples-part.html)
described how to do queries and read results. Refer to it to learn
how to connect to Postgres and all that.

I thought I was just getting into the good stuff with this library.
I thought you could create algebraic data types and then read them
in and out of table rows. The reality is frankly not that cool. The
way you associate a data type with a SQL statement is very positional.
You have to match your data structure with the SQL expression
argument by argument. Different parts of your code need to know and
implicitly share this order. And the compiler can't detect the order
being flipped, so you can get runtime errors.

My verdict: this is not Haskell at its best, but it is nice and
simple. Use the `postgresql-simple` library if you need to throw
together a direct ad-hoc query. It's great for that, but not good
for much more.

Here's how to read rows into a data structure and print out the
results.  Seems pretty smooth. Since the `Dictionary` derives `Show`
I didn't have to write any logic to format the output. The SQL
needed to know that you construct a `Dictionary` by specifying
`word` first and `definition` second, so that's kind of annoying.

Writing rows feels even more awkward. Notice that how I've written
this, I might as well have dispensed with writing a Dictionary
per-se, and could have just written the values I used to construct
the Dictionary.

But there you have it. If you'd like to do simple things with
queries, that's how this library does it.
