---
title: Honest Code
---

This post is about sucking it up and just writing low-level code.
Over the past few months I have visited and revisited ideas for
interpreting [combinatory
logic](http://en.wikipedia.org/wiki/Combinatory_logic) which is a
functional language with just three single-letter building blocks.
The vision was to create a database which
[memoizes](http://en.wikipedia.org/wiki/Memoize) all combinatory
logic reductions for good. An ever-growing central repository for
all computable knowledge. Any purely functional program could be
compiled into combinatory logic and checked against the database
to see if it had alredy been run -- or if parts of it had.

My [first attempt](https://github.com/begriffs/clache/tree/feature/php)
was a web app in PHP with a Postgres back end. The term to be reduced
was passed as a url (or post variable for larger terms), and the
server replied with either a [normal form](http://bit.ly/syy6dB)
or a timeout.

The algorithm used some intensive [back and
forth](http://code.google.com/p/clache/wiki/ReductionStrategy) with
the database, and started off running very slowly. As it processed
more terms, it sped up because subterms were cached in the database.
However, as the database grew (to several gigabytes), the algorithm
hit a performance wall.

My [next attempt](https://github.com/begriffs/clache) was to write
an app for Google's App Engine. After all, reducing CL terms can
be done in parallel just fine, there is no mutable data involved.
I used a simpler algorithm with the database. Rather than storing
intermediate steps, I stored just successful reductions to normal
form. I then generated all CL terms of length 17, and tried reducing
them all, ten at a time concurrently. The app engine worked flawlessly,
no slowdown. However, when I tried reducing very large CL terms
compiled from Scheme, the app engine's computation time limit
prevented the reduction from ever finishing. No amount of preliminary
steps could soften the problem and get past the time limit. This
app would be very promising if I could somehow lift the limit.

Today I decided to trash the frills and see how well a bare metal
C implementation would stack up. Lots of pointer arithmetic and a
few memcpy's later, and the
[beast](https://github.com/begriffs/clache/tree/feature/c) was
ready. The result? At under 100 loc and avoiding all clever database
tricks, it was super fast, much faster than the tricky ones. Kind
of embarassing to tell the truth. Now perhaps the app engine version
would scale to leverage existing computations in its cache, but
maybe not.

Sometimes the best code is just dumb simple. Long live C.
