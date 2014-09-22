---
title: Miscellaneous database stuff and an interesting book
---

I was wrong when I said before that the Groundhog ORM lacks
documentation. It's on Hackage, although the docs for the migration
DSL are in the `groundhog-th` package. So tonight I sent a pull
request to the Groundhog repo to add a readme with my simple usage
example and links to the other docs. Might be helpful for people
who stumble on the project through Github rather than Hackage.

Given the extensive docs for Groundhog, I really don't need to
detour to make tutorials. I should probably keep focusing on following
the Yesod book. The Yesod section on persistence actually mentions
Groundhog,

> "Earlier versions of Persistent made much heavier usage of Template
Haskell. Starting with 0.6, there is a new architecture inspired
by the groundhog package. This approach uses phantom types to carry
a lot of the burden."

Apparently much of the Groundhog goodness is now incorporated into
Yesod's persistence library. Tonight I started playing with the
models in `config/models` but I have nothing interesting to report.
It's just the stuff you will find in the Yesod book.

However, speaking of books, somebody recommended one to me in my
earlier post about using mathematical structures in code. It's
called [Elements of Programming](http://amzn.com/032163537X) and
now it's being shipped to my house. The authors discuss general
algorithms with actions and orbits (you might remember these from
group theory). ordered structures, rearrangements, partitioning,
"coordinate" structures...general ideas that use the taxonomy of
abstract algebra. The examples in the book are written in C++, but
think how smoothly they will work implemented in Haskell. It's going
to be beautiful.
