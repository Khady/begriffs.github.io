---
title: Some extra safety with Yesod routing
---

Today I played with adding some routes to my sample chat API server.
I have nothing original to report, it's all just stuff in the [Yesod
Book](http://www.yesodweb.com/book/routing-and-handlers). But one
interesting thing I noticed about routing is that you add constraints
by using types -- either the standard ones or your own. This allows
you to restrict the valid URLs for your app without much boilerplate
code.

For instance, imagine a user view in your app, which needs a numerical
user id passed in the route. Add a line in `config/routes` like
`/user/#Integer UserR GET`

If I then hit the app at `/user/hi` it gives me a 404 immediately.
It doesn't have to run any of my code which would probably try to
run a database query. It just knows that the route is hopeless
because it can't turn "hi" into an Integer.

Using your own types and [smart
constructors](http://www.haskell.org/haskellwiki/Smart_constructors) you
can easily make validations which will be used both in your controllers
(called _handlers_ in Yesod land) and in the generated routes. Talk
about [DRY](https://en.wikipedia.org/wiki/Don), this single source
of truth coordinates several pieces of your app. You know what's
also dry about Yesod? Before sending requests off to your handlers
it goes through a round of URL parsing to make the paths canonical.
If someone hits `/user//1/` the framework will actually send a 301
redirect to `/user/1`, ensuring your search engine juice stays
juicy.

What other types might be useful for routing validation? Maybe
something like ModernYear that ensures a year after 1900 for people's
birthdays.

Anyway, while thinking about it, I came across a crazy bit of Haskell
research called _type-level natural numbers_. The implementations
so far look super awkward but the idea is cool: to allow programmers
to restrict the numbers used in constructing a type and to allow
the compiler to enforce the restriction. So using the wrong values
or even creating a situation which might use the wrong values could
be stopped before you run the program. Yeah, that sucked up lots
of my time tonight and didn't lead to any blogworthy epiphanies.
