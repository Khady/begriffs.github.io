---
title: Popularizing Haskell through easy web deployment
---

I want to see Haskell become "just another language," one that
people pick to solve ordinary tasks. One that becomes common on the
job. Wouldn't professional programming be nicer if we could walk
into more companies and expect to find nicely structured codebases
whose language helped prevent a jumbled undocumented mess?

Popularizing the Haskell platform requires making certain tasks
really easy. Someone once told me about the **Five Minute Rule**:
If you want people to keep using your product or library they need
to be able to go from the README to some small success in under
five minutes. The product may have lots of features -- that's fine
-- but it needs a solid quickstart.

Many people are interested in learning web development these days.
Code schools in San Francisco are overflowing with people learning
to crank out Rails and Node apps. This is in part because these
platforms have a smooth infrastructure to scaffold and deploy apps.
Students type a few magic incantations and, behold, their site is
live. It's heartening for a beginner.

Think if deploying a modest Yesod app was this quick. A seasoned
devops engineer might flippantly reply, "Deployment? Just install
nginx/apache, upload your app, run it as a daemon and proxy requests
to it." Sorry, that fails the five minute rule.

[Heroku](https://www.heroku.com/) is a popular hosting "platform
as a service" in the Ruby world. You deploy by setting a git remote
and pushing to it. After
[reviewing](2013-08-22-haskell-on-heroku-omg-lets-get-this.html)
existing solutions for using Haskell with Heroku, I decided to try
updating the process with the new features of Cabal and a modern
GHC version.

Here is the result:
[begriffs/heroku-buildpack-ghc](https://github.com/begriffs/heroku-buildpack-ghc)

It works pretty well. If you're interested in promoting Haskell for
everyday coding, consider contributing to the project. Check out
the github issues for ways you can help.
