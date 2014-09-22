---
title: Haskell on Heroku, let's simplify
---

Since Haskell is my new favorite language I'd like to deploy it on
my favorite hosting platform -- Heroku. Luckily some kind folks
have created a Heroku buildpack to make this as easy as doing a git
push.

I happily began using
[puffnfresh/heroku-buildpack-haskell](https://github.com/puffnfresh/haskell-buildpack-demo)
to deploy my app, but noticed that it is referencing some slightly
outdated versions of GHC and Cabal. So I forked the code and got
my pull request ready and then noticed something odd...

**There are twenty forks of this project and they all differ!**
People are not sending pull requests, and there is no clear project
maintainer. Come on, guys, let's fix this.

Question 1: who wants to maintain this project? The original author
is [luciferous](https://github.com/luciferous), but his repo hasn't
seen a commit in a year, and has fewer stars than some of its forks.
Let's get ownership of the root repo transferred to whoever is
motivated to maintain it.

Question 2: what is lacking about this buildpack and are its forks
trying to solve the same or different problems? To answer this I
reviewed everyone's commits. Everything mostly agreed up through
December 2012. Puffnfresh and mwotton made tons of commits to improve
logging, cleanup, and caching. After that point it diverges. Here
is what everyone has been doing.

* mwotton
    * multi-threaded compilation
    * remove a "criterion" dependency
* RevCBH
    * clear cabal cache during compilation
    * install node.js and coffeescript for some reason
    * many variations on cabal options
* tcrayford
    * adjust cabal caching and cleaning
    * copy buildpack files from&nbsp;brianmckenna.org onto s3
* samstokes
    * include yesod, postgres, and other goodies
* eightyeight
    * include yesod
    * upgrade cabal and ghc
    * adjust caching
* dmjio
    * include snap
* ameingast
    * strip symbols from installed binaries to save space
    * gitignore swp files
* matt2224
    * make cabal install more verbose
* egonSchiele
    * copy buildpack files from brianmckenna.org into git
* BrianMMcClain
    * adjust buildpack to work on Cloud Foundry
    * http://catdevrandom.me/blog/2013/05/16/buildpacks-in-cloud-foundry-v2/
* EdgarGames
    * include postgres snaplet
* Tener
    * include cabal-dev
    * lots of other fixes, perhaps relating to cabal-dev
* benhirsch24
    * experimenting with parallel cabal install and happy
    * trying force-reinstalls
    * include language-c
* jogrms
    * upgrade cabal and ghc
    * went back and forth on caching and force-reinstall
* agocorona
    * include monadloc-pp
* nmk
    * copy buildpack files to grozdova.com
    * copy them again to dropbox and host them there

OK there are patterns here.

First people are wanting to include extra software like postgres
and web frameworks. My personal opinion is to not include this kind
of thing in the basic buildpack. You can actually combine more than
one buildpack in your deployment
using&nbsp;[ddollar/heroku-buildpack-multi](https://github.com/ddollar/heroku-buildpack-multi)
and I would advise keeping separate things separate.

Second is where to host the big GHC and Cabal binaries. Many people
are hosting them in all kinds of places, from personal domains to
s3 to dropbox. What is the best practice here? I wonder if Heroku
itself could host them?

Third is caching between deployments. This one has got to be tricky
because more than one person has gone back and forth, enabling and
disabling it. So for those of you who know, what is the final
verdict? Is it a good idea to do caching, and what are the right
settings? In this category I include the `force-reinstall` option
that people go back and forth on too.

Fourth is cleanup and minification. This category includes deleting
parts of cabal, like docs. It includes stripping symbols from
binaries. There are just all kinds of commits trying to shave space
off the final result. What makes this a big deal? Are there disk
limits on Heroku?

Finally some of these dependencies seem to have a multi-threaded
build mode, and it seems to cause problems sometimes. Should it be
enabled?

I'll email everyone involved and we can discuss and make the Haskell
deployment process everything we want it to be.
