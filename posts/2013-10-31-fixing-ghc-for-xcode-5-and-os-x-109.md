---
title: Fixing GHC for xcode 5 and OS X 10.9 Mavericks
---

I tried to install the Haskell snap web server today and had problems.
The `skein` dependency failed to build. Luckily Mark Lentczner
(mzero) has already gotten to the bottom of this issue and released
a script that we Mac people can use to patch GHC 7.x to get around
the problem.

Just download and run [this
patch](https://gist.github.com/mzero/7245290#file-ghc-clang-wrapper). It
worked well for me and I'm back in business. Thanks Mark!
