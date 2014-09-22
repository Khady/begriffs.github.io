---
title: How to compile Haskell libraries for Heroku
---

I noticed that all forks of the Haskell buildpack for Heroku are
using outdated binaries. So I began what turned out to be a long
journey to create Heroku-compatible binaries for the the newest
versions of GHC and Cabal. It ultimately succeeded and you can use
my method to build whatever Haskell binaries you want and they will
be ready to run in a Heroku instance.

**Step 1.** Clone, patch, and deploy a Vulcan build server.

```bash
git clone https://github.com/heroku/vulcan.git
cd vulcan
curl https://gist.github.com/begriffs/6980915/raw/2b1cb9ba4d4dc8b6c896eae14bbc7804d2e11f29/vulcan-haskell.patch | git apply
bundle
vulcan create [YOUR-SERVER-NAME]
```

**Step 2.** Download and extract the Haskell source code of the
package you want to build. (It should include a `Setup.hs` file.)
Then submit it to your build server.

```bash
# in your Haskell package's source directory
 
vulcan build --verbose -c "export PATH=\$PATH:/app/vendor/ghc-7.6.3/bin && export LD_LIBRARY_PATH=/app/vendor/ghc-libs:/usr/lib && ghc -L/app/vendor/ghc-libs --make Setup && ./Setup configure --prefix=/tmp/built && ./Setup build && ./Setup install" -p /tmp/built
```

**Step 3.** Download the binaries. The build process will output a
link for you to follow to get a tarball of the results.
