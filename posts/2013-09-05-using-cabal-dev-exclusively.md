---
title: Using cabal-dev exclusively
---

<div class="alert alert-warning" role="alert">
  <h4>Update</h4>

  Don't do this! Cabal version 1.18 now has built-in
  [sandboxing](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html).
</div>

Ever since I erased and reinstalled my whole Haskell Platform and
began installing packages using nothing but cabal-dev I discovered
there is an extra step to compile programs in their own sandbox. I
must have earlier been inadvertently leaning on globally installed
libraries with using GHC to compile programs. Today when I cabal-dev
installed a package I got an error during compilation that said the
package couldn't be found.

The solution is to set your `GHC_PACKAGE_PATH` so it includes the
places cabal-dev installs libraries. First look where all your
libraries live now by doing `ghc-pkg list`. Remember these paths
and edit your shell's init scripts to set the environment variable.
I use bash, so I add this line `~/.bashrc`

```bash
export GHC_PACKAGE_PATH=./cabal-dev/packages-7.6.3.conf:$HOME/.ghc/x86_64-darwin-7.6.3/package.conf.d:/Library/Frameworks/GHC.framework/Versions/7.6.3-x86_64/usr/lib/ghc-7.6.3/package.conf.d
```

Your paths will likely differ, but you see the pattern. The paths
are separated by colons, and are searched in the order listed.

Be sure the first place you have it check is the packages directory
inside `./cabal-dev`. This is because I run `cabal-dev install`
from the directory that contains my code. Now when I run ghc in
this directory it will be able to find the cabal-dev sandboxed
packages.

Using search paths is a common pattern among compilers, but it
confused me for a little while tonight. Makes a good topic for an
off-night blog post too. :)
