---
title: Haskell "God Mode" Sandbox
---

Sandboxes have abolished cabal hell but in many cases have introduced
long redundant compilation (looking at you, `haskell-src-exts` and
`lens`). Recently I've given [Stackage](https://www.stackage.org/)
a try to share a common sandbox between projects while pinning
compatible package versions.

Stackage has worked great, but I thought of a way to get even crazier
with it: preemptively **build all of hackage** into a huge shared
sandbox pinned to a given Stackage LTS version.  This would take
forever, right? Yep, it did. Over a week on my Mac running around
the clock. But now that it's done virtually any haskell project
builds on my machine immediately without having to build its
dependencies.

If you're also using OS X 10.10, GHC 7.8 and Stackage LTS 2.5 then
you can get the speed too without having to turn your computer into
a space heater:

```bash
mkdir -p ~/.stackage
curl http://bin.begriffs.com/godmode/osx-10.10_ghc-7.8.4_stackage-lts-2.5.tar.xz | tar xJ -C ~/.stackage
# it's about 800MB
```

Next install
[stackage-cli](https://hackage.haskell.org/package/stackage-cli).
Then, inside any project you'd like to build run

```bash
stackage sandbox init lts-2.5
```

Building the project will use the sandbox and be fast. If `cabal
install` hangs on resolving resources, try the flag `--solver=topdown`
which uses more memory but appears to get past that problem.

### Creating a new megasandbox

<div class="alert alert-warning" role="alert">
  <h4>Security Warning</h4>

  Haskell package builds can run arbitrary code. You probably do
  not want to build all packages on hackage right on your personal
  computer. Use an isolated environment instead like CI or a virtual
  machine so that your data is not exposed to random code on the
  internet.
</div>



To build god mode for another architecture you need to get the list
of all packages on hackage and xargs it into cabal. Here's my hacky
little solution:

```bash
npm install -g json
mkdir -d /tmp/build && cd $_
cabal sandbox init
wget https://www.stackage.org/snapshot/lts-2.5/cabal.config
curl -H "Accept: application/json" https://hackage.haskell.org/packages/ | \
  json | grep packageName | cut -f 4 -d '"' | \
  xargs -L 1 cabal install -j --disable-documentation --max-backjumps=10

# Clean the executables themselves (they're big!)
rm ~/.stackage/ghc-7.8.4/lts-2.5/bin/*
```

Note that the first packages in the list begin with capital letters
and these packages are mostly old and bogus. You could remove them
from the list with `grep ^[a-z]` without doing too much harm because
the useful capital letter packages will end up being included by
other packages.

Some packages failed to build on my machine because certain specialized
C libraries were missing. That's OK by me since it built all the
general purpose packages. Also the `max-backjumps` was important
because otherwise cabal would freeze on certain packages as the
sandbox got big. The magic number is open to experimentation.  I
think it did cause some packages to fail when they shouldn't have.

### Automation?

CircleCI now supports [OS X builds](https://circleci.com/docs/ios)
in addition to Linux. We could use it to do the builds for  new
Stackage LTS versions as they are released. Have Circle run the
build, compress the folder, and deploy to S3.

Having full prebuilt Stackage sandboxes available across platforms
could help make Haskell development faster and more fun for everybody.
