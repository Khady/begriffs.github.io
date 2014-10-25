---
title: Creating a package on Hackage
---

How do you go from a pile of Haskell code on your machine to a
finished package on [Hackage](http://hackage.haskell.org) in all
its tactfully understated glory? I recently took this journey and
some of the steps were tricky enough that I thought someone ought
to write a guide.

### Step 1 - the account
[Register](http://hackage.haskell.org/users/register-request)
a user account.  Like many steps in Hackage, this is a somewhat
human and manual process. A person has to review your submission
and deem you worthy. So I guess don't pick a profile name like
`javascriptFTW` that would anger them.

### Step 2 - package structure
Structure your package in a standard way. Source directories
are nested and named after the exposed module path. Your test suite
lives in its own place and needs some boilerplate configuration.
And of course you'll need a good cabal file. _Want the shortcut?_
Run [fujimura/hi](https://github.com/fujimura/hi) to generate your
project structure.  By default it will choose a BSD license and use
`hspec` for tests.  Speaking for myself I changed it to an MIT
license and [hspec2](http://hackage.haskell.org/package/hspec2).

```bash
$ hi --module-name "Foo.Bar.Baz" --author "J Doe" --email "jdoe@me.com"
```

### Step 3 - cabal
Customize the package cabal file. If you're wondering whether
it includes enough information, check with
```bash
$ cabal check
```
which will point out any problems. In fact Hackage will refuse a
package upload that fails the check. However there are fields you
might like to add beyond the bare minimum, such as the `Homepage`
field with a link to the project on Github. See this
[reference](http://www.haskell.org/ghc/docs/7.0.3/html/Cabal/authors.html#general-fields)
of all the fields.

### Step 4 - docs
Add Haddock documentation to your code as comments.  There is more
to Haddock than can reasonably fit in a little guide like this, but
a good way to go is copy what people do in a popular package you
admire. Generally the structure of generated docs is determined in
your module declaration. Be sure to include some example code since
it gives a quick overview to would-be users.

```haskell
module Foo.Bar.Baz
  (
    -- * Example usage
    -- $use

    -- * A section
    Thingie(..)

    -- * More stuff
  , fun
  , joy
  ) where
```

Notice you can create chunks by name like `$use` and interpolate
them at the right place in the module declaration. To preview your
docs locally run

```bash
$ cabal haddock
```

which will build the docs in `dist/doc/html/your-pkg/index.html`.
Note that unlike the custom in other languages, Haskellers usually
don't put much info into READMEs. At first I thought they were being
negligent but I learned that the combination of strong types and
Haddock's structure provide a standardized documentation experience
that works better. I like to add a link in my Github readme to point
at the Hackage docs to help those unfamiliar with the convention.

### Step 5 - CI
Enable continuous integration. You want to ensure that

* your usual tests pass
* your project works in a few versions of GHC
* your cabal file is well-formed
* a source distribution can be generated
* docs build cleanly

Here's a nice Travis config adapted from
[bitemyapp](https://twitter.com/bitemyapp) who got it from
[hvr](https://github.com/hvr).

```yaml
language: haskell

ghc:
 - 7.6
 - 7.8

before_install:
 - sudo add-apt-repository -y ppa:hvr/ghc
 - sudo apt-get update
 - sudo apt-get install happy-1.19.3
 - sudo apt-get install alex-3.1.3
 - export PATH=~/.cabal/bin:$PATH # for newer alex
 - cabal update
 - cabal install alex happy

install:
 - cabal install --only-dependencies --enable-tests --enable-benchmarks --force-reinstalls

script:
 # -v2 provides useful information for debugging
 - cabal configure --enable-tests --enable-benchmarks -v2

 # this builds all libraries and executables
 # (including tests/benchmarks)
 - cabal build

 - cabal test
 - cabal check

 # tests that a source-distribution can be generated
 - cabal sdist

 # check that the generated source-distribution can be built & installed
 - export SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}') ;
   cd dist/;
   if [ -f "$SRC_TGZ" ]; then
      cabal install "$SRC_TGZ";
   else
      echo "expected '$SRC_TGZ' not found";
      exit 1;
   fi
```

### Step 6 - dependencies
Add some constraints to your library's dependencies in the cabal
file.  Locking major versions of dependencies will help prevent
surprise Cabal build failures by your users. This is about the only
rule of thumb I know. Setting constraints more intelligently is
certainly possible, and I welcome your comments about your own
strategies.

### Step 7 - candidate package
Your tests pass, your docs look good. It's time to upload a "package
candidate" for a last check that things are OK on the real Hackage.

Create a source distribution

```bash
cabal sdist
```

This will generate `dist/your-pkg-x.y.z.tar.gz`. Select this file
in the [candidate
uploader](https://hackage.haskell.org/packages/candidates/upload)
and give it a go.

### Step 8 - release it
Some people like to leave their packages as candidates for a while
to find bugs etc. The quality on hackage is generally pretty high
so you want to avoid throwing things up there half-baked. Generally
if you've followed the previous steps you should be in pretty good
shape to release your package for real though.

```bash
$ cabal upload dist/your-pkg-x.y.z.tar.gz
```

All done, right? Time to celebrate! Not necessarily. Recently
Hackage has been failing to run haddock remotely to generate documentation.
The maintainers claim there is a small delay but I have found sometimes
it never happens. Thankfully [Edward Kmett](https://twitter.com/kmett)
created a script to build your own docs and push them to Hackage.

```bash
#!/bin/bash
set -e

if [ "$#" -ne 1 ]; then
  echo "Usage: scripts/hackage-docs.sh HACKAGE_USER"
  exit 1
fi

user=$1

cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

pkg=$(awk -F ":[[:space:]]*" 'tolower($1)=="name"    { print $2 }' < "$cabal_file")
ver=$(awk -F ":[[:space:]]*" 'tolower($1)=="version" { print $2 }' < "$cabal_file")

if [ -z "$pkg" ]; then
  echo "Unable to determine package name"
  exit 1
fi

if [ -z "$ver" ]; then
  echo "Unable to determine package version"
  exit 1
fi

echo "Detected package: $pkg-$ver"

dir=$(mktemp -d build-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --hoogle --hyperlink-source --html-location='/package/$pkg-$version/docs' --contents-location='/package/$pkg-$version'

cp -R dist/doc/html/$pkg/ $dir/$pkg-$ver-docs

tar cvz -C $dir --format=ustar -f $dir/$pkg-$ver-docs.tar.gz $pkg-$ver-docs

curl -X PUT \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     -u "$user" \
     --data-binary "@$dir/$pkg-$ver-docs.tar.gz" \
     "https://hackage.haskell.org/package/$pkg-$ver/docs"
```

Now you will have published a package that looks good and will be
be easy for people to use. So get that code off your computer and
onto Hackage and contribute to the glorious Haskell ecosystem!
