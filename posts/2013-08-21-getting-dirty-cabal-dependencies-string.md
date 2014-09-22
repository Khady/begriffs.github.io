---
title: Getting dirty - cabal dependencies, string types, JSON
---

Tonight we leave the calm of the stratosphere and descend into the
babbling of human language and strife of cabal dependencies. In
short, I tried to do a regular programming thing.

JSON. It's passing messages the world over, and I'd like my program
to speak it too. Some Googling reveals that
[bos/aseson](https://github.com/bos/aeson) is the thing to use. I
try to install it with cabal and apparently all the other things I
had previously installed have put me in "cabal hell" and I have
contradictory dependencies. No problem, blew all my installation
away with `rm -fr ~/.ghc ~/.cabal` and installed
[hsenv](https://github.com/Paczesiowa/hsenv). For you Ruby people,
gem = cabal and rvm = hsenv. Even though doing so takes more space
on your disk you should use hsenv.

So let's take an example from the Aeson docs and parse a JSON array.

```haskell
Prelude> :m +Data.Aeson
Prelude Data.Aeson> decode "[1,2,3]" :: Maybe [Int]

<interactive>:3:8:
    Couldn't match expected type `Data.ByteString.Lazy.Internal.ByteString'
                with actual type `[Char]'
    In the first argument of `decode', namely `"[1,2,3]"'
    In the expression: decode "[1,2,3]" :: Maybe [Int]
    In an equation for `it': it = decode "[1,2,3]" :: Maybe [Int]
Prelude Data.Aeson> :t decode
decode
  :: FromJSON a =>
     Data.ByteString.Lazy.Internal.ByteString -> Maybe a
```

Wait, what's wrong with my string? Turns out that we often mean
several things by "string" and these things are disambiguated in
Haskell. One meaning is an array of Chars, which are big fat UTF-32
characters all in memory at once. Sadly this is wrong for JSON in
two ways. First we often want to decode JSON from a stream, aka
lazily. Second JSON is UTF-8. Enter `Data.ByteString.Lazy.Char8`,
a representation of a lazy sequence of bytes which is an instance
of the `IsString` typeclass. Perfect for slurping UTF-8 in from a
socket, and exactly what Aeson uses.

We could explicitly convert our string literals to this type, but
there's a GHC extension to make this nicer. Enabling it will infer
the type of string needed from context and use the function
`fromString` (derived from `IsString`) to convert a string literal
to the right type as needed. Once we import the needed type and
enable the GHC extension our example works like we would expect.

```haskell
-- having started ghci with -XOverloadedStrings
Prelude> :m +Data.Aeson
Prelude Data.Aeson> :m +Data.ByteString.Lazy.Char8
Prelude Data.Aeson Data.ByteString.Lazy.Char8> decode "[1,2,3]" :: Maybe [Int]
Just [1,2,3]
```

Tomorrow I'll continue toward making a real API web service in
Haskell. It will power an in-app messaging ajax widget for other
web sites. I met some people last week here in SF who want to add
this type of feature to their site so I thought it would be a good
way to put my Haskell to the test and power a JS widget. Who knows,
maybe I can learn about compiling Haskell to JS to make the widget.
All kinds of fun ahead!
