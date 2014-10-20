---
title: Writing controller specs for a Warp server
---

This guide will show you how to configure a Cabal project to test
a Warp server (such as Scotty or Yesod) using hspec2. It will allow
you to test HTTP requests and responses and to prepare the database
before/between steps.

### directories and cabal

Assuming your project code lives in a top-level `src` directory,
create another top-level directory called `test`. We need to add a
new section to the project cabal file that builds the test suite
and can access the rest of the project. Append this and adjust
package versions to taste:

```haskell
Test-Suite spec
  Type:                exitcode-stdio-1.0
  Default-Language:    Haskell2010
  Hs-Source-Dirs:      test, src
  ghc-options:         -Wall -W -Werror
  Main-Is:             Main.hs
  Other-Modules:       -- other project modules
  Build-Depends:       base
                     , hspec2
                     , hspec-wai
                     , hspec-wai-json
                     , warp
                     , wai
```

Add `test/Main.hs`. This is a good place to run other pre-test tasks
like loading a database fixture.

```haskell
module Main where

import Test.Hspec
import Spec

main :: IO ()
main = hspec spec
```

Create `test/Spec.hs` containing this gobbledygook

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --no-main #-}
```

This will search for and run all Haskell files `spec/**/*Spec.hs`
as part of the test suite so you do not have to later remember to add
new spec files to a configuration list. We'll see later there is a
way to focus the tests when running the suite.

Finally build your project and make it available for testing.

```sh
$ cabal install -j --enable-tests
```

### beyond simple request specs

The [hspec-wai](https://hackage.haskell.org/package/hspec-wai)
package contains some matchers to make things easy.  Here's an
example from the readme showing simple reqeusts.

```haskell
app :: IO Application
app = S.scottyApp $ do
  S.get "/" $ do
    S.text "hello"

spec :: Spec
spec = with app $
  describe "GET /" $ do
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

    it "responds with 'hello'" $
      get "/" `shouldRespondWith` "hello"

    it "responds with 200 / 'hello'" $
      get "/" `shouldRespondWith` "hello" {matchStatus = 200}
```

I like to run the tests with colored output and showing all steps,
even those that pass.

```sh
$ cabal test -j --show-details=always --test-options="--color"
```

This is nice and simple, but what if you want to run a customized
check on a response? For instance, let's test that a header value
matches a regex. You can do it inside lifted IO.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders,simpleStatus))
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI(..))
import Text.Regex.TDFA ((=~))

matchHeader :: CI BS.ByteString -> String -> [Header] -> Bool
matchHeader name valRegex headers =
  maybe False (=~ valRegex) $ lookup name headers

spec :: Spec
spec = with app $
  describe "GET /list" $
    it "responds with valid range headers" $ do
      r <- request methodGet "/list"
             ["Range-Unit" <:> "items", "Range" <:> "0-9"]

      liftIO $ do
        simpleHeaders r `shouldSatisfy`
          matchHeader "Content-Range" "^0-[0-9]+/[0-9]+$"
        simpleStatus r `shouldBe` partialContent206
```

How do the tests know where to send requests? The trick is at the
beginning in the `with` function which is an alias for `before`
and defined in `hspec-wai` rather than `hspec` itself.

```haskell
before :: IO a -> SpecWith a -> Spec
```

We give it an IO action, in this case `app :: IO Application` and
it builds `SpecWith Application` which is a Reader monad that
future test steps can query. For instance, the `request` method
grabs the application internally with `getApp`.

```haskell
-- | Perform a request to the application under test, with specified HTTP
-- method, request path, headers and body.
request :: Method -> ByteString -> [Header] -> LB.ByteString
                  -> WaiSession SResponse
request method path headers body =
  getApp >>= liftIO . runSession (Wai.srequest $ SRequest req body)
  where
    req = setPath defaultRequest
            {requestMethod = method, requestHeaders = headers} path
```

Really `getApp` is a glorified `ask`, along with type constraints
that would make it fail to compile if the surrounding test was not
using `SpecWith Application`.

```haskell
getApp :: WaiSession Application
getApp = WaiSession ask
```

Another less explicitly monadic way to interact with the test subject
is using an argument in the `it` function. What goes into `with`
can come out in `it`.

```haskell
spec :: Spec
spec = with (return 42) :: IO Int $
  describe "This magical number" $
    it "is bigger than 40" $ \n ->
      n `shouldSatisfy` (>40)
```

Perhaps it's silly as stated, but imagine the integer is a database
connection instead. In fact this leads to the next topic...

### cleaning the db between tests

In contrast to the `before` family of functions which use `SpecWith a`
types, the `after` and `around` functions use `ActionWith a`. Internally
it's not much to speak of, but the alias will make our actions' types
read cleaner.

```haskell
type ActionWith a = a -> IO ()
```

What's a good use case for around actions? Cleaning up the environment
so tests do not pollute each other's state is one. Here's an example
of rolling back any changes to a Postgres database after each test.

```haskell
import Test.Hspec
import Database.HDBC
import Database.HDBC.PostgreSQL
import Control.Exception.Base (bracket)

withDatabaseConnection :: ActionWith Connection -> IO ()
withDatabaseConnection = bracket openConnection disconnect
  where openConnection = connectPostgreSQL' "postgres://etcetc"

withRollback :: ActionWith Connection -> IO ()
withRollback action = withDatabaseConnection $ \c -> do
  runRaw c "begin;"
  action c
  rollback c

spec :: Spec
spec = around withRollback $
  describe "inserting with abandon" $
    it "does all kinds of things" $ \conn ->
      -- here we can use the connection and be assured
      -- our sql commands will be rolled back
```

One tiny but important detail is the choice of `connectPostgreSQL'`
(with an apostrophe). The non-prime connect function in HDBC.PostgreSQL
enables auto-commit. This means it peforms every statement in a
transaction, which will cause surprises for you. The prime version
is for manual transaction management like we are using here.

Simply combine the `with app` and `around withRollback` to do
controller tests that include database cleaning. And don't forget
you can perform an action before the entire suite runs inside the
`Main.hs` we created.
