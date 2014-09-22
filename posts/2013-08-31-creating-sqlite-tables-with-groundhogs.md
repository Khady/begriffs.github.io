---
title: Creating sqlite tables with Groundhog's default settings
---

I simplified a Groundhog code example to see how it behaves with
its default settings. To make this code work you'll need to install
some cabal packages.

```bash
cabal install groundhog
cabal install groundhog-sqlite
cabal install groundhog-th
```

Now for the code. We'll declare tables for Machines and Parts, and
create a relationship to say that machines have many parts. Then
migrate out in-memory database, insert some sample data and query
it.

```haskell
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Machine = Machine { modelName :: String, cost :: Double } deriving Show
data Part = Part { partName :: String, weight :: Int, machine :: DefaultKey Machine }
deriving instance Show Part

mkPersist defaultCodegenConfig [groundhog|
- entity: Machine
- entity: Part
|]

main = withSqliteConn ":memory:" $ runDbConn $ do
  runMigration defaultMigrationLogger $ do
    migrate (undefined :: Machine)
    migrate (undefined :: Part)

  megatron <- insert $ Machine "Megatron 5000" 2500.00
  insert $ Part "Megamaker" 50 megatron
  insert $ Part "Tiny Bolt" 1 megatron

  microtron <- insert $ Machine "Microtron 12" 19.99
  insert $ Part "Insignificonium" 2 microtron

  partsForMegatron <- select $ (MachineField ==. megatron) `orderBy` [Asc PartNameField]
  liftIO $ putStrLn $ "Parts for the Megatron: " ++ show partsForMegatron
```

Declaring the data types is standard Haskell. Just some data types
that derive `Show`. I did notice that I needed to declare `cost`
as a `Double`. I tried `Float` initially but it failed.

The first magic appears with `mkPersist`. Notice it takes a quasiquoted
groundhog DSL. I stripped down the declaration here to declare
tables with the default settings. If you remove this code then
things break. The `mkPersist` function writes code for you. Its DSL
supports _a lot_ of options, and we'll (dis)cover them in later
posts.

Given our declarations we can now run migrations. Here's is the SQL
it generates:

```sql
CREATE TABLE "Machine" ("id" INTEGER PRIMARY KEY NOT NULL, "modelName" VARCHAR NOT NULL, "cost" REAL NOT NULL)
CREATE TABLE "Part" ("id" INTEGER PRIMARY KEY NOT NULL, "partName" VARCHAR NOT NULL, "weight" INTEGER NOT NULL, "machine" INTEGER NOT NULL, FOREIGN KEY("machine") REFERENCES "Machine"("id"))
```

Not bad! It adds NULL constraints, declares keys, and links the
tables together with references.

Next we insert some sample data into the tables. Straightforward.
Notice how it then builds a query, but you don't write SQL directly.
Writing the queries in Haskell prevents injection and supposedly
allows the queries to easily compose. Groundhog introduces its own
operators to connect SQL conditions (called `Conds`). One such is
`==.`.

```haskell
> :t (==.)
(==.)
  :: (Database.Groundhog.Expression.Unifiable a b,
      Database.Groundhog.Expression.Expression db r a,
      Database.Groundhog.Expression.Expression db r b) =>
     a -> b -> Cond db r
```

Try building and running the code. Seems to work fine.
