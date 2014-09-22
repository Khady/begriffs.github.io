---
title: Haskell Applicative Functors Explained Without Words
---

Shh! Words get in your way. Let's play functional charades, where
you will understand a function by watching it in silence.

#### <$>

```haskell
> (5+) $ 2
7
> (5+) <$> Just 2
Just 7
> (5+) <$> Nothing
Nothing
> (5+) <$> [1,2]
[6, 7]
> (5+) <$> []
[]

> ((5+) . read) $ "2"
7
> ((5+) . read) <$> getLine
2<Enter>
7

> (5+) $ (3+) 2
10
> (5+) <$> (3+) $ 2
10

> (5+) <$> Left 2
Left 7
> (5+) <$> Right 2
Right 7

> getZipList $ ZipList [1,2]
[1,2]
> getZipList $ (+5) <$> ZipList [1,2]
[6,7]
```

#### <*>

```haskell
> (+) <$> (Just 5)
Just (+5)
> Just (+5) <*> (Just 2)
Just 7
> (+) <$> Just 5 <*> Just 2
Just 7

> (||) True False
True
> (||) <$> (Just True) <*> (Just False)
Just True
> (||) <$> (Just True) <*> Nothing
Nothing

> (||) <$> (Left True) <*> (Right False)
Left True
> (||) <$> (Right True) <*> (Left False)
Left False
> (&&) <$> (Right False) <*> (Left False)
Left False
> (+) <$> (Left 5) <*> (Left 2)
Left 5

> (+) 5 2
7
> (+) <$> [5] <*> [2]
[7]
> (+) <$> [1,2] <*> [10,100]
[11,101,12,102]

> getZipList $ (+) <$> ZipList [1,2] <*> ZipList [10,100]
[11,102]

> (++) "ab" "cd" :: [Char]
"abcd"
> (++) <$> getLine <*> getLine
ab<Enter>
cd<Enter>
"abcd"
```

#### pure

```haskell
> 5
5
> (pure 5) :: Maybe Int
Just 5
> (pure 5) :: Either Int Int
Right 5
> (pure 5) :: [Int]
[5]
> getZipList $ (pure 5 :: ZipList Int)
[5,5,5,5,5,5 ...]
> (pure 5) 2
5

> (+) <$> Just 5 <*> pure 2
Just 7
> (+) <$> [1,2,3] <*> pure 5
[6,7,8]
> getZipList $ (+) <$> ZipList [1,2,3] <*> pure 5
[6,7,8]
> (++) <$> getLine <*> pure "!"
hi<Enter>
"hi!"
> (+2) <$> pure 5
7
```

Now you know.
