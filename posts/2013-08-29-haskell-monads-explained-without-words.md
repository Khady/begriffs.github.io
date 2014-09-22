---
title: Haskell Monads Explained Without Words
---

Shh! Words get in your way. Let's play functional charades, where
you will understand a function by watching it in silence. This is
part two of the "without words" series. It will help you to observe
[part one](2013-08-27-weird-symbols-in-their-native-tongue.html)
first.

Note you'll need to run in `ghci -XNoMonomorphismRestriction` for
it to allow some of this without you needing to specify type
signatures.


#### return, empty, join, >>=

```haskell
> pure 5 :: Maybe Int
Just 5
> return 5 :: Maybe Int
Just 5
> return 5 :: [Int]
[5]
> empty :: [Int]
[]
> empty :: Maybe Int
Nothing

> Just $ Just 5
Just (Just 5)
> join $ Just $ Just 5
Just 5
> join [[1,2], [3,4]]
[1,2,3,4]

> (\x -> [x,x]) <$> [1,2]
[[1,1],[2,2]]
> join $ (\x -> [x,x]) <$> [1,2]
[1,1,2,2]
> [1,2] >>= \x -> [x,x]
[1,1,2,2]

> Just 5 >>= return
Just 5
> [5] >>= return
[5]
> [5] >>= return . (+2)
[7]
> Nothing >>= return . (+2)
Nothing

> let prev = \x -> if x > 0 then return (x-1) else empty
> Just 1 >>= prev :: Maybe Int
Just 0
> return 1 >>= prev :: Maybe Int
Just 0
> return 1 >>= prev :: [Int]
[0]
> return 1 >>= prev >>= prev :: Maybe Int
Nothing
> [1,2] >>= prev
[0,1]
> [1,2] >>= prev >>= prev
[0]
> [1,2] >>= prev >>= prev >>= prev
[]
```
