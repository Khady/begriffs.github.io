---
title: Magic numbers in polynomial hash functions
---

Every time I see copypasta polynomial string hash functions on the
internet I am mystified by the arcane and magical numbers they
contain. Today it's time to find out which numbers are acceptable
and why. Scanning stack overflow discussions and spending some time
at the blackboard has revealed the beginning of the secret.

Polynomial hashes are computed from a base number and the character
codes of an input string. Let \\(s_0 \\ldots s_{k-1}\\) be the codes
of each input character in string \\(s\\). It's our job to choose
constants \\(b\\) and \\(n\\) to minimize collisions in the hash
function \\(h(s) = \\sum b^i s_i\\ \\text{mod}\\ n\\), where \\(b\\)
is an arbitrary number and \\(n\\) is the number of buckets in our
hash table.

Increasing \\(n\\) certainly helps. If \\(n=1\\) then everything
will collide and we needn't worry about \\(b\\). So fix \\(n\\) as
large as sensible for application memory. We'll see that certain
choices of \\(b\\) are statistically better than others. Certain
choices are really bad.

Let's get to our conclusion the roundabout way and see what happens
when we pick bad values. Assume \\(n \\mid b\\), that is \\(b =
nm\\) for some \\(m\\). In this case \\(h(s) = \\sum b^i s_i = s_0
+ nmX \\equiv s_0\\ \\text{mod}\\ n\\). Hence only the first character
of the string affects the hash value. This is terrible performance.

More generally if \\(i \\mid b\\) and \\(i \\mid n\\) for \\(i >
1\\) then \\(b = ij\\), \\(n = ik\\) for some \\(j\\) and \\(k\\).
Thus each \\(\\sum b^i s_i\\) can be written \\(s_0 + ijX\\). As
in the previous case the \\(s_0\\) term turns out to be more important
than the others. Notice \\(s_0 + ijX \\equiv s_0 + ijY\\ \\text{mod}\\
ik\\) iff \\(ij(X-Y) \\equiv 0\\ \\text{mod}\\ ik\\) iff \\(j(X-Y)
\\equiv 0\\ \\text{mod}\\ k\\). That's not good -- the final terms,
whatever they may be, are modded by \\(k\\) which is \\(i\\) times
smaller than \\(n\\). Smaller modding means fewer bucket choices
which makes collisions more likely.

Which brings us to the first conclusion: choose \\(b\\) and \\(n\\)
to be relatively prime. Beware that integer arithmetic is already
modular, so \\(h(s)\\) is really \\(h(s)\\ \\text{mod}\\ 2^{32}\\).
Don't choose \\(b\\) as a power of two (in fact choose it to be
odd) or else \\(\\gcd{(b, 2^{32})} > 1\\).

This is why the typical hash function snippet on stack overflow
uses a prime for \\(b\\). The author doesn't know what you'll pick
for \\(n\\) so they play it safe. However there is still an interesting
question about which prime to pick. Sadly coprimality, while
necessary, is not sufficient to guard against collisions. I wrote
some code to test various strings and constants.

```haskell
import Control.Monad (replicateM)
import Data.Char (ord)
import Data.List (group, sort)

allStrings :: Int -> Int -> [Char] -> [ [Char] ]
allStrings from to alphabet = [from..to] >>= (`replicateM` alphabet)

numCollisions :: Ord a => [a] -> Int
numCollisions = sum . (filter (> 1)) . (map length) . group . sort

coprimes :: Int -> [Int]
coprimes n = [m | m <- [2..n], (gcd m n) == 1]

-- Horner's method for polynomial evaluation
horner :: (Num a) => a -> [a] -> a
horner x = foldr (\a b -> a + b*x) 0

hash :: Int -> Int -> [Char] -> Int
hash n b s = (horner b (map ord s)) `mod` n

len3collisions :: Int -> Int -> Int
len3collisions n b = numCollisions $ map (hash n b) $ allStrings 3 3 ['a'..'z']
```

There are \\(26^3 = 17576\\) length three strings of lowercase
letters. If we let \\(n = 17576\\) and run through all relatively
prime choices of \\(b &lt; n\\) there are plenty of bad values. To
get a feel for how the performance varies, I sorted the number of
keys that collide with any other keys as \\(b\\) varies. (The x-axis
below is **not** \\(b\\).) The graph gives a feeling for the range
of success.

![Hash collisions](/images/hash-chart.png)

For the best \\(b\\) a whole 89% of keys are collision-free. At the
worst end all but six collide. Apparently there is some deeper stuff
going on. That's as far as I'm going to take it for now.
