---
title: An example of software transactional memory
---

Yesterday I took a train for hours to consult a Haskell
[guru](https://github.com/sciolizer). I now bring unto you his
wisdom.

Let's take advantage of Haskell's efficient threads and the elegance
of the STM pattern to write a short program to spawn workers and
consume the results as they are generated. It may help to watch
this
[presentation](http://blip.tv/oreilly-open-source-convention/oscon-2007-simon-peyton-jones-322473)
about STM (it is not specific to Haskell) before looking at my
example code.

Part of what might look unfamiliar about the code is its use of
`Data.Sequence`. That library provides a queue structure which
allows efficient insertion to the beginning or end. Internally it
is a tree, so we use a trick to pattern-match it in a function. But
one thing at a time.

Ready, set, read.

```haskell
module Main where
import Data.Sequence
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main = do
  queue2 <- atomically $ newTVar empty
  queue1 <- atomically $ newTVar empty
  total <- atomically $ newTVar 0
  forkIO $ 100 `replicateM_` (work queue1 >> threadDelay 100000)
  forkIO $ 100 `replicateM_` (work queue2 >> threadDelay 100000)
  200 `replicateM_` consumer total queue1 queue2

work :: TVar (Seq Int) -> IO ()
work queue = atomically $ modifyTVar queue (1 <|)

consumer :: TVar Int -> TVar (Seq Int) -> TVar (Seq Int) -> IO ()
consumer total p q = print =<< atomically (pull p `orElse` pull q) where
  pull queue = do
    seq <- readTVar queue
    case viewl seq of
      h :< rest -> do
        writeTVar queue rest
        writeTVar total . (+h) =<< readTVar total
        readTVar total
      EmptyL -> retry
```

Here is what this program does in general. There are two worker
threads and a consumer. Each worker appends to its own queue of
results. The consumer reads off of whichever queue has new values
ready, adds the value to a running total, and prints the total so
far. But it works differently than traditional lock-based concurrency
because the pieces of code are done inside transactions which can
be composed.

The secret ingredients are `atomically`, `retry`, and `orElse`.
Doing a collection of actions atomically means they happen in a
transaction -- either all or none of them will occur. If two threads
try to modify the same variables at once within distinct atomic
blocks then Haskell will consult a log of operations, reset the
variables and try again. What's cool is that a programmer using the
library can work in the "high level" language of STM and specify
how to compose atomic blocks including when to explicitly retry.
Our example is the consumer function.

On line 19 it enters into an atomic block which tries taking elements
from a sequence `p` or a sequence `q`. It does this with another
function we defined called pull. The pull function unwraps the STM
sequence variable and checks if it has an element in it. If it finds
one it updates another STM variable and returns it wrapped back up.
(I'll get to what this wrapping up means.) If pull detects that the
sequence is empty then it calls `retry`.

What does `retry` do? It goes back to the start of the atomic block
on line 19, which looks for what to try next. In this case it sees
an `orElse`, and it knows it has just tried pulling `p`, so it tries
pull on `q`, which is the second argument to `orElse`. So look at
that, we composed some options into an atomic block!

OK, but you ask: what if pull `q` finds an empty sequence and retries
as well? More Haskelly magic, that's what. It suspends the current
thread, but it somehow knows the variables referred to by the block
and will awaken the thread if anything else updates the variables
and will try again, starting with pull `p`. (Remember that "pull"
is a custom function we wrote ourselves, it's not part of the STM
library.)

I learned more about Haskell programming by trying to construct
this program. Joshua helped me when I got stuck but let me flail
around for a while first, which was helpful. Let me summarize the
ways I learned to think about writing these functions.

Whenever you mutate data you do it inside an IO monad. When you're
working "inside a monad" you can use the "do" command for concision.
However you have to consistently stay inside the same kind of monad
for each line of the do block. Looking at the type signatures of
some of the functions involved in this program can help orient you
to the general flavor of each of the do blocks.

```haskell
> :t newTVar
newTVar :: a -> STM (TVar a)
> :t readTVar
readTVar :: TVar a -> STM a
> :t writeTVar
writeTVar :: TVar a -> a -> STM ()
> :t modifyTVar
modifyTVar :: TVar a -> (a -> a) -> STM ()
 
> :t retry
retry :: STM a
> :t orElse
orElse :: STM a -> STM a -> STM a
 
> :t atomically
atomically :: STM a -> IO a
> :t forkIO
forkIO :: IO () -> IO ThreadId
```

Look how everything inside `main` returns `IO`. This means `main`
is concerned with changing things. It gets stuff done. So do `work`
and `consume`. However pull is an entirely different animal.
Everything inside its do block returns type STM. I think of things
that return STM as just orchestrating how things will change when
they do change. STM variables direct the evil eye of Haskell to
watch things changing and reset them as appropriate. Doing the
`readTVal` and `retry` and all that is like setting the dominoes
into an interesting pattern. Calling `atomically` is like knocking
down the first domino, as its return type of `IO` would indicate.

Other random notes about the code. Don't worry about the `viewl`,
`<|`, and `:<` weirdness. It is defined to pattern-match and modify
a `Data.Sequence`. They are specifics of a library. Module writers
are free to define custom operators so you'll see a lot of them
introduced. Also check out the "backwards bind" on lines 19 and 25.
Joshua advises that if you're using the regular bind your code is
unclear because other operators like function composition work from
right to left. If you match that order with a backwards bind you
can scan a line uniformly from end to beginning. For instance "read
the variable, add a value to it, write it (on line 25).

Finally, it's a good idea to declare the type of your functions.
Haskell can deduce it just fine, so it might feel redundant, but
when a type error ever happens in the way you use your functions
it will tend to be reported high up the call stack. That makes it
hard for you to debug. So throw type declarations liberally so the
errors are matched closer to their cause.

So that's STM in a nutshell. Go forth and concur.
