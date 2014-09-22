---
title: Onion Testing
---

I am experimenting with layered code tests. Each layer is more
specific and strict. The **layers test continuously**, watching the
behavior of the code. Beware, **the tests are relentless** and can
hurt your feelings.

Let me illustrate in the Coffeescript interpreter with an example
from algebra. You can grab the code
[here](https://github.com/begriffs/mother-structures/tree/develop) and
play along. Let's define some sets and operations. First we'll
include the Coffeecup structures library.

```coffeescript
coffee> s = require './Structure.coffee'
{ Set: [Function: Set],
  Magma: [Function: Magma],
  Monoid:
   { [Function: Monoid]
     __super__: { op: [Function], pow: [Function], cross: [Function] } } }
```

Sets add **extra type checks** to the language and are the first
layer of the onion. Let's create the sets of all integers and odd
integers.

```coffeescript
coffee> Z = new s.Set (x) -> x == (Math.floor x) and x < Infinity
{ char: [Function] }
coffee> Odd = Z.where (x) -> x%2 in [1,-1]
{ char: [Function] }
```

Note that the "[1,-1]" thing corrects a
[bug](http://javascript.about.com/od/problemsolving/a/modulobug.htm) in
the underlying JavaScript modulo operator. Here is what these sets
report.

```coffeescript
coffee> Odd.contains -1
true
coffee> Odd.contains 2
false
coffee> Odd.contains 3.14
false
```

Now let's **add a layer** to the onion. We'll associate a binary
operation with Z and Odd and assert that they form
[magmas](http://bit.ly/pMsgVz). This means the sets are closed under
the operation. We will use ordinary addition as our operation.

```coffeescript
coffee> Z = new s.Magma Z, ((x,y) -> x+y)
{ s: { char: [Function] }, o: [Function] }
coffee> Odd = new s.Magma Odd, ((x,y) -> x+y)
{ s: { char: [Function] }, o: [Function] }
```

Take them for a test drive.

```coffeescript
coffee> Z.op 2, 2
4
coffee> Odd.op 2, 2
AssertionError: values outside of magma
```

The magma adds a check to protect us from elements outside its
domain.  Furthermore,

```coffeescript
coffee> Odd.op 1, 3
AssertionError: magma operation not closed
```

This response highlights that the operation we chose (simple addition)
is not suitable for making a magma out of odd numbers. **Onion
testing always watches** what we do and compares it to what we
assert.

Now let's see how onion testing can **have a memory**. We want to
assert that strings form a [monoid](http://en.wikipedia.org/wiki/Monoid)
under concatenation. This means, in part, that concatenation is
associative. A failure of associativity only manifests after several
uses of an operation so this layer of the onion must compare previous
operation results to uncover it.

We will define two operations on strings, **normal** concatenation
and **weird** concatenation. We will assert they are both monoids.
Monoids add another layer of testing to magmas.

```coffeescript
coffee> Str = new s.Monoid (new s.Set (x) -> typeof(x) == 'string'), ((x,y) -> (x+y)), ''
{ s: { char: [Function] },
  o: [Function],
  i: '',
  log: [] }
coffee> WeirdStr = new s.Monoid (new s.Set (x) -> typeof(x) == 'string'), ((x,y) -> (x+x+y)), ''
{ s: { char: [Function] },
  o: [Function],
  i: '',
  log: [] }
```

First try using Str.

```coffeescript
coffee> Str.op 'a', 'b'
'ab'
coffee> Str.op 'ab', 'c'
'abc'
```

Now try the same on WeirdStr.

```coffeescript
coffee> WeirdStr.op 'a', 'b'
'aab'
```

**So far so good** in its own weird way. But is it associative?

```coffeescript
coffee> WeirdStr.op 'aab', 'c'
AssertionError: monoid not left-associative
```

**No.** The testing uncovered the fact that in this weird concatenation
`('a' + 'b') + 'c' = 'aabaabc'` whereas `'a' + ('b' + 'c') = 'aabbc'`.
It tried things both ways behind the scenes. Written more concisely,
these are the errors we get from WeirdStr.

```coffeescript
coffee> WeirdStr.op (WeirdStr.op 'a', 'b'), 'c'
AssertionError: monoid not left-associative
coffee> WeirdStr.op 'a', (WeirdStr.op 'b', 'c')
AssertionError: monoid not right-associative
```

I'll be adding more structures to Coffeecup and will continue
developing this layered testing approach.
