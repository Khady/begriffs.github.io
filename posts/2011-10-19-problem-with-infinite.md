---
title: A Problem with the Infinite
---

I am having a problem adding symmetry groups to
[Coffeecup](https://github.com/begriffs/coffeecup). It is possible
to assert that two functions are extensionally equal. The assertion
wraps the functions so that as they are called they will check what
the other says and make sure they agree. This can identify that
functions differ, but there is really no way to know if they are
the same, no finite number of tests.

A new programming paradigm comes to mind. Any time a conditional
asks if functions are equal, just assume they are, and add an
assertion to that effect. If the assertion is invalidated later,
rewind to the conditional and take the path that says they aren't.
Computation is in this way like a flashlight which illuminates small
portions of the infinite. A computation path makes assumptions and
continues to operate on them until it discovers an inconsistency.

For instance, suppose we have a Group class and want to know the
order of one of its elements. The internal code would raise the
element to various powers until it found that the result is equal
to the group identity, then return that power. So imagine how it
might work in the "backtracking flashlight" paradigm. Our first
test will be if the argument itself (raised to the first power) is
the idenity. The equality comparison will happily say it is, and
onion testing code will be added to watch out for inconsistencies
later. So the order function will return 1. The calling code will
operate under this assumption. If an inconsistency arises (such as
that this element doesn't act like the idenity in a future
multiplication), the code will stop and restore itself to its
previous state inside the order method. It will be back in the order
function testing if the element to the first power equals the group
identity, and this test will fail. It will then raise the power,
make the equality test, and return that the order is two. The calling
code will try this scenario.

The execution path of a successfully terminated program written
this way can be considered a "model" of its constraints. Perhaps
this approach would allow a program to be written in terms of actual
completed infinities, to operate exactly and symbolically on infinite
data structures.
