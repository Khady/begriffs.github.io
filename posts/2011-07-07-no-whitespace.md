---
title: No Whitespace
---


Code editors should manage text layout on screen, and they should
not save whitespace in the file. Forget tabstops and newlines. A
typewriter is not a parse tree. We should move through source files
not by line but by block. For instance, jumping from function to
function, or argument to argument, or statement to statement —
whatever our currently chosen granularity.

We already have sophisticated mathematical typesetting with systems
like TeX, so I am confident an editor could eventually handle the
nuances of displaying code pleasantly. For instance, lining up the
equal signs in a clump of variable initialization, or wrapping
function arguments to align with each other and not with the start
of the function name. The point is that our manual whitespace
management in a teletype metaphor is a waste of time.

Whitespace creates needless incompatibilities like the UNIX vs
Windows newline convention. How telling that our whitespace vocabulary
uses pseudo-equestrian phrases like carriage return. Whitespace
disagreements irritate teams, and team members' subsequent whitespace
battles appear as vacuous changes in their version control systems.
