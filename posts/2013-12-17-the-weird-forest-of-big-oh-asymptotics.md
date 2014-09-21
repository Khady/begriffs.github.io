---
title: The weird forest of "Big-Oh" asymptotics
---

You may have a decent intuition of common algorithm time complexities
— recognizing one as linear time, another quadratic, etc — but still
feel uncomfortable about the details of "Big-Oh" notation. It's no
wonder. The notation has long been abused by computer science
textbooks, and is used today in sloppy jargon.

This article will show you how to reason about the asymptotic
behavior of functions and then demonstrate that their growth rates
form a strange tapestry of which most textbooks show you only a
thread.

Let's start with some unconventional notation that I think is more
suggestive than Big-Oh and easier for reasoning. The fundamental
definition is this. If \\(f\\) and \\(g\\) are functions \\(\\mathbb{R}
\\to \\mathbb{R}\\), we say \\(f \\lesssim g\\) iff there is a \\(c
\\in \\mathbb{R}^+\\), and an \\(x_0 \\in \\mathbb{R}\\) such that
\\(\\left|f(x)\\right| &lt; c\\left|g(x)\\right|\\) for every \\(x
&gt; x_0\\). You can read \\(f \\lesssim g\\) as "\\(g\\) _linearly
dominates_ \\(f\\)."

The linear dominance relation allows us to compare the growth of
functions from afar, ignoring inconsequential details. The notation
I chose, \\(f \\lesssim g\\), looks like an order relation and it
almost is, but it's looser than you might imagine.

For instance \\(f \\le g\\) (pointwise) implies \\(f \\lesssim g\\)
(just let \\(c = 1\\) and \\(x_0 = 0\\)), but the converse is not
always true. A counterexample is \\(2x &gt; x\\) whereas \\(2x
\\lesssim x\\), which you can see by picking \\(c = 3\\). (The
notation \\(2x\\) above is an abbreviation for the function \\(x
\\mapsto 2x\\).) When \\(f \\lesssim g\\) and \\(g \\lesssim f\\)
we'll write \\(f \\sim g\\). So our example isn't too weird, it's
just the case that \\(x \\sim 2x\\). These functions grow at linearly
similar speed.

Now we can succinctly define the standard notation. We'll continue
to work with the linear dominance notation though, because it's
easier.
\\[&nbsp;\\begin{array}{lcl}
O(f) &amp; = &amp; \\{ g \\mid g \\lesssim f \\} \\\\
\\Omega(f) &amp; = &amp; \\{ g \\mid g \\gtrsim f \\} \\\\
\\Theta(f) &amp; = &amp; \\{ g \\mid g \\sim f \\} \\end{array}
\\]

At this point you should notice two ways that the standard presentation
is misleading. First, people tend to write stuff like \\(2n =
O(n)\\). The literal meaning of this statement is senseless. What
people mean is \\(2n \\in O(n)\\). Alternatively it makes sense to
say \\(O(2n) = O(n)\\). Second, why use \\(O\\) when you could use
\\(\\Theta\\)? The former is underspecified. Sure \\(2n \\in O(n)\\)
but \\(2n \\in O(n^2)\\) and \\(O(n!)\\) too. Oftentimes we know
exactly how complex an algorithm is, so using Big-Oh is like me
saying, "I'll give you a hint, I'm less than eight feet tall!" It's
a computer science convention to give the smallest \\(O\\) value
and imply a statement about \\(\\Theta\\).

Linear dominance, and hence \\(O\\)-sets have properties that will
enable us ignore unimportant details. First, dominance is a _preorder_,
which is to say reflexive and transitive. Certainly \\(f \\lesssim
f\\) by picking \\(c = 1\\) with any \\(x_0\\). Also if \\(f \\lesssim
g\\) and \\(g \\lesssim h\\) by parameters \\(c\\), \\(x_0\\) and
\\(c'\\), \\(x_0'\\) then \\(f \\lesssim h\\) by parameters \\(max(x_0,
x_0')\\) and \\(cc'\\). It's not quite a partial order on functions
themselves because it's not anti-reflexive (e.g. \\(x \\lesssim
2x\\) and \\(2x \\lesssim x\\) but \\(x \\ne 2x\\)). However a
slight modification provides a partial order on \\(\\Theta\\)-classes:
say \\(\\Theta(f) \\le \\Theta(g)\\) iff \\(f \\lesssim g\\). This
operation is well-defined and forms a partial order due to a general
theorem about pre-orders. Let's examine ways to simplify the
descriptions of \\(\\Theta\\) sets, and then return to considering
the shape of the order itself.

Polynomials provide a good setting to learn how to reason about
dominance. For an arbitrary polynomial \\(f\\), which \\(\\Theta\\)-class
contains it? And do multiple polynomials live in the same class?
Certainly \\(f \\in \\Theta(f)\\), but depending on the expansion
of \\(f\\) this can be unwieldy. Happily, we can always find simple
representatives of polynomial complexities.

To do this, first, notice \\(cf \\in \\Theta(f)\\) for any \\(c
\\in \\mathbb{R} \\setminus \\{0\\}\\). Just use the definition and
check that \\(f \\sim cf\\) like we did for \\(x\\) and \\(2x\\)
previously. Also \\(f \\lesssim h\\) and \\(g \\lesssim h\\) implies
\\(f + g \\lesssim h\\) since \\(\\left|f\\right| \\le
c\\left|h\\right|\\), \\(\\left|g\\right| \\le c'\\left|h\\right|\\)
past \\(x_0\\) and \\(x_0'\\) respectively means \\(\\left|f +
g\\right| \\le \\left|f\\right| + \\left|g\\right| \\le c\\left|h\\right|
+ c'\\left|h\\right| = (c + c')\\left|h\\right|\\) past \\(max(x_0,
x_0')\\).

Also because dominance was defined on the absolute value of functions
we get that \\(f \\lesssim f + g\\) for any \\(f\\) and \\(g\\).
Combining this with the previous paragraph you can see that \\(f
\\lesssim g\\) implies \\(g \\sim g + f\\). These facts will allow
us to recognize when polynomials share a \\(\\Theta\\)-class.

OK, we have tools to identify functions that grow at a similar rate,
but how do we find strictly larger ones? Non-constant multiplication
is one way. Let \\(L = x \\mapsto x\\). Then \\(f \\lnsim Lf\\) for
any \\(f \\gnsim 0\\). Why? No matter which \\(c \\in \\mathbb
R^+\\) we choose, we find that \\(c\\left|f(c+1)\\right| &lt;
(c+1)\\left|f(c+1)\\right| = \\left|(Lf)(c+1)\\right|\\). So there
are infinitely many complexities of polynomials, forming a chain
\\(O(1) \\subset O(x) \\subset O(x^2) \\subset \\cdots\\).

This chain includes all monic monomials \\(\\{x^i\\}\\), but where
does an arbitrary polynomial fit in? Using the rules about addition
and constant multiplication we can show that any polynomial is in
\\(\\Theta(x^i)\\) for some \\(i\\). For example, \\(ax^2 \\sim
x^2\\), \\(bx \\sim x\\) and \\(c \\sim 1\\) so \\((ax^2 + bx + c)
\\sim (x^2 + x + 1)\\). Also \\(x^2 \\sim (x^2 + x) \\sim (x^2 + x
+ 1)\\) since \\(1 \\lesssim x \\lesssim x^2\\). Hence \\(ax^2 +
bx + c \\in \\Theta(x^2)\\). In general doing this procedure
repeatedly on any polynomial shows that \\(\\sum a_i x^{b_i} \\in
\\Theta(x^{max \\{b_i\\}})\\).

That takes care of polynomials. Their complexities are unbounded
and well-ordered, with a smallest element. Their order mirrors the
order of their degrees. Don't get too comfortable; complexity classes
in general are arranged much more chaotically.

Here's where things get weird and interesting. Despite there being
infinitely many \\(\\Theta(1) &lt; \\Theta(x) &lt; \\Theta(x^2)
&lt; \\cdots\\) there are other functions that linearly dominate
the whole lot of them. One such function is \\(x^x\\). It dominates
any polynomial. As we saw any polynomial \\(p\\) is a member of
\\(\\Theta(x^k)\\) for some \\(k\\). And \\(\\Theta(x^k) &lt;
\\Theta(x^x)\\) because for all \\(k\\) and \\(c\\), there is an
\\(x\\) such that \\(c\\left|x^k\\right| &lt; |x^x|\\), namely \\(x
= ck\\).

So much for a nice little chain. Aside from functions able to
dominate whole unbounded chains of others, there is the strange
fact of what happens between the functions. For instance there are
plenty of commonly-encountered complexity classes between polynomials
and the superexponential \\(x^x\\). What may surprise you is that
there are complexity classes between _any_ two others. For suppose
\\(f \\lnsim g\\). Then I claim \\(f \\lnsim \\sqrt{fg} \\lnsim
g\\). Think what this means. Any significant gap between functions
contains another function that still leaves a significant gap between
its fellows. For large enough inputs it can make or break a timely
result.

The proof is a bit fiddly, so let's assume \\(f\\) and \\(g\\) are
everywhere positive. We'll start by proving the inclusion. Assume
\\(f \\lesssim g\\). Then \\(f(x) \\le cg(x)\\) for some \\(c\\)
when \\(x\\) gets big enough. Let's omit the \\(x\\). Hence \\(f =
\\sqrt{f^2} \\le \\sqrt{cfg} = \\sqrt{c}\\sqrt{fg}\\). This means
\\(f \\lesssim \\sqrt{fg}\\) since \\(\\sqrt{c}\\) is a constant.
Also \\(\\sqrt{fg} \\le \\sqrt{cg^2} = \\sqrt{c}g\\) so \\(\\sqrt{fg}
\\lesssim g\\).

Now we'll prove the strictness of the inequality. Recall that \\(f
\\lnsim g\\) means \\(f \\lesssim g\\) and \\(\\neg (g \\lesssim
f)\\). It's easier to prove the contrapositive: \\(\\sqrt{fg}
\\lesssim f\\) or \\(g \\lesssim \\sqrt{fg}\\) implies \\(g \\lesssim
f\\). Assume the first possibility, \\(\\sqrt{fg} \\lesssim f\\).
Then \\(\\sqrt{fg} \\le cf\\) so \\(fg \\le c^2 f^2\\) so dividing
by \\(f\\), \\(g \\le c^2 f\\). Cool. Assume the second possibility,
\\(g \\lesssim \\sqrt{fg}\\). Then \\(g \\le c\\sqrt{fg}\\) so
\\(g^2 \\le c^2 fg\\) so dividing by \\(g\\), \\(g \\le c^2 f\\).
Done.

Of course the finer we cut between functions the larger the inputs
it takes before we feel the difference in dominance. For practical
applications it doesn't matter if there's a function between \\(x!\\)
and \\(x^x\\) since we won't be computing out there anyway. But the
theorem does hint there might be a cosmos of intricate algorithms
to fill the gaps between the well-known complexities.

To summarize, we saw that \\(\\Theta\\)-classes are densely ordered.
The next surprise (at least for me) is that they are not totally
ordered. That is, the elements can't be arranged in a single line,
having a definite place with respect to every other. Some functions
forever alternately outperform each other, so you can't say that
either ever dominates the other. For instance \\(\\neg (1 \\lesssim
x\\cdot sin(x))\\) and \\(\\neg (x\\cdot sin(x) \\lesssim 1)\\).
They diverge beyond any constant multiple, and neither has the upper
hand consistently.

Asymptotic dominance is a bit of a riddle. It's a dense partial
order with infinite chains and looks stranger than introductory
articles led me to believe. The twelve [function
families](https://en.wikipedia.org/wiki/Big_O_notation#Orders_of_common_functions)
listed on Wikipedia's Big-O article are like twelve months which
hold mysterious days and hours.
