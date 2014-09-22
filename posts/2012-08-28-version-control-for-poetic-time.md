---
title: Version control for poetic time travelers
---

Matthew McCullough ([@matthewmccull](http://twitter.com/matthewmccull))
leads Git workshops across the world. He recently visited Madison,
Wisconsin to discuss and demonstrate advanced topics such as rerere
merging, the reflog, interactive rebase, cherry-pick, show-branch,
branch filtering, submodules, querying the log, refspecs, and path
aliases.

However, to me, Matthew's most fascinating observation is that Git
excels at representing changes independently of time. It allows
developers to tell a story that is somehow outside of time and
space. Git intentionally separates the bureaucracy of time from the
narrative of code.

The mechanism of this time travel is known as rebasing and like any
time travel story one must be aware of paradox. We'll see later how
Git paradoxes arise and how to prevent them.

First a little background. When developers change code simultaneously
and want to reconcile their changes they have two options: merging
and rebasing. Merging preserves a record of actual time. Parallel
branches show the passage of time, and come together at a merge
commit. The graph-order of commits along each branch remain faithful
to the order they happened.

If we think of code changes as telling a real story, a story like
any we would read or tell in English, then why not analyze it as
such? For instance, we can apply ideas from Aristotle's <u>Poetics</u>
when thinking about our code. Literate programming indeed.

Let's examine how a big merge functions in our "story." Often the
differences to be merged are substantial, and conflicts occur. One
person often resolves all conflicts in a single commit. This should
be familiar from literature -- it's called _deus ex machina_! A
deeply troubled plot suddenly ends aright due to unexplained
mysterious actions (in this case of a single individual). Aristotle
criticizes this kind of plot device because the plot's unraveling
should arise from the plot itself.

In fact a big merge is worse than its literary counterpart. A merge
can be opaque, encompassing so many aspects that only the person
doing the merge knows how it all works. This is not good documentation.

By contrast, a rebase rewrites the history of a branch into similar,
but entirely different commits. Some people criticize rebasing as
being a lie. The order and even the changes between commits are
just not what they were before. But this is nothing new; it's called
fiction. The story, while false, still instructs us. Aristotle wrote
that "poetry is finer and more philosophical than history; for
poetry expresses the universal, and history only the particular."
Ultimately our code is historical and specific, but Git doesn't
impede a graceful retelling.

As mentioned, rebasing is like time travel because if done carelessly
it will create paradox. Specifically you shouldn't rebase code that
other people might have pulled. Git history is exact and discrete
and your rebase will sever common history and lead to painful
reconstruction.

There are some exceptions to this rule, but they're best left to
master Git magicians. For instance, the developers of Git itself
share a branch called "pu" (for Proposed Updates) which they allow
to be rebased at any time. Also, the employees of Github have a
convention that a shared branch prefixed with someone's initials
should be left alone by others, free for that person to rebase. It
serves as a backup to their local repo and as a place to share code
ideas.

Merging can be seductive. For one thing, it makes the Git log look
like a lot of work has occurred. Lots of criss-crossing lines look
like serious progress. You can proudly show a manager the teeming
edifice. Although branches provide an artifact of how and when
people were doing things, Matthew believes in "value over metadata."
He just wants the software to work at the end of the day and feels
that rebasing allows a clear way to trace through the changes. A
straight line tells a better story.

But there's an art to the commits themselves. Matthew suggests
committing **every five to fifteen minutes**, even if your code is
ugly. You can deal with the ugliness later by squashing the mindless
little commits. Fundamentally, squashing frees creativity from
continuous housekeeping. Typically Matthew makes twenty to twenty-five
commits before lunch and then squashes them to three or four.

To code like a time traveler, you'll want to change the way you
pull changes. One thing to remember is that there is no central
repository coordinating a team so nobody can tell for sure who is
changing code at any specific time. Hence one person will surely
"lose" when two people push to a single repo. The loser must
pull before they push, and the default behavior of a pull
followed by a push creates ugly trapezoids in graph history.

You can fix this merge behavior by using `git pull --rebase`. In
effect you are subordinating your changes to ensure a fluid, cohesive
story. You're allowing your commits to appear last in the graph
even if they were created earlier.

Rebased pulling is great until you get halfway through and run into
a conflict. As you resolve changes and stage them you may be tempted
to commit them, which is the incorrect thing to do. What you need
to do is `git rebase --continue` or things will go terribly wrong.
Another note: a confusing safety net will kick in if you choose to
commit your local version of files without modification during the
rebase. Git considers your commit vacuous and refuses to apply it.
You can get out of this situation using `git rebase --skip`. That
forgets the commit and continues the rebase.

To enjoy rebasing by default during pulls on an existing branch,
run `git config branch.branch-name.rebase true`. To set it as default
for all branches created in future, run `git config branch.autosetuprebase
always`. Add a `--global` flag to affect all local repos.

Tell a good story. Reshape your commits. Change the past. Be a poet.
