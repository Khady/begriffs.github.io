---
title: Put Quality on Autopilot
---

### Summary

In this article I will identify when and how to use automated
code-quality tools (especially for web development). I will survey
and classify existing tools, and share new tools I am developing.

### Introduction

We bred dogs to bark. Their ancestors, wolves, don't relentlessly
yip like the poodle next door. If you find yapping dogs annoying
let me remind you that we made them that way. We did it for a reason,
and I suggest we make our computers noisy too.

Much like a dog, a computer isn't very busy most of the time. The
interval between our keypresses stretches into clock cycle eternity.
Also, both canines and computers have senses that exceed our own.
Dogs have keen senses of smell and hearing, and computers have
perfect attention. Dogs use their senses to protect and alert us,
and I propose we do a better job training our software to similarly
bark at any hint of danger.

### Necessary Conditions

If you introduce the wrong code checking tool in a project it will
quickly fall out of use and potentially delay the project. Below
is a list of what I believe are the necessary conditions for any
code-quality tool to succeed.

Use an automated code-quality tool only if:

* **A1**) It can't possibly create a bug
* **A2**) It isn't onerously slow
* **A3**) It requires no repeated intervention to function
* **A4**) Its use can be enforced across your whole team
* **A5**) Its results are concise and understandable

The reason we turn to computers to check our work is because they
are methodical and merciless. If we want to use tools effectively,
we need to ensure our they fulfill **A3**. If a tool requires our
intervention then we are back to our original problem of human
negligence.

Having **A3** without **A1** would be an unpredictable nightmare
where code breaks unnoticed. A manually run quality tool could
conceivably be useful without **A1**, but only under close supervision.

A tool with **A3** but without **A4** would create friction in a
team and ulimately be abandoned. Some team members (new ones,
subcontractors, consultants) would drift into ignoring the quality
warnings and leave other members to clean up. Any tool your team
adopts must be difficult to circumvent. For instance your team can
block Git commits of failing code via a pre-commit hook.

The remaining requirements, **A2** and **A5** are more subjective,
but vitally important. I have worked on codebases with test suites
that took more than twenty minutes to run. In those cases
[BDD](http://en.wikipedia.org/wiki/Behavior-driven_development)
more accurately stands for Break-Driven Development.

If a tool improves code but violates **A5** by providing confusing
output or suggestions then its use is questionable. Somewhat selfishly
I might discard it because it makes my job hateful, especially if
it's in my face day after day due to property **A4**. If, on the
other hand, a transformation tool violates **A5** by producing
correct but awkward code then it should be rejected without question.
Just be sure that you don't mistake habitual, unexamined stylistic
allegiance for a substantial objection. More on this below.

### Sufficient Conditions and Popular Resistance

When, then, should an automated code quality tool be used? Are
**A1**-**A5** sufficient (as well as necessary) conditions? If code
quality tools are seldom used, is it simply because they violate
**A1**-**A5**?

Speaking for myself, I have failed to use code quality tools because of:

* **B1**) ignorance
* **B2**) incompatible requirements
* **B3**) stylistic allegiance
* **B4**) unimportant programs

Ignorance needs no explanation. Luckily there is plenty to learn
about code-quality tools, and delightful opportunities abound. My
first exposure to automated sanity checking was doing C programming.
I soon discovered compiler warning level options and saved myself
lots of time. Even now I'm discovering more warning options for the
`gcc` compiler (check
[this](http://stackoverflow.com/questions/5088460/flags-to-enable-thorough-and-verbose-g-warnings/9862800#9862800)
out and be amazed).

Incompatible requirements are a bigger problem. When your program
uses a sloppy library that raises a slew of warnings, then reading
the output will be annoying and will obscure any warnings about
your own code.

Choosing to disable warnings rather than fix libraries causes what
social scientists James Wilson and George Kelling call "the broken
window" effect. They ask us to "consider a building with a few
broken windows. If the windows are not repaired, the tendency is
for vandals to break a few more windows." Using sloppy dependencies,
even if they are bug-free, is actually hazardous to your code insofar
as they prevent you from comfortably enabling automated warnings.

Anyone who has programmed for a while has developed their own
syntactical style. However these styles have no effect on program
execution in most languages; they are subjective. When working with
a team it is best to choose a convention and compromise your style.
Personal style allegiance can interfere with tools like linters and
reformatters, so if you are already compromising your style to
harmonize with a team, why not match the team style with that
expected by your quality tools? Doing so in fact effectively enforces
the team style.

In his JavaScript lectures, Douglas Crockford maintains that
"[programming] style should not be about personal preferences and
self-expression," rather, "the most important thing about a program
is for people to understand it." A style that is least surprising
reduces errors, and syntactical forms that hide defects are themselves
defective.

The last reason why I have personally neglected to use program
quality checkers is that I (rightly or wrongly) believed my programs
weren't important. When writing a little script or toy project the
codebase starts small. Often I write a program to test its fundamental
ideas, it felt irritating to simultaneously audit the code quality.

Unfortunately, the habits (_ethos_) we practice with our unimportant
projects eventually shift our programming character (_ethikos_).
So get comfortable incorporating quality checks into your unimportant
projects and you will be prepared for important ones.

In summary, every reason other than incompatible requirements **B2**
is within my power to change. I conclude that properties **A1**-**A5**
are sufficient reason to use an automated testing tool in the absence
of objection **B2**.

### Types of Tools

Code quality tools can be broadly classified as linters, fuzz
testers, vulnerability scanners, and transformers. Linters statically
analyze source code to find suspicious patterns such as unused
variables, unreachable control flow, or side-effect trickery. Fuzz
testers generate unusual inputs to test programs outside of the
bias existing in human developers' minds. Vulnerability scanners
work in some cases like linters to look for insecure patterns in
source code, and other times probe a live server with web requests.
Transformers rewrite source code, in some cases standardizing its
style, in others simplifying its logic.

Below is a survey of code quality tools that are relevant to a Ruby
on Rails development stack. Please let me know about others I have
missed.

#### linters

* [clutchski / coffeelint](https://github.com/clutchski/coffeelint)
* [douglascrockford / JSLint](https://github.com/douglascrockford/JSLint)
* [stubbornella / csslint](https://github.com/stubbornella/csslint)
* [codegram / pelusa](https://github.com/codegram/pelusa)
* [seattlerb / heckle](https://github.com/seattlerb/heckle)
* [bendyworks / lock_block](https://github.com/bendyworks/lock_block)

#### fuzz testers

* [relevance / tarantula](https://github.com/relevance/tarantula)
* [IKEGAMIDaisuke / rushcheck](https://github.com/IKEGAMIDaisuke/rushcheck)

#### vulnerability scanners

* [presidentbeef / brakeman](https://github.com/presidentbeef/brakeman)
* [Arachni / arachni](https://github.com/Arachni/arachni)
* [Netflix / SimianArmy](https://github.com/Netflix/SimianArmy)

#### transformers

* [einars / js-beautify](https://github.com/einars/js-beautify)
* [sass-convert](https://github.com/nex3/sass)
* [begriffs / css-ratiocinator](https://github.com/begriffs/css-ratiocinator) (in progress)

### My Contribution

I and some other [Bendyworkers](http://bendyworks.com/workers) are
experimenting with creating our own code-quality tools. The first
is a Ruby gem called *lock_block*. It's a command line (and
vim-enabled) tool to help you get a handle on your changing code.
You select a couple of lines of code and tell Lock Block to tag
those lines. It wraps the block with annotating comments containing
a hash of the code state. If the code changes then you can find
out.

Lock_block is useful for associating comments with code and reminding
people to keep the comments up to date. It's also useful in a legacy
codebase to flag fragile and dangerous sections. This gives newcomers
an explicit warning, especially when combined with a Git pre-commit
hook to stop people from committing changes before they acknowledge
them.

The second tool we're working on is called the _CSS Ratiocinator_
(after Leibniz's Calculus Ratiocinator). It examines the live DOM
in the browser and reverse engineers a new, more elegant, CSS
definition that captures styles down to the pixel.

It addresses the problem of old CSS whose styles accumulate and
contradict each other. After a certain point all CSS seems to grow
only by internal antagonism. The ratiocinator wipes the slate clean
and provides a harmonious new beginning. It is best used with
sass-convert to further improve the result.

Ratiocinator is under active development and has plenty of challenges
left before it is ready for production. Pull requests are welcome.
If you want to know how to help, examine the issues logged in GitHub.

I'll close with a word of warning: don't turn off your mind. Automated
code tools can lull you into a false sense of security. If your
vulnerability scanner gives you the green light you might conclude
your code is secure...don't. However, if you stay vigilant you can
expect nothing but help from code-quality tools.
