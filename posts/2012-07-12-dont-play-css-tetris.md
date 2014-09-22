---
title: Don't Play CSS Tetris
---

"Want to see the future?" Dale Sande
([@anotheruiguy](https://twitter.com/anotheruiguy/)) leaned toward
me with a conspiratorial gleam in his eye. His laptop was crowded
with code and design layouts. He proceeded to show me that I have
been doing CSS all wrong.

My first mistake is using CSS at all. "If you're like most developers
you start designing a web page from the top-left down. This ends
up painting you into a corner," Dale explains. Building style by
stacking DOM elements together is like playing Tetris -- you will
always lose in the end.

He believes in a modular inside-out approach to design, one where
developers start by identifying and isolating repeated widgets of
design, like navigation links, sections, and forms. CSS was not
designed for this approach, and Dale believes that SCSS provides
the necessary features to encourage proper modular reuse.

In the object oriented SCSS (OOSCSS) approach, repeating widgets
are developed outside of the actual composition -- or "comp" as it
is affectionately known -- in a style guide. The guide shows each
widget in all its guises, not just how they appear on the "happy
path." For instance, the guide shows how a login form looks after
a failed login attempt.

More ambitious widgets are composed from simpler widgets, and are
displayed in the style guide as well. The chain of composition
extends all the way up, until a comp is a simple matter of arranging
big widgets. Dale believes there is no such thing as a one-off view,
that is a page with small ad hoc differences not shared by sibling
pages. One-offs are only situations which have not been properly
forseen in the style guide. They should be unified.

For a style guide to be effective it must reference exactly the
same styles as are live on a site. However, Dale uses sophisticated
preprocessing tools to sculpt the final CSS which is served to the
browser. This preprocessing can introduce unwanted dependencies to
a simple site, which is why he advocates serving styles from a
separate software stack, potentially from a different server than
that which handles the site content. An elegant design is to mount
your style server on a subdomain of your site, such as
style.yourdomain.com.

Dale is developing a [Compass](http://compass-style.org/) extension
which handles tricky layout mathematics and which provides a deft
OOSCSS grid. He derides popular CSS frameworks: "The moment you
pull in Twitter Bootstrap, you've applied over four thousand lines
of styling -- all before adding any customizations of your own.
Most of these will be irrelevent to your markup." He is developing
an alternative framework,
[Toadstool](https://github.com/Anotheruiguy/toadstool), which
includes only what you need when you need it. The framework will
leverage SCSS's upcoming silent class feature, which only serves
those classes which are @extend'ed.

To learn about modern web styling I will refactor my
[1up](http://1up.begriffs.com/) site to use a styleguide and serve
styles from a separate Toadstool-powered Heroku server.

For more information about OOSCSS and Toadstool, see [this
presentation](https://speakerdeck.com/u/anotheruiguy/p/module-design-ui-dev-patterns).
