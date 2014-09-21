---
title: Database migrations without merge conflicts
---

Ever discover an open source project that makes you overjoyed? That
moment when you feel like *somebody gets it*, and things are getting
better? That's how I felt tonight:

<div style="text-align: center;">[&nbsp;<a href="https://github.com/theory/sqitch">theory/sqitch</a>&nbsp;]</div>

This is a database migration system built for the git era. It
supports non-linear patch history and a system of detailed logging,
reversion, and tagging. Oh, and it's built around test driven
database design.

Here's one ingenious property of the system. It has a special format
for the so-called "plan file" it updates as you apply or revert
migrations. People are free to work on multiple branches of a
codebase and change the plan file willy nilly. You might be used
to getting merge conflicts in your database schema, but the plan
file format is designed to merge cleanly and automatically in git
with the [union merge
strategy](http://git-scm.com/docs/gitattributes#_built-in_merge_drivers).
Just put `sqitch.plan merge=union` into your `.gitattributes` file
and relax.

Sqitch is under active development for "better integration with
version control systems...to make managing idempotent reworkings
even easier."

<div style="text-align: center;">[&nbsp;<a href="https://github.com/theory/sqitch/blob/master/lib/sqitchtutorial.pod">try the tutorial</a>&nbsp;]</div>

#### Objections

* "It's written in Perl, eww!" Yeah I don't write perl either, but
  the project is easy to install (even has a homebrew <a
  href="https://github.com/theory/homebrew-sqitch/">tap</a>) and you
  don't have to worry about it. That's like complaining about the
  fuel used in a hyperdimensional warp drive as you ride in your
  horse-drawn buggy.
* "It wants me to write crusty stored procedures?" No, not at all.
  Write the SQL of your choice to match the architecture of your app.
* "Is that an unsalted md5 password in the documentation example?"
  Don't worry, read the whole documentation. Some examples intentionally
  show the wrong ways of doing things in order to correct them later
  and demonstrate concepts.

<script async class="speakerdeck-embed" data-id="ad105ed0ac490130d6a626f5cde8fd08" data-ratio="1.2994923857868" src="//speakerdeck.com/assets/embed.js"></script>
