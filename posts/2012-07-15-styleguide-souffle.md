---
title: Styleguide soufflé
---

Adam Braus ([@ajbraus](https://twitter.com/ajbraus)) designs software
with a clear vision of what he wants to accomplish, and especially
what real value he will create for users. He was immediately
interested in the ideas presented
[here](2012-07-12-dont-play-css-tetris.html) but still yearned for
something a little simpler, a little lighter...a styleguide soufflé.

Recently he has been immersed in Rails development with Mike Fenchel
([@mfenchel](https://twitter.com/mfenchel)) creating a social
scheduling web app. They are beginning to define its CSS styles and
want to try doing inside-out design with a styleguide.

Adam and I searched for a simple Rails-specific styleguide template,
but didn't find any. Wouldn't it be natural and nice to have a Rails
generator for this?

```bash
rails generate styleguide
```

We certainly thought so, and we made a gem called `styleguide_rails`.
It is now [released on
RubyGems](https://rubygems.org/gems/styleguide_rails) and provides
a generator to scaffold a controller, a view, and routes for your
guide. To add a style module, create a partial in `app/views/styleguide/`
and visit `http://your_app/styleguide`. You'll see the rendered
element and its raw HTML next to one another. It's a convenient way
to document your preferred HTML snippet to create various modules
like login forms.

The basic functionality is there, but there are some obvious ways
to improve the gem. Among other things, it could use these features:

* syntax highlighting for the HTML snippets
* hiding the /styleguide route in production
* handsome style for the guide itself
* ERB evaluation to include one module in another

I'll work on addressing these issues, but feel free to fork the
code [on GitHub](https://github.com/begriffs/styleguide_rails) and
send a pull request.
