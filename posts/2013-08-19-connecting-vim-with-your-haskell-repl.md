---
title: Connecting Vim with your Haskell repl
---

Only a few days in and typing code right into GHCI is getting old.
I hooked up my Vim configuration so that I can send blocks of code
into the repl from the editor. This is way easier, and you can do
it too by using [begriffs/dotfiles](https://github.com/begriffs/dotfiles).
Installing on a mac is a one-liner, and it backs up your old
configuration so it's easy to give it a try.

Here's a screencast where I show you how I use Vim and GHCI.

<div id="wistia_5lfe805pzx" class="wistia_embed" style="width:640px;height:360px;"> </div>
<script charset="ISO-8859-1" src="//fast.wistia.com/assets/external/E-v1.js"></script>
<script>
wistiaEmbed = Wistia.embed("5lfe805pzx", { videoFoam: true });
</script>

###

You'll want to watch out when sending multi-line strings to the
repl, because it greedily starts trying to execute the first line.
You can instruct it that a multi-line block is a single unit by
enclosing it like so

```haskell
:{
  -- multi line
  -- code here
:}
```

Alternately, run `:set +m` in GHCI and it will require a blank line
after each input before interpreting. Anyone else have cool ways
to edit and run programs?
