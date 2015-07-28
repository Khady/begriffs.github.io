---
title: Haskell Source Navigation
video: true
twitpic: https://i.vimeocdn.com/video/528283298.jpg?mw=700
desc: Command-line tools to navigate and inspect Haskell source code
---

In this video I demonstrate the use of command-line tools to
interactively explore Haskell source code. The tools provide a fluid
development experience but can be tricky to configure. This talk
covers their use, configuration, and integration with an editor.

<video poster="https://i.vimeocdn.com/video/528283298.jpg?mw=700" class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/134634569.hd.mp4?s=bca20c829e3a57bc54fd9f6b90210439" type="video/mp4">
</video>

<a class="embedly-card" href="http://www.slideshare.net/begriffs/haskell-code-tools">Haskell Source Navigation</a>

### Summary

- Types provide more than just safety
- Haskell has enough structure to support sophisticated tools
    - Jump to symbol definitions
    - Find all uses of functions
    - Get (good) linting suggestions
    - Interactive error checking
    - Finding docs by type or name
    - Autocompletion from dependencies
    - Code formatting
- Getting into the tools themselves, below the editor layer
    - hasktags
    - codex
    - hscope
    - hoogle
    - hoobuddy
    - Alfred shortcuts
    - ghc-mod
    - hlint
    - stylish-haskell
    - hindent
- Putting them together into a [vim config](https://github.com/begriffs/haskell-vim-now)

<script async src="//cdn.embedly.com/widgets/platform.js" charset="UTF-8"></script>
