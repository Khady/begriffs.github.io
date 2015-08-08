---
title: Nix ±Cabal
video: true
twitpic: https://i.vimeocdn.com/video/529705750.jpg?mw=700
desc: A newcomer's perspective on Nix
---

[James Earl Douglas](http://earldouglas.com/), Technical Lead at
the Wikimedia Foundation, shares his recent experience using Nix
to manage system state. He discusses installing it in Ubuntu, using
Nix to install other packages, and finally how it interacts with
GHC and Cabal.

<video poster="https://i.vimeocdn.com/video/529705750.jpg?mw=700" class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/135615305.hd.mp4?s=bcc3febf8794750db8afb64f97991846" type="video/mp4">
</video>

See the slides [here](http://earldouglas.com/presentations/nix-maybe-cabal/slides.html#(1)).

### Summary

- A newcomer’s perspective on nix
- Cabal can be difficult to use and nix helps alleviate that pain
- Nix is purely functional
    - Packages never change
    - Helps resolve version conflicts of tools (like ghc) and libraries (like base)
- Each package is stored in a unique, read-only directory
- Packages have a hash that forever identify both them and their dependencies
- Within NixOS itself
    - Everything lives in a nonstandard place, even bash
    - No more “hash-bang-bin-bash” — use the `env` command
- Getting started on Ubuntu
    - Install by curling and running a shell script
    - Installing ghc and cabal from scratch with the nix-env command
    - Making specific packages visible with `nix-shell`
- Examples of Nix configuration and expressions
