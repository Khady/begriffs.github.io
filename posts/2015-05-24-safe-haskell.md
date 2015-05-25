---
title: Safe Haskell
video: true
---

Kristen Kozak explains how the Safe Haskell language extension can
be used to deal with untrusted code. She shows how certain Haskell
language features create security loopholes and why Safe Haskell
disallows them. The safe subset is not enabled by default but you
can (and should) enable it with `{-# LANGUAGE Safe #-}` whenever
possible.

<div class="flowplayer" data-embed="false">
  <video type="video/mp4"
         src="http://player.vimeo.com/external/128024210.hd.mp4?s=edb4c23d14c4f532c26c37da58e0f446"
         poster="https://i.vimeocdn.com/video/519026893.jpg?mw=700"
  ></video>
</div>

### Summary

* Safe Haskell has been around since ghc 7.2
* It supports custom security policies for running untrusted code and was designed to be unobtrusive
* Great for environments like tryhaskell.org that accepts wild user input
* Another source of safety is using [labeled IO](https://hackage.haskell.org/package/lio) which binds labels to data for tracking provenance and restricting disclosure
* Safe Haskell also helps developers audit their libraries because GHC enforces levels of safety (more on this later)
* Using Safe Haskell improves code style because it forces you to separate the safe and unsafe parts of your modules
* What is “safety” then?
    * First that the types can always be trusted. For instance that pure functions cannot perform IO. UnsafePerformIO is prohibited to them.
    * Next that the encapsulation of modules is enforced. No template haskell tricks, for instance, which could get access to private parts of other modules
* Isn’t this how Haskell already works? Not exactly, there are loopholes which safe haskell closes
    * SH is a language subset of Haskell
    * Adds rules around module imports
    * Defines a trust systems for entire packages
* SH trust is defined on entire modules, and each module can be one of
    * XSafe (written in the safe language, with safe or trusted imports)
    * XTrustworthy (marked as such by user)
    * XUnsafe (the usual)
* Properties of the safe language itself
    * Type safety
    * Referential transparency
    * Module encapsulation
    * Modular reasoning
    * Semantic consistency (code doesn’t change meaning just because safety is enabled)
* The safe language disallows some language extensions and restricts others
* Limitations
    * Does not address resources limits or compilation safety
    * Use something like a virtual machine for that
    * Relies on library authors to do the separation in their modules into safe and unsafe parts
    * For instance the vector package has not yet been separated
* To use safe haskell effectively you’ll need to enable the GHC flag `-fpackage-trust` otherwise everything will be marked as trustworthy
* Q&A
