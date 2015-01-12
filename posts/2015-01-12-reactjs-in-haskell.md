---
title: Writing a React JS front-end in Haskell
video: true
---

[Joel Burget](http://joelburget.com/), Developer at [Khan
Academy](https://www.khanacademy.org/) explains the design of his
new [react-haskell](https://hackage.haskell.org/package/react-haskell)
library.

It allows you to write a front-end app in Haskell which you compile
to JavaScript via Haste and render using React JS.

<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/116516127.hd.mp4?s=69cd6b66fe5bff69a43e1b4375482edc"
         poster="https://i.vimeocdn.com/video/502970736.png?mw=700"
  ></video>
</div>

### Overview

* Overview of React itself (without the Haskell)
    * The virtual dom
    * Partial diffing
* Haste: turning Haskell into JavaScript
* Inspiration for React-Haskell
    * Blaze-HTML builder provided the general flavor of the API
    * Recently Joel has been switching to the Lucid library instead of Blaze
    * Building the eventual DOM happens inside a monad
    * It could be considered an abuse of a monad, but provides nice do-notation
* Examples of react html written in Haskell
* Event handlers
* Principles of interruptible animations (inspired by UIKit)
    * the model is discrete
    * we apply updates immediately
    * use additive animation by default
* Performance observations
