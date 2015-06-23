---
title: Stack, the Haskell Build Tool
video: true
---

[Dan Burton](https://unknownparallel.wordpress.com/) gave this talk
at [Bayhac 2015](http://bayhac.org/).  In it he introduces Stack, a
candidate replacement for Cabal.  The tool provides an easy one
line command to install Haskell packages. It also installs any
missing tools onto the system (GHC, Cabal, and libraries like alex,
happy and cpphs). By default it uses the curated Stackage long-term
support databases to choose packages known to build and coexist
together.  Finally it reuses previously installed packages whenever
possible to avoid unnecessary recompilation.

<div class="flowplayer" data-embed="false">
  <video type="video/mp4"
         src="https://player.vimeo.com/external/131463587.hd.mp4?s=027fe40cde79f1e2b7ce47e6eed66a06"
         poster="https://i.vimeocdn.com/video/523723567.jpg?mw=700"
  ></video>
</div>

### Summary

- [Stack](https://github.com/commercialhaskell/stack) is a build tool for developers, currently in beta
    - The spiritual successor to stackage-cli
    - It can install packages and also prereqs like GHC itself
- Stack builds on top of [LTS Haskell](https://www.stackage.org/lts) which is a curated set of packages from package
    - alleviates dependency hell
    - upgrading minor version of LTS implies upgrading only up to minor versions of each package
    - To get your package added to stackage, follow these [instructions](http://www.stackage.org/authors)
- Each project needs a stack.yaml

```yaml
   # stack.yaml
   resolver: lts-2.15
```

- LTS resolver versions imply a version of GHC
    - the 2.x series implies GHC 7.8
    - the upcoming 3.x series implies GHC 7.10
- Running `stack build` examines your cabal file and generates a stack.yaml that fits
- Stack can share dependencies between projects
    - it improves the cabal sandboxing state of the art
    - avoids reinstalling things for every project
- You can supplement LTS snapshots
    - with newer package versions on package
    - patched version on the disk
    - or straight from a github repo
- Stackage projects can contain multiple packages
    - Example of how Yesod uses multiple packages
- Discussion of custom scripts in builds
- Stack reimplements the features of cabal-install, and uses the cabal library underneath
- Stack prefers to use a GHC found on your path but will suggest `stack setup` if the version does not match the resolver
    - it will also install other tools as needed like alex, happy and cpphs
- Q&A
