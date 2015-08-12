---
title: Dev and Deploy Haskell on Docker
video: true
twitpic: https://i.vimeocdn.com/video/530093762.jpg?mw=700
desc: An efficient workflow without troublesome optimizations
---

[Christopher Biscardi](http://www.christopherbiscardi.com/) shares
his Docker wisdom, showing how to package the whole Haskell dev
environment inside a container and how to optimize it for production
release.

<video poster="https://i.vimeocdn.com/video/530093762.jpg?mw=700" class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/135823847.hd.mp4?s=e7db2f1e096004d428858b69a3515415" type="video/mp4">
</video>

### Summary

- How do we build Haskell code locally?
- Docker is one answer
- In the Dockerfile copy your source code into the image, then cabal update and cabal install
- This works but it is naive
    - Changing any file causes you to rebuild everything (the whole cabal sandbox!)
- So we could try to optimize the Dockerfile
    - Have to determine the individual pieces and where they will end up and how long that should take
- Heavily optimizing Dockerfiles takes work, and it’s more fun to build your application than burn time tweaking the Dockerfile
- A better solution is to use Docker as your development environment
    - Clone your project
    - Docker run an interactive tty
    - Mount the directory
    - At that point you’re in the container and can install deps, build the sandbox, etc
    - Container changes are persisted to a volume so you can kill it and resume work later
- This way you don’t need Haskell installed in the host system at all
- Easy to switch GHC version between projects
- How do we ship the dev work to production?
    - Building a new Docker image containing the binary built in dev results in shared library errors
    - Solution: build a static binary
    - Works, and plays well with Circle CI etc
- The production image unfortunately can be rather big, like 150mb
    - Solution: compile GHC with MUSL
    - …or if you want to cheat, just use the [nilcons/ghc-musl](https://github.com/nilcons/ghc-musl) image
    - Chris’ production image (based on Alpine) shrank down to 36mb
