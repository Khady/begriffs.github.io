---
title: Learning Real Haskell Incrementally
video: true
twitpic: https://i.vimeocdn.com/video/541079128.jpg?mw=700
desc: An incremental approach to learn Haskell through building things
---

The audience laughs appreciatively as [Mark Fine](https://mfine.github.io/)
talks about the standard books used by new Haskellers: LYAH, Real
World Haskell, etc.  Oftentimes these books contain code that no
longer compiles, or reference obsolete libraries. Mark suggests
another way to learn: pick small but *real* projects, and iterate
on them.

In this video he shares tricks to keep the compiler happy and to
get yourself unstuck while developing Haskell projects.

<video poster="https://i.vimeocdn.com/video/541079128.jpg?mw=700" class="video-js vjs-default-skin" controls preload="auto">
  <source src="https://player.vimeo.com/external/143409955.hd.mp4?s=803b248bd3d7f433853264e255d7b74f" type="video/mp4">
</video>

### Summary

* It’s difficult getting your first real thing done in Haskell
* Some background about Mark’s involvement at Swift Navigation
* Things you have probably already tried
    * The big stack o’ Haskell books only gets you so far
    * Github code search for examples
    * Stack Overflow
    * Hoogle
* Approaches to working and learning incrementally
    * Coding “wishfully” is a powerful way to learn Haskell
    * Growing programs forward from inputs as well as backwards from outputs
    * Make minimum viable changes to keep compiler happy
    * Compile early often and always!
    * One great tool is punting on a function definition with `undefined`
* Real example of working iteratively
    * [mfine/nfl-divisions-power-rankings](https://github.com/mfine/nfl-divisions-power-rankings)
    * It’s a project in twelve steps
    * Tries to answer which NFL teams are best
    * Notice this project is a problem that Mark enjoys, hence easier to stick with
