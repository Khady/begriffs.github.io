---
title: Haskell on Heroku, let's simplify
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
Since Haskell is my new favorite language I'd like to deploy it on my favorite hosting platform -- Heroku. Luckily some kind folks have created a Heroku buildpack to make this as easy as doing a git push.<br /><br />I happily began using <a href="https://github.com/puffnfresh/haskell-buildpack-demo">puffnfresh/heroku-buildpack-haskell</a> to deploy my app, but noticed that it is referencing some slightly outdated versions of GHC and Cabal. So I forked the code and got my pull request ready and then noticed something odd...<br /><br /><b>There are twenty forks of this project and they all differ!</b> People are not sending pull requests, and there is no clear project maintainer. Come on, guys, let's fix this.<br /><br />Question 1: who wants to maintain this project? The original author is <a href="https://github.com/luciferous">luciferous</a>, but his repo hasn't seen a commit in a year, and has fewer stars than some of its forks. Let's get ownership of the root repo transferred to whoever is motivated to maintain it.<br /><br />Question 2: what is lacking about this buildpack and are its forks trying to solve the same or different problems? To answer this I reviewed everyone's commits. Everything mostly agreed up through December 2012. Puffnfresh and mwotton made tons of commits to improve logging, cleanup, and caching. After that point it diverges. Here is what everyone has been doing.<br /><br /><ul><li>mwotton</li><ul><li>multi-threaded compilation</li><li>remove a "criterion" dependency</li></ul><li>RevCBH</li><ul><li>clear cabal cache during compilation</li><li>install node.js and coffeescript for some reason</li><li>many variations on cabal options</li></ul><li>tcrayford</li><ul><li>adjust cabal caching and cleaning</li><li>copy buildpack files from&nbsp;brianmckenna.org onto s3</li></ul><li>samstokes</li><ul><li>include yesod, postgres, and other goodies</li></ul><li>eightyeight</li><ul><li>include yesod</li><li>upgrade cabal and ghc</li><li>adjust caching</li></ul><li>dmjio</li><ul><li>include snap</li></ul><li>ameingast</li><ul><li>strip symbols from installed binaries to save space</li><li>gitignore swp files</li></ul><li>matt2224</li><ul><li>make cabal install more verbose</li></ul><li>egonSchiele</li><ul><li>copy buildpack files from brianmckenna.org into git</li></ul><li>BrianMMcClain</li><ul><li>adjust buildpack to work on Cloud Foundry</li><li>http://catdevrandom.me/blog/2013/05/16/buildpacks-in-cloud-foundry-v2/</li></ul><li>EdgarGames</li><ul><li>include postgres snaplet</li></ul><li>Tener</li><ul><li>include cabal-dev</li><li>lots of other fixes, perhaps relating to cabal-dev</li></ul><li>benhirsch24</li><ul><li>experimenting with parallel cabal install and happy</li><li>trying force-reinstalls</li><li>include language-c</li></ul><li>jogrms</li><ul><li>upgrade cabal and ghc</li><li>went back and forth on caching and force-reinstall</li></ul><li>agocorona</li><ul><li>include monadloc-pp</li></ul><li>nmk</li><ul><li>copy buildpack files to&nbsp;grozdova.com</li><li>copy them again to dropbox and host them there</li></ul></ul><div><br />OK there are patterns here.</div><div><br /></div><div>First people are wanting to include extra software like postgres and web frameworks. My personal opinion is to not include this kind of thing in the basic buildpack. You can actually combine more than one buildpack in your deployment using&nbsp;<a href="https://github.com/ddollar/heroku-buildpack-multi">ddollar/heroku-buildpack-multi</a> and I would advise keeping separate things separate.</div><div><br /></div><div>Second is where to host the big GHC and Cabal binaries. Many people are hosting them in all kinds of places, from personal domains to s3 to dropbox. What is the best practice here? I wonder if Heroku itself could host them?</div><div><br /></div><div>Third is caching between deployments. This one has got to be tricky because more than one person has gone back and forth, enabling and disabling it. So for those of you who know, what is the final verdict? Is it a good idea to do caching, and what are the right settings? In this category I include the <span style="font-family: Courier New, Courier, monospace;">force-reinstall</span> option that people go back and forth on too.</div><div><br /></div><div>Fourth is cleanup and minification. This category includes deleting parts of cabal, like docs. It includes stripping symbols from binaries. There are just all kinds of commits trying to shave space off the final result. What makes this a big deal? Are there disk limits on Heroku?</div><div><br /></div><div>Finally some of these dependencies seem to have a multi-threaded build mode, and it seems to cause problems sometimes. Should it be enabled?</div><div><br /></div><div>I'll email everyone involved and we can discuss and make the Haskell deployment process everything we want it to be.</div><br />
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03097657129811885396">
  <div class="css-comment-name js-comment-name">
    Chris
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-22T14:31:32.341Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Wow. This is really nice work, Joe.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05197498379165188603">
  <div class="css-comment-name js-comment-name">
    Daniel
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-23T05:46:51.736Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    I&#39;ll quote first from my email:<br /><br />&quot;Hey Joe. Thanks for getting in touch - glad someone&#39;s rallying the troops on this. I doubt I&#39;ll be much help to you, though. I was trying to get Yesod working on Heroku, so I forked the buildpack after realising that it didn&#39;t work out of the box. I fiddled around with it a bit and realised that I had absolutely no idea what I was doing, so I put that project on the shelf. I&#39;d be interested in digging a bit deeper and trying to get it working again, though I&#39;m pretty new to Haskell (and anything to do with Cabal terrifies me :P... I can use it to install stuff, as long as it ALL WORKS PERFECTLY the first time).&quot;<br /><br />Since then, I dived back in and got a basic warp server going with Brian McKenna&#39;s buildpack. Trying to run his Yesod demo resulted in a bunch of errors relating to http-conduit. Seemed like the warp and yesod libraries were fighting over the versions.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/13383730017141090311">
  <div class="css-comment-name js-comment-name">
    Ivan Babushkin
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-23T19:58:49.702Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Nice work, Joe!<br /><br />From my email:<br />&quot;Actually, the only useful thing I&#39;ve done on heroku-buildpack-haskell is upgrading to ghc-7.6.2. I built it in heroku environment and uploaded to http://dl.dropboxusercontent.com/u/52714911/ghc.tar.gz as you can see in the sources. The buildpack worked fine for a small project but I gave up making it work on bigger projects with heavy dependencies. Heroku allows only 15 minutes for compilation, so a huge project should be either precompiled or compiled chunk by chunk via sequence of pushes. So in the end I decided to use a vagrant setup to precompile my project.&quot;<br /><br />There are some posts on how to precompile haskell binaries using vagrant. For example this https://github.com/JanAhrens/blog/blob/master/2012-07-05_yesod-deployment.markdown<br /><br />I basically installed a lucid64 VM, then ssh&#39;d into it and installed cabal and ghc, then compiled my haskell project and pushed the binary and shared library dependencies to heroku.<br /><br />The &quot;chunk by chunk&quot; compilation is just following: on the first push compilation fails to complete, but some dependencies are compiled and stored in the cache, then the next push continues prom the point it failed, and so on, until whole project is compiled. So you need the cache to be enabled to use this hack.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05440774752453573594">
  <div class="css-comment-name js-comment-name">
    Joe Nelson
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-29T23:11:16.199Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    So I tried the buildpack, and it worked fine. It took a long time to compile things on the first deploy, but it went quickly the second time. It seems like puffnfresh/heroku-buildpack-haskell is the most up-to-date and complete fork out there. I&#39;d advise sending a pull request to puffnfresh if you have outstanding changes, just to get all the newest best stuff in one place.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05197498379165188603">
  <div class="css-comment-name js-comment-name">
    Daniel
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-30T00:34:49.841Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    What did you deploy there? Just the basic warp demo app? I reckon the next step from here is to try to deploy something from Happstack, Yesod and Snap, since they seem to be the big three of Haskell web apps.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05440774752453573594">
  <div class="css-comment-name js-comment-name">
    Joe Nelson
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-30T00:46:28.816Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Oops, forgot to mention that part. I deployed Yesod, using steps I documented here:<br /><br />http://blog.begriffs.com/2013/08/deploying-yesod-to-heroku-with-postgres.html
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/06136840792496025317">
  <div class="css-comment-name js-comment-name">
    Justin
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-09-11T04:08:21.129Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    John I wish you the best of luck and godspeed.  I was about to fork my own before seeing if anyone has had any luck with uniting all of these.  I believe that getting this all sorted out is integral to the future of Haskell.  Would ruby have been half as successful as it is now if it weren&#39;t for heroku?
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05197498379165188603">
  <div class="css-comment-name js-comment-name">
    Daniel
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2014-04-03T07:32:54.329Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    I just came back to web stuff with Haskell, deploying some simple Scotty/WebSockets stuff with buildpack-heroku-ghc. The process was butter-smooth and simple. Thanks for your work on this!
  </div>
  <br/>
</div>
</div>
{% endraw %}
