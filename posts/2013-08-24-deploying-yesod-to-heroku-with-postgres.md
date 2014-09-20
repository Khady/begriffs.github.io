---
title: Deploying Yesod to Heroku with Postgres support
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
<a href="http://4.bp.blogspot.com/-eMJtzUFFBo4/UhgZyKmdzFI/AAAAAAAAADw/6bM8lXNQazQ/s1600/heroku.png" imageanchor="1" style="clear: right; float: right; margin-bottom: 1em; margin-left: 1em;"><img border="0" height="200" src="http://4.bp.blogspot.com/-eMJtzUFFBo4/UhgZyKmdzFI/AAAAAAAAADw/6bM8lXNQazQ/s200/heroku.png" width="200" /></a>I deployed my first Yesod app today! Deploying to Heroku has gotten way smoother since even a few months ago let me tell you. I read articles with crazy gymnastics like booting up a virtual machine that matches Heroku infrastructure to build a binary on a special deploy branch that you copy and... no. Not doing that.<br /><br />I like Yesod so far. It feels like a real framework that can make real pages. It was definitely influenced by Rails but one thing it left behind is the creepy Rails magic. Things are explicitly linked together so it makes sense (aside from some of the Haskell constructions which are still foreign to me). Plus these DSLs are startlingly clean. Cabal files are like <span style="font-family: Courier New, Courier, monospace;">make</span> meets <span style="font-family: Courier New, Courier, monospace;">bundler</span>. Hspec is like rspec without stuttering. But my sycophancy aside, here's how you deploy.<br /><h3><br />Step 1: a basic Yesod app and local databases</h3><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6325936"></code> Now edit <span style="font-family: Courier New, Courier, monospace;">config/postgresql.yml</span> with the username and databases you created, and remove the production database entry. We'll be reading it from a Heroku environment variable.<br /><br /><h3>Step 2: a new Heroku app and production database</h3><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6325987"></code> <h3>Step 3: have the app ask Heroku for db connection info</h3><br />Follow these excellent instructions to <a href="http://pbrisbin.com/posts/parsing_database_url">add the Heroku helper</a> to your app. Two things puzzled me for a while that the article doesn't mention. You need to modify your project .cabal file and add <span style="font-family: Courier New, Courier, monospace;">heroku</span> to the end of the <span style="font-family: Courier New, Courier, monospace;">build-depends</span> and <span style="font-family: Courier New, Courier, monospace;">Helpers.Heroku</span> to the <span style="font-family: Courier New, Courier, monospace;">exposed-modules</span>.<br /><h3><br />Step 4: tell Heroku how to run your app and deploy!</h3><br />Heroku reads a Procfile to determine how to spawn various types of processes like the web server and workers. Yesod projects come with a Procfile you can copy and modify. It's full of scary comments about the bad old days but you can remove all the comments and keep the last line. So do this:<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6326092"></code> This is all a one-time thing. The first deploy takes a really long time as it installs all the dependencies. To deploy in the future just do&nbsp;<span style="font-family: Courier New, Courier, monospace;">git push heroku master</span> and it's fast and works perfectly.
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
    2013-08-27T16:58:43.920Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    I am having a problem getting my example app running on Heroku. Maybe one of you wizards knows the answer:<br /><br />2013-08-27T15:11:01.764268+00:00 heroku[web.1]: Process exited with status 1<br />2013-08-27T15:11:01.779798+00:00 heroku[web.1]: State changed from starting to crashed<br />2013-08-27T15:11:09.413024+00:00 heroku[web.1]: Error R99 (Platform error) -&gt; Failed to launch the dyno within 10 seconds<br />2013-08-27T15:11:09.413024+00:00 heroku[web.1]: Stopping process with SIGKILL<br />2013-08-27T15:21:02.921242+00:00 heroku[web.1]: State changed from crashed to starting<br />2013-08-27T15:21:05.475487+00:00 heroku[web.1]: Starting process with command `./dist/build/assay/assay production -p 22725`<br />2013-08-27T15:21:06.767885+00:00 app[web.1]: assay: SqlError {sqlState = &quot;&quot;, sqlExecStatus = FatalError, sqlErrorMsg = &quot;could not connect to server: Connection refused\n\tIs the server running on host \&quot;localhost\&quot; (127.0.0.1) and accepting\n\tTCP/IP connections on port 5432?\n&quot;, sqlErrorDetail = &quot;&quot;, sqlErrorHint = &quot;&quot;}<br />2013-08-27T15:21:08.262682+00:00 heroku[web.1]: Process exited with status 1<br />2013-08-27T15:21:08.276326+00:00 heroku[web.1]: State changed from starting to crashed<br /><br />It looks like my app isn&#39;t talking to the database but I&#39;m sort of lost as to why. Any pointers?<br />
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
    2013-08-27T22:18:05.609Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Looks like it&#39;s trying to connect to your database with default settings. Did you remove the Production group from config/postgresql.yml and follow the instructions about modifying dbconf in Application.hs?<br /><br />http://pbrisbin.com/posts/parsing_database_url
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
    2013-08-30T03:37:33.983Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Thanks for this! Going to give it a try tonight.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03097657129811885396">
  <div class="css-comment-name js-comment-name">
    Chris
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-09-01T03:02:17.868Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    I must have missed that part. Thanks for pointing that out. This worked great.
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
    2013-09-05T10:32:55.995Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Ugh, having trouble getting the PostgreSQL package to install properly in Windows. I&#39;m going to try MongoDB with <a href="https://addons.heroku.com/mongolab" rel="nofollow">this</a>.
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
    2013-09-06T01:48:15.911Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    <a href="https://github.com/eightyeight/floating-castle-2421" rel="nofollow">Got it working</a>. Huzzah. Ended up going with MongoHQ and I had to modify the code from the Heroku package.
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
    2013-09-06T03:31:34.989Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Wow nice work, very cool!<br /><br />Think it&#39;s possible to modify the Heroku package to parse either postgres or mongodb (perhaps depending on an argument)? If so, consider sending a pull request to https://github.com/gregwebs/haskell-heroku
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
    2013-09-09T04:34:45.069Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    That&#39;s the plan. I figure that for compatibility, importing Heroku should still give you a Postgres config. Importing Heroku.Mongo will look for a mongo config.
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
    2013-09-11T01:39:36.562Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    It&#39;s finally working :P. <a href="https://github.com/eightyeight/floating-castle-2421/blob/e01c571f91ac3860e161ee018fee6850798b2b59/Helpers/Heroku.hs" rel="nofollow">Here&#39;s my Helpers/Heroku.hs</a>. Doesn&#39;t require the Heroku module yet (until I get some pull requests accepted).
  </div>
  <br/>
</div>
</div>
{% endraw %}
