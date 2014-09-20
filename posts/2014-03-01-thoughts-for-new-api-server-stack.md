---
title: Thoughts for a new API server stack
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
If you could choose any technologies to build your next API server, what would they be?<br /><br />I've been thinking a lot about this question because I know we can do better than the status quo. I feel that the current generation of web frameworks are lacking because they make us focus on MVC procedures rather than the data itself.<br /><br />Before I jump into coding my first proof-of-concept, I feel like talking it out in a blog post. If you disagree with my approach let's discuss.<br /><br /><ol><li>Start with <b>PostgreSQL</b>. It is a very capable database, and we can use its built-in features rather than rewriting them. It might not be the right choice for certain applications (like graph connectedness queries), but it is perfect for traditional crud apps. It can handle geospatial and full text searching as well.</li><li>Store database migrations in a git repo and use&nbsp;<a href="https://github.com/depesz/Versioning"><b>depesz/Versioning</b></a> (see his <a href="http://www.depesz.com/2010/08/22/versioning/">blog post</a>). If you've used Rails then you know about migrations, but depesz's are better. Have you ever switched branches in your code and been unable to migrate because some of your migrations are on a different branch? I don't think this library has that problem (but I haven't tried it yet). Unlike the Rails model, this one recognizes that migrations form a dependency tree, not a straight line, and it keeps fuller record in the db of migration history. [<b>UPDATE</b>: <a href="https://github.com/theory/sqitch">theory/sqitch</a>&nbsp;looks even better]</li><li>Use <b>test-driven development</b> in migrations. The good thing about relational databases is there is less to test in the first place. By using constraints you can often just declare what you want. But for more complicated things like subtle row ordering or joins we can include fixtures and tests with each migration. Perhaps wrap the migration in a transaction that ensures the tests fail before migration and succeed afterward, else roll back the whole thing.</li><li>Define <b>views</b> that will be the face of the API. Don't allow public access to any underlying tables. Postgres 9.3 now supports <a href="http://michael.otacoo.com/postgresql-2/postgres-9-3-feature-highlight-auto-updatable-views/">auto-updatable</a> views, so you don't have to create stored procedures to change data. Even views with aggregate functions and joins can be updatable, as long as you write triggers to intercept the update.</li><li><b>Add HTTP caching</b> hints to views as <a href="http://www.postgresql.org/docs/9.3/static/sql-comment.html">comments</a>. The comment can be SQL which produces a single-row table that can contain any of these columns: <span style="font-family: Courier New, Courier, monospace;">Expires</span>, &nbsp;<span style="font-family: Courier New, Courier, monospace;">max_age</span>, &nbsp;<span style="font-family: Courier New, Courier, monospace;">Last-Modified</span>, and <span style="font-family: Courier New, Courier, monospace;">ETag</span>. The API server can run the comment query in a production environment to decide what headers to set.</li><li><b>Version</b> the views with schemas. Backward compatibility is important for an API, so we should address it early. For each major (breaking) version create a schema like v1 or v2. Consumers of the API will have their queries executed within a schema depending on the version they request. Also views can be accessed across schemas with the&nbsp;<span style="font-family: Courier New, Courier, monospace;">search_path</span> setting (to fall back to old versions of whichever views you don't feel like redefining in the new version).</li><li>Use Postgres default <b>security through <a href="http://www.postgresql.org/docs/9.3/static/user-manag.html">roles</a></b>, not custom app logic. Create the roles and decide which views they can access. Signing into the API should be standard HTTPS auth which forwards to the database for login. Your API will not create its own table with usernames and hashed passwords. If you need to store more info about your users (name, email etc) you can create a table with a foreign key to the&nbsp;<span style="font-family: Courier New, Courier, monospace;">pg_authid</span> table.</li><li><b>No controller logic.</b> Wrap your database behind a <a href="http://sandman.io/">Sandman</a> server. The routes and content of every view is then created mechanically from your database views. If you want to change anything you have to do it in the db. This way there is one source of truth and one place to write tests.</li><li>Select the version with proper HTTP <a href="http://blog.begriffs.com/2014/02/api-versioning-best-practices-no-really.html"><b>content negotiation</b></a>. The chosen version will determine the schema in which queries execute.</li><li>Provide <b>server-side pagination</b> through <a href="http://blog.begriffs.com/2014/01/unlocking-deep-http-with-javascript-pt-2.html">headers</a>.</li><li><b>Deploy using Heroku.</b> I'm not a devops expert, but I know Heroku makes things pretty easy. It handles automated database backup, and has easy Postgres scaling settings (depending on your budget). Add the New Relic <a href="https://newrelic.com/plugins/boundless/109">Postgres Plugin</a>, and autoscale your web dynos with <a href="http://hirefire.io/">HireFire</a>.</li></ol><div>This is my current opinion about how to create quality APIs. Please help me think through the issues and poke holes in it. Likely both Sandman and depesz/Versioning need more features to support the ideas in my list.</div>
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/09505637637499810033">
  <div class="css-comment-name js-comment-name">
    Rehno Lindeque
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2014-03-14T05:25:08.213Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    On #11, I almost don&#39;t want to mention this because I wouldn&#39;t want to distract you from the amazing work you&#39;re doing on the haskell buildpack (please don&#39;t stop!)... It might also be worth checking out https://www.docker.io/. There are already some good looking haskell dockers at https://index.docker.io/search?q=haskell. Currently they&#39;re recommending waiting for 1.0 release (http://blog.docker.io/2013/08/getting-to-docker-1-0/) before using in production.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05245796149619242061">
  <div class="css-comment-name js-comment-name">
    Mietek
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2014-04-04T03:10:05.959Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    This comment has been removed by the author.
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05245796149619242061">
  <div class="css-comment-name js-comment-name">
    Miëtek Bak
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2014-04-04T03:12:54.082Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    On #7, how do you propose to authenticate with Postgres, without opening at least one connection per user, if not per request?
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
    2014-04-04T05:20:32.673Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Oh good point. Perhaps use pgBouncer for connection pooling (https://wiki.postgresql.org/wiki/PgBouncer) and then if the system is deployed to Heroku use this buildpack (https://github.com/gregburek/heroku-buildpack-pgbouncer).<br /><br />It would still require a separate connection for each distinct user probably. Although the max connections is configurable on a postgres server (http://www.postgresql.org/docs/9.2/static/runtime-config-connection.html#GUC-MAX-CONNECTIONS) I don&#39;t have any experience about how much memory it consumes or how well it scales. Maybe anonymous users would share a single connection pool which would cut down on the connections.<br /><br />Do you think these measures would help fix the problem or is it doomed?
  </div>
  <br/>
</div>
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/05245796149619242061">
  <div class="css-comment-name js-comment-name">
    Miëtek Bak
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2014-04-04T06:04:49.080Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Connection pooling would still require at least one connection per user.  On Heroku, there is a clear relationship between the connection limit and the monthly price, with 500 connections at the highest tier:<br />https://devcenter.heroku.com/articles/heroku-postgres-plans<br /><br />I don&#39;t see a way to avoid doing authentication outside the DB.  You can, however, use roles for authorization:<br />http://dba.stackexchange.com/a/25400<br /><br />Let&#39;s chat on Freenode.
  </div>
  <br/>
</div>
</div>
{% endraw %}
