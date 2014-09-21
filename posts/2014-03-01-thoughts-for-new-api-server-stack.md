---
title: Thoughts for a new API server stack
---

If you could choose any technologies to build your next API server,
what would they be?

I've been thinking a lot about this question because I know we can
do better than the status quo. I feel that the current generation
of web frameworks are lacking because they make us focus on MVC
procedures rather than the data itself.

Before I jump into coding my first proof-of-concept, I feel like
talking it out in a blog post. If you disagree with my approach
let's discuss.

1. Start with **PostgreSQL**. It is a very capable database, and
   we can use its built-in features rather than rewriting them. It
   might not be the right choice for certain applications (like graph
   connectedness queries), but it is perfect for traditional crud apps.
   It can handle geospatial and full text searching as well.
2. Store database migrations in a git repo and use
   [**depesz/Versioning**](https://github.com/depesz/Versioning) (see
   his [blog post](http://www.depesz.com/2010/08/22/versioning/)). If
   you've used Rails then you know about migrations, but depesz's are
   better. Have you ever switched branches in your code and been unable
   to migrate because some of your migrations are on a different branch?
   I don't think this library has that problem (but I haven't tried
   it yet). Unlike the Rails model, this one recognizes that migrations
   form a dependency tree, not a straight line, and it keeps fuller
   record in the db of migration history. (**UPDATE**:
   [theory/sqitch](https://github.com/theory/sqitch) looks even better)
3. Use **test-driven development** in migrations. The good thing
   about relational databases is there is less to test in the first
   place. By using constraints you can often just declare what you
   want. But for more complicated things like subtle row ordering or
   joins we can include fixtures and tests with each migration. Perhaps
   wrap the migration in a transaction that ensures the tests fail
   before migration and succeed afterward, else roll back the whole
   thing.
4. Define **views** that will be the face of the API. Don't allow
   public access to any underlying tables. Postgres 9.3 now supports
   [auto-updatable](http://michael.otacoo.com/postgresql-2/postgres-9-3-feature-highlight-auto-updatable-views/)
   views, so you don't have to create stored procedures to change data.
   Even views with aggregate functions and joins can be updatable, as
   long as you write triggers to intercept the update.
5. **Add HTTP caching** hints to views as
   [comments](http://www.postgresql.org/docs/9.3/static/sql-comment.html). The
   comment can be SQL which produces a single-row table that can contain
   any of these columns: `Expires`, `max_age`, `Last-Modified`, and
   `ETag`.  The API server can run the comment query in a production
   environment to decide what headers to set.
6.  **Version** the views with schemas. Backward compatibility is
   important for an API, so we should address it early. For each
   major (breaking) version create a schema like v1 or v2. Consumers
   of the API will have their queries executed within a schema
   depending on the version they request. Also views can be accessed
   across schemas with the `search_path` setting (to fall back to
   old versions of whichever views you don't feel like redefining
   in the new version).
7. Use Postgres default **security through
   [roles](http://www.postgresql.org/docs/9.3/static/user-manag.html)**,
   not custom app logic. Create the roles and decide which views
   they can access. Signing into the API should be standard HTTPS
   auth which forwards to the database for login. Your API will not
   create its own table with usernames and hashed passwords. If you
   need to store more info about your users (name, email etc) you
   can create a table with a foreign key to the `pg_authid` table.
8. **No controller logic.** Wrap your database behind a
   [Sandman](http://sandman.io/) server. The routes and content of
   every view is then created mechanically from your database views.
   If you want to change anything you have to do it in the db. This
   way there is one source of truth and one place to write tests.
9. Select the version with proper HTTP [**content
   negotiation**](http://blog.begriffs.com/2014/02/api-versioning-best-practices-no-really.html).
   The chosen version will determine the schema in which queries
   execute.
10. Provide **server-side pagination** through
   [headers](http://blog.begriffs.com/2014/01/unlocking-deep-http-with-javascript-pt-2.html).
11. **Deploy using Heroku.** I'm not a devops expert, but I know
   Heroku makes things pretty easy. It handles automated database
   backup, and has easy Postgres scaling settings (depending on
   your budget). Add the New Relic [Postgres
   Plugin](https://newrelic.com/plugins/boundless/109), and autoscale
   your web dynos with [HireFire](http://hirefire.io/).

This is my current opinion about how to create quality APIs. Please
help me think through the issues and poke holes in it. Likely both
Sandman and depesz/Versioning need more features to support the
ideas in my list.
