---
title: Deploying Yesod to Heroku with Postgres support
---

<img src="/images/heroku.png" class="right" />
I deployed my first Yesod app today! Deploying to Heroku has gotten
way smoother since even a few months ago let me tell you. I read
articles with crazy gymnastics like booting up a virtual machine
that matches Heroku infrastructure to build a binary on a special
deploy branch that you copy and... no. Not doing that.

I like Yesod so far. It feels like a real framework that can make
real pages. It was definitely influenced by Rails but one thing it
left behind is the creepy Rails magic. Things are explicitly linked
together so it makes sense (aside from some of the Haskell constructions
which are still foreign to me). Plus these DSLs are startlingly
clean. Cabal files are like `make` meets `bundler`. Hspec is like
rspec without stuttering. But my sycophancy aside, here's how you
deploy.

#### Step 1: a basic Yesod app and local databases

```bash
# basic yesod stuff
cabal update
hsenv && source .hsenv/bin/activate
yesod init
# fill out your details and select Postgres for the database

# create the db itself
createuser APPNAME
createdb -OAPPNAME APPNAME_development
createdb -OAPPNAME APPNAME_test
```

Now edit `config/postgresql.yml` with the username and databases
you created, and remove the production database entry. We'll be
reading it from a Heroku environment variable.

#### Step 2: a new Heroku app and production database

```bash
git init . && git add . && git commit -m 'Yesod init'
heroku create --stack=cedar --buildpack https://github.com/puffnfresh/heroku-buildpack-haskell.git
heroku addons:add heroku-postgresql:dev
# take note of the color name in the url this outputs
heroku pg:promote HEROKU_POSTGRESQL_[colorname]_URL
```

#### Step 3: have the app ask Heroku for db connection info

Follow these excellent instructions to [add the Heroku
helper](http://pbrisbin.com/posts/parsing_database_url) to your
app. Two things puzzled me for a while that the article doesn't
mention. You need to modify your project .cabal file and add `heroku`
to the end of the `build-depends` and `Helpers.Heroku` to the
`exposed-modules`.

#### Step 4: tell Heroku how to run your app and deploy!

Heroku reads a Procfile to determine how to spawn various types of
processes like the web server and workers. Yesod projects come with
a Procfile you can copy and modify. It's full of scary comments
about the bad old days but you can remove all the comments and keep
the last line. So do this:

```bash
cp deploy/Procfile .
# Edit all the comments out of the file
git add Procfile
# Also be sure you added the Heroku helpers from the previous step
git commit -m 'Ready to deploy'
git push heroku master
```

This is all a one-time thing. The first deploy takes a really long
time as it installs all the dependencies. To deploy in the future
just do `git push heroku master` and it's fast and works perfectly.
