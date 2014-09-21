---
title: Create a static site with Hakyll, Github and Travis CI
---

"Static sites are fast, secure, easy to deploy, and manageable using
version control." So states the webpage for <a
href="http://jaspervdj.be/hakyll/">Hakyll</a>, a great way to set
up a static site or blog. It allows you to write blog posts by
simply editing markdown in git, all the while having access to
delicious Haskell for deeper customizations.

You can configure things to let you write blog posts directly on
Github's interface and use Travis CI to deploy your changes. Most
day-to-day blogging will not require using Haskell at all or even
having the Haskell environment installed on the blogger's machine.

I'll show you how to set everything up, including an optimized
Travis config that can deploy changes in less than a minute. There
is some existing information online about doing this sort of thing,
but it's all outdated in one way or another.

We'll be using Github Pages to serve the final assets. I'll assume
you're making a static site for a Github organization called `myorg`
and want it to live at `myorg.io`.

### Installation

1. Create a Github organization. E.g. `myorg`
1. Create a project `myorg/myorg.github.io`
1. The master branch will be repeatedly overwritten and committed
   later on by Travis, so you won't make any edits there directly. For
   now add a file to the root of the master branch called `CNAME`
   containing the string `myorg.io`
1. Create two `A` records in the DNS for `myorg.io` pointing at
   192.30.252.153 and 192.30.252.154 respectively.
1. Generate the base Hakyll project.
    ```bash
    # in an empty directory of your choice
    # NOT in the git repo you've been using

    cabal sandbox init
    cabal install -j --disable-documentation hakyll
    cabal exec hakyll-init myorg.github.io
    ```
1. Create an orphan source branch in your git repo and copy the
   generated files there.
    ```bash
    git checkout --orphan source
    git rm CNAME
    cp -r /path/to/generated/myorg/* .
    git add .
    ```
1. Reuse the cabal sandbox you created earlier:
    ```bash
    cp -r /where/you/ran/cabal/install/.cabal-sandbox .
    ```
1. Keep build artifacts out of git by adding these lines to `.gitignore`
    ```
    .cabal-sandbox
    cabal.sandbox.config
    dist/
    _cache
    _site
    ```
1. Run your new site locally to see that it works!
    ```bash
    cabal sandbox init
    cabal run rebuild
    cabal watch

    # now load http://localhost:8000
    ```
1. Create `.travis.yml` and add the following boilerplate:
    ```yaml
    language: haskell
    ghc: 7.8
    branches:
      only:
      - source
    before_install:
      - git submodule foreach --recursive 'git checkout master; git ls-files | grep -v README | grep -v CNAME | xargs -r git rm'
    install:
      - curl http://bin.begriffs.com/hakyll/cabal-sandbox.tar.xz | tar xJ
      - cabal sandbox init
      - cabal configure --disable-library-profiling --disable-tests --disable-library-coverage --disable-benchmarks --disable-split-objs
    before_script:
      - git config --global user.email "$GIT_EMAIL"
      - git config --global user.name "$GIT_NAME"
    script: cabal run -j build
    after_script:
      - cd _site
      - export REMOTE=$(git config remote.origin.url | sed 's/.*:\/\///')
      - git remote add github https://${GH_TOKEN}@${REMOTE}
      - git add --all
      - git status
      - git commit -m "Built by Travis ( build $TRAVIS_BUILD_NUMBER )"
      - git push github master:master | grep -v http
    ```
1. Generate a Github [auth
token](https://help.github.com/articles/creating-an-access-token-for-command-line-use).
1. Set encrypted environment variables to allow Travis to commit
   to the master branch
    ```bash
    gem install travis
    travis encrypt -r myorg/myorg.github.io --add GH_NAME="J. Doe" GH_EMAIL=jdoe@myorg.io GH_TOKEN=xxxxxxxx
    ```
1. Commit all the files.
1. Enable Travis for your repo. [Instructions
here](http://docs.travis-ci.com/user/getting-started/#Step-one%3A-Sign-in).
1. Push the `source` branch to Github.
1. Watch the deploy progress at https://travis-ci.org/myorg/myorg.github.io

Now you can [create and
edit](https://help.github.com/articles/editing-files-in-your-repository)
blog posts right in Github and your changes get deployed automatically.

#### (optional) Generating a custom cabal sandbox for Travis

You can use my [shared cabal
sandbox](http://bin.begriffs.com/hakyll/cabal-sandbox.tar.xz) on
Travis as done above, or you can build your own. It's a little
trickier. Use this Travis config as a start. It takes advantage of
post-build deployment to S3.

```yaml
language: haskell
ghc: 7.8
branches:
  only:
  - source
before_install:
  - travis_retry sudo add-apt-repository -y ppa:hvr/ghc
  - travis_retry sudo apt-get update
  - travis_retry sudo apt-get install --force-yes happy-1.19.3 alex-3.1.3
  - export PATH=/opt/alex/3.1.3/bin:/opt/happy/1.19.3/bin:$PATH
install:
  - cabal sandbox init
  - cabal install -j --disable-documentation -fhttps pandoc
  - cabal install -j --disable-documentation --disable-tests --reorder-goals
deploy:
  provider: s3
  access_key_id: AKIAIYJJY5B5UWSU3CFQ
  bucket: YOUR_BUCKET
  region: us-west-1
  skip_cleanup: true
  local-dir: ".cabal-sandbox"
  upload-dir: hakyll
  acl: !ruby/string:HighLine::String public_read
  on:
    repo: myorg/myorg.github.io
    branch: source
  secret_access_key:
    secure: YOUR_SECURE_KEY
```
