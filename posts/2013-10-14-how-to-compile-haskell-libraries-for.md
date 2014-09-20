---
title: How to compile Haskell libraries for Heroku
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
I noticed that all forks of the Haskell buildpack for Heroku are using outdated binaries. So I began what turned out to be a long journey to create Heroku-compatible binaries for the the newest versions of GHC and Cabal. It ultimately succeeded and you can use my method to build whatever Haskell binaries you want and they will be ready to run in a Heroku instance.<br /><br /><b>Step 1.</b> Clone, patch, and deploy a Vulcan build server.<br /><br /><code data-gist-id="6981119" data-gist-hide-footer="true" data-gist-hide-line-numbers="true"></code><br /><b>Step 2.</b> Download and extract the Haskell source code of the package you want to build. (It should include a <span style="font-family: Courier New, Courier, monospace;">Setup.hs</span> file.) Then submit it to your build server.<br /><br /><code data-gist-id="6981525" data-gist-hide-footer="true" data-gist-hide-line-numbers="true"></code><br /><b>Step 3.</b> Download the binaries. The build process will output a link for you to follow to get a tarball of the results.
</div>
{% endraw %}
