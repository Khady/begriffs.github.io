---
title: Using cabal-dev exclusively
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
<hr /><b>Update</b>: Don't do this! Cabal version 1.18 now has&nbsp;built-in <a href="http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html">sandboxing</a>.<br /><hr /><br />Ever since I <a href="http://blog.begriffs.com/2013/09/haskell-dependencies-ok-this-is-getting.html">erased and reinstalled</a> my whole Haskell Platform and began installing packages using nothing but cabal-dev I discovered there is an extra step to compile programs in their own sandbox. I must have earlier been inadvertently leaning on globally installed libraries with using GHC to compile programs. Today when I cabal-dev installed a package I got an error during compilation that said the package couldn't be found.<br /><br />The solution is to set your <span style="font-family: Courier New, Courier, monospace;">GHC_PACKAGE_PATH</span> so it includes the places cabal-dev installs libraries. First look where all your libraries live now by doing <span style="font-family: Courier New, Courier, monospace;">ghc-pkg list</span>. Remember these paths and edit your shell's init scripts to set the environment variable. I use bash, so I add this line <span style="font-family: Courier New, Courier, monospace;">~/.bashrc</span><br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6446170"></code> Your paths will likely differ, but you see the pattern. The paths are separated by colons, and are searched in the order listed.<br /><br />Be sure the first place you have it check is the packages directory inside <span style="font-family: Courier New, Courier, monospace;">./cabal-dev</span>. This is because I run <span style="font-family: Courier New, Courier, monospace;">cabal-dev install</span> from the directory that contains my code. Now when I run ghc in this directory it will be able to find the cabal-dev sandboxed packages.<br /><br />Using search paths is a common pattern among compilers, but it confused me for a little while tonight. Makes a good topic for an off-night blog post too. :)
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/10725680599856600394">
  <div class="css-comment-name js-comment-name">
    Brian McKenna
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-09-06T00:23:44.931Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    For any new projects, you should try using the sandboxing which is included in Cabal 1.18 - released yesterday!
  </div>
  <br/>
</div>
</div>
{% endraw %}
