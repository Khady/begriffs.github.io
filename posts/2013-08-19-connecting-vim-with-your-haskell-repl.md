---
title: Connecting Vim with your Haskell repl
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
Only a few days in and typing code right into GHCI is getting old. I hooked up my Vim configuration so that I can send blocks of code into the repl from the editor. This is way easier, and you can do it too by using&nbsp;<a href="https://github.com/begriffs/dotfiles">begriffs/dotfiles</a>. Installing on a mac is a one-liner, and it backs up your old configuration so it's easy to give it a try.<br /><br />Here's a screencast where I show you how I use Vim and GHCI.<br /><iframe allowfullscreen="" frameborder="0" height="270" src="//www.youtube.com/embed/zpUuc259a0c" width="480"></iframe><br /><br />You'll want to watch out when sending multi-line strings to the repl, because it greedily starts trying to execute the first line. You can instruct it that a multi-line block is a single unit by enclosing it like so<br /><br /><code data-gist-hide-footer="true" data-gist-hide-line-numbers="true" data-gist-id="6265785"></code> Alternately, run <span style="font-family: Courier New, Courier, monospace;">:set +m</span> in GHCI and it will require a blank line after each input before interpreting. Anyone else have cool ways to edit and run programs?
</div>
{% endraw %}
