---
title: Bespoke Vim
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
<br />Carpenters used to make their own applewood saw handles to perfectly fit their hand. The handles outlasted the rest of the saw. When a saw wore out, the carpenter would reattach its handle to a new saw. Using the handle for years only made it smoother.<br /><br />Vim is my saw, and I have undertaken to fashion a handle. I started with the basic default MacVim installation and arrived at a comfortable, logical, and personal result.<br /><br />Others have of course done the same, and their configurations illustrate what's possible. Examples of high-quality configurations include <a href="https://github.com/skwp/dotfiles">skwp</a>, <a href="https://github.com/spf13/spf13-vim">spf13</a>, <a href="https://github.com/carlhuda/janus">janus</a>, <a href="https://github.com/vgod/vimrc">vgod</a>, <a href="https://github.com/amix/vimrc">amix</a>, and <a href="https://github.com/mrmargolis/vim_files">mmargolis</a>. You should try them and see how they feel. You'll ultimately discover your own style.<br /><br />I found that I value two things most in a vin configuration: fast manual control, and logical groupings of commands. Some configs enable vim features that I find unnerving, such as autocompleted suggestions at every keystroke, or unsolicited spell checking, or background ctagging. They provide the features of an IDE but introduce lag and make vim unpredictable. I prefer a lightning fast editor that waits for explicit commands before acting.<br /><br />The vocabulary of explicit commands should be logical. Basic vim commands require a fair amount of brute memorization and use up most of the available keys. Hence additional commands tend to get mapped to a series of keys beginning with a "leader." The leader key signals a new meaning for its followers. Whereas some configurations seem to choose key mappings indiscriminately, I prefer to define them in prefix groups. For instance, I assign all git-related commands the "&lt;<i>leader</i>&gt;g" prefix, all the shell and test running commands the "&lt;<i>leader</i>&gt;r" prefix, etc.<br /><br />When beginning to customize your vim, it helps to create a list of things you wish it could do. What do you find awkward about how you currently work? This was my list:<br /><ul><li>&nbsp; Find definition and uses of functions</li><li>&nbsp; Send text to a repl</li><li>&nbsp; Run tests</li><li>&nbsp; Summarize methods in file</li><li>&nbsp; Align symbols</li><li>&nbsp; Manipulate language blocks</li><li>&nbsp; Interrogate git</li><li>&nbsp; Clerical conversions</li><li>&nbsp; Tab completion</li><li>&nbsp; Syntax closing</li></ul><br />Next investigate the many vim plugins available to accomplish these tasks. Certain plugins are clearly champions (generally anything written by <a href="https://github.com/tpope">tpope</a>), and have been discussed ad nauseum on blogs for years.<br /><br />You can manage plugins several ways. In the beginning people manually copied them into certain local directories. Then <a href="https://github.com/tpope/vim-pathogen">Pathogen</a> came along and simplified the directory structure while leveraging git submodules to keep plugins up to date. Finally there is <a href="https://github.com/gmarik/vundle">Vundle</a>, which improves Pathogen and allows you to declare from inside your <i>.vimrc</i> which vim plugin "bundles" you want to use. This is the best option because it's concise and keeps all your environment choices inside one file.<br /><br />The best way to learn about vim plugins and configurations is to try them, and I invite you to <a href="https://github.com/begriffs/dotfiles">try mine</a>. It is fairly self-contained, well commented, and should be easy to modify. You can consult the README for installation instructions and list of key mappings.<br /><br />If you work with text every day then you owe yourself a bespoke vim. Take a cue from craftsmen of old and use a tool that fits you.
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/14113492611976509889">
  <div class="css-comment-name js-comment-name">
    Gerard thomas
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2012-11-21T07:43:29.294Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    Nice collection. You should link the screenshots to the blogs.<br />
  </div>
  <br/>
</div>
</div>
{% endraw %}
