---
title: Bespoke Vim
---

Carpenters used to make their own applewood saw handles to perfectly
fit their hand. The handles outlasted the rest of the saw. When a
saw wore out, the carpenter would reattach its handle to a new saw.
Using the handle for years only made it smoother.

Vim is my saw, and I have undertaken to fashion a handle. I started
with the basic default MacVim installation and arrived at a
comfortable, logical, and personal result.

Others have of course done the same, and their configurations
illustrate what's possible. Examples of high-quality configurations
include [skwp](https://github.com/skwp/dotfiles),
[spf13](https://github.com/spf13/spf13-vim),
[janus](https://github.com/carlhuda/janus),
[vgod](https://github.com/vgod/vimrc),
[amix](https://github.com/amix/vimrc), and
[mmargolis](https://github.com/mrmargolis/vim_files). You should
try them and see how they feel. You'll ultimately discover your own
style.

I found that I value two things most in a vin configuration: fast
manual control, and logical groupings of commands. Some configs
enable vim features that I find unnerving, such as autocompleted
suggestions at every keystroke, or unsolicited spell checking, or
background ctagging. They provide the features of an IDE but introduce
lag and make vim unpredictable. I prefer a lightning fast editor
that waits for explicit commands before acting.

The vocabulary of explicit commands should be logical. Basic vim
commands require a fair amount of brute memorization and use up
most of the available keys. Hence additional commands tend to get
mapped to a series of keys beginning with a "leader." The leader
key signals a new meaning for its followers. Whereas some configurations
seem to choose key mappings indiscriminately, I prefer to define
them in prefix groups. For instance, I assign all git-related
commands the "<*leader*>g" prefix, all the shell and test running
commands the "<*leader*>r" prefix, etc.

When beginning to customize your vim, it helps to create a list of
things you wish it could do. What do you find awkward about how you
currently work? This was my list:

* Find definition and uses of functions
* Send text to a repl
* Run tests
* Summarize methods in file
* Align symbols
* Manipulate language blocks
* Interrogate git
* Clerical conversions
* Tab completion
* Syntax closing

Next investigate the many vim plugins available to accomplish these
tasks. Certain plugins are clearly champions (generally anything
written by [tpope](https://github.com/tpope)), and have been discussed
ad nauseum on blogs for years.

You can manage plugins several ways. In the beginning people manually
copied them into certain local directories. Then
[Pathogen](https://github.com/tpope/vim-pathogen) came along and
simplified the directory structure while leveraging git submodules
to keep plugins up to date. Finally there is
[Vundle](https://github.com/gmarik/vundle), which improves Pathogen
and allows you to declare from inside your _.vimrc_ which vim plugin
"bundles" you want to use. This is the best option because it's
concise and keeps all your environment choices inside one file.

The best way to learn about vim plugins and configurations is to
try them, and I invite you to [try
mine](https://github.com/begriffs/dotfiles). It is fairly self-contained,
well commented, and should be easy to modify. You can consult the
README for installation instructions and list of key mappings.

If you work with text every day then you owe yourself a bespoke
vim. Take a cue from craftsmen of old and use a tool that fits you.
