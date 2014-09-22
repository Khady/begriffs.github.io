---
title: Spam is Dead, long live email forwarding
---

I used to be wary of providing my email address to websites. Either
I would do it and bear the spammy consequences, or I would use a
[disposable](http://mailinator.com/) email service. Neither is
optimal, because I often want to allow a site to continue contacting
me, but I want to feel confident I can block its messages at any
time. What's more, I'd like to be able to prevent the site from
selling my address to other spammers.

One solution is to use lots of email accounts and use certain
accounts for certain types of website. That's almost as much trouble
as dealing with spam. Another solution is [plus
addressing](http://www.techiecorner.com/22/create-gmail-with-plus-addressing-to-prevent-spam/)
but it exposes the underlying email address before the plus symbol.

The true solution uses your own domain, so it may not work for
everyone.

* Create your primary email address as [long random string]@yourdomain.com
* When you sign up on a website, say Twitter, use a new email address
  like twitter@yourdomain.com which forwards to your secret address.
* (optional, paranoid) Add
  [salt](http://en.wikipedia.org/wiki/Salt_%28cryptography%29) to
  these addresses, e.g. twitter-[random string]@yourdomain.com so
  others can neither guess them, nor probe which services you use.
* If you start getting spam, check its to-address and delete the
  forwarding.Many hosts provide an easy way to create email forwards
  through cPanel, but I set up some scripts to make life easier.
  Adding, deleting, and listing my forwards are just a command away
  in the shell.
