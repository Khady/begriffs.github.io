---
title: Software transactional memory
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
This evening I took a train to Sunnyvale to visit a Haskell guru named Joshua Ball. He got his start in the language years ago and is very comfortable with it. I had a question for the master: what is the mysterious STM type?<br /><br />Back when I was experimenting with applicative functors and wrapping values in various things like Maybe and lists I encountered STM. Josh explains it stands for software transactional memory, or as other languages like to call it, variables. Except STM doesn't contain your usual sloppy variables, it provides a totally thread-safe way to transactionally change values and run alternatives when variables are not ready to read. I'll give you an example.<br /><br />Actually I won't -- at least not tonight. That train ride was two hours each way and now it's time for bed. Check back for my next blog post to see some Haskell code that solves a multithreaded scenario which would baffle most languages.
</div>
{% endraw %}
