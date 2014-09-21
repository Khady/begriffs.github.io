---
title: Unlocking Deep HTTP with JavaScript, pt 1
---

When rich JavaScript apps first started getting popular a few years
ago, it honestly took me a while to catch on. I mistakenly thought
it was primarily an optimization of page load times, a technique
that sacrificed simplicity and forced developers to reinvent mature
technology like browser page-history.

I was wrong — I think client side apps provide a better developer
experience fundamental ways. The design of server-side apps is built
around historical browser limitations.

HTTP begin in 1991 as a simple protocol. It had one (idempotent)
method, `GET`. You would use it to get documents over TCP. The
essentially modern [Version 1.1](https://tools.ietf.org/html/rfc2068)
was standardized at the beginning of 1997. Those were the days of
Internet Explorer 2. Compared with the foresight and subtlety of
HTTP 1.1 these browsers were quite primitive.

Browser technology improved slowly, and during this time we developers
were getting accustomed to the status quo. But let's stop and think
of how little a vanilla (non JavaScript) page can do. It can basically
do three things.

First thing: primitive transitions. The page can link to another
page with a `GET` request. This link can control only the protocol,
URL and query string. No headers beyond server-set cookies, and no
other methods. The page can `POST` content type
`application/x-www-form-urlencoded` using forms.

Second: when a new page loads, the browser can show it, or redirect.
Until relatively recently some browsers failed even to handle all
the redirect codes like 303. Forget reacting appropriately to HTTP
status codes. Browsers neglected actions for some status codes,
like refreshing the page on 503 (service unavailable) errors according
to the `Retry-After` header. Browsers just load the page and wait.

Third: caching. With the slow dial-up connections of those days
people pretty much had to get this one working. Of course there
were plenty of quirks.

Enter XMLHTTPRequest. Looking back, this may be the most significant
feature added to the browser, because it frees JavaScript from the
default browser behavior. It was 2006 before this feature was
widespread enough to really use. Whereas previously you could only
set `document.location` for a blunt dynamic hyperlink, after the
introduction of XMLHTTPRequest you could react to HTTP status codes,
and read/write headers. You could fundamentally script the browsing
experience.

But to me at least this power didn't really sink in. A link is a
link, right? A form is a form. I guess this Ajax thing is just for
updating little parts of the page... My experience prevented me
from thinking outside these conventions. In the next two blog posts
I'm going to help you reimagine the possibilities for the way modern
client-side apps interact with the server over HTTP. We'll use
pagination as a case study.

In the next post I'll discuss a way to do server-side pagination .
Hint, there won't be any `"?page=2"` nonsense. In the post after
that I'll walk you through a way to extend AngularJS's $resource
service to automatically accommodate pagination by using HTTP to
negotiate with the server.
