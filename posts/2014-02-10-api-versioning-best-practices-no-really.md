---
title: API versioning best practices
---

While preparing to create the next version of the
[Bountysource](http://bountysource.com/) API, I've been reading
articles about versioning. There are many contradicting opinions
online.

Many articles argue that APIs should be easy for developers to
explore in a web browser, so API versions should be specified in
the url, like `https://api.site.com/v23/foo` rather than being
negotiated in the HTTP headers. How else could you explore the API
through a web browser? However I'd argue that we have browser
extensions for this
([1](https://chrome.google.com/webstore/detail/advanced-rest-client/hgmloofddffdnphfgcellkdfbfbjeloo?hl=en-US)),
9[2](https://chrome.google.com/webstore/detail/rest-console/cokgbflfommojglbmbpenpphppikmonn?hl=en)),
([3](https://chrome.google.com/webstore/detail/simple-rest-client/fhjcajmcbmldlhcimfajhfbgofnpcjmb))
([4](https://chrome.google.com/webstore/detail/postman-rest-client/fdmmgilgnpjigdojojpjoooidkmcomcm?hl=en)).
Also no end user needs to make direct old-school bookmarks of an
API resource.

I think versioning in the URL is not the right choice, and it stems
from a historical constraint. Developers had to resort to JSONP to
make cross-origin requests in old browsers, and you can't specify
headers with JSONP. Nowadays CORS support is widespread. If you
really have to support IE <10 you can actually encode headers
over JSONP as a query parameter if the client and server cooperate.

I think that versioning in the URL ignores how the web already
works. When I want `/foo` in Spanish I don't request `/es/foo`, I
send an `Accept-Language` header. In fact many representational
details are negotiable, like the charset, the encoding, and the
mime type of a resource. We don't put them in the URL because their
position is irrelevant. It gets absurd: `/es/utf8/application-json/foo`.
Ultimately we are just requesting different representations of a
single resource.

So what's the right way? I propose using the `Accept` header with
type `application/vnd.you.com+format; version=n` where format
is e.g. `json`. Rather than using [semantic versioning](http://semver.org/)
conventions, we specify major version only, since only breaking
changes matter. Give clients backward-compatible changes for free;
they can handle it.
