---
title: Unlocking Deep HTTP with JavaScript, pt 2
---

<div class="alert alert-warning" role="alert">
  <h4>Update!</h4>

  I believe my suggestion for using of Link headers for next, prev,
  first, last is deprecated. See my article [Beyond HTTP Header
  Links](2014-03-06-beyond-http-header-links.html).
</div>

In [part one](2013-12-31-unlocking-deep-http-with-javascript-pt-1.html)
I covered some of the history of HTTP and mentioned how my and other
people's understanding of the protocol was limited by poor browser
implementations. Let's leave all that behind and focus on how web
clients and servers can cleanly communicate by taking full advantage
of HTTP.

Let's focus on server-side pagination, a common part of many apps.
It serves as an illustrative case study of how developers perpetuate
an early misunderstanding even when it's no longer necessary.

The most common way to paginate is to add a query string parameter
like `"/stuff?page=2"`. This does make sense within imaginary HTML
restrictions. A plain HTML link can't say anything except protocol,
host, port, url, and query string, so the pagination info gets
tacked onto the query string. Note that even if we're sticking with
this approach, it's more flexible to use `limit` and `offset` rather
than `page`. This way the client can request any contiguous range
of results.

But how does the client know when to send those parameters? Presumably
because the authors of the client code wrote the backend too and
just know. The ends are coupled with secret information. Think about
the alternative, what if pagination could be discovered and negotiated?
This problem is not novel and not restricted to the pagination that
users see. It's essentially the same problem as downloading a large
file in parts, and selectively requesting any part (to resume a
download, for instance).

Happily this feature was introduced in RFC2616 in the 90s. The
client can use it to discover when a collection supports pagination
and then automatically add an appropriate interface for the user.
That way the client code can then use uniform list rendering code
and can gracefully degrade to non-paginated information.

Here's the trick. If we think of paginated API collections like big
downloads we can use HTTP headers to discover if pagination is
required and to restrict responses. In particular we can use the
`Range` request header. Here's an example network exchange to
demonstrate how this works.

```
Client: HEAD /biglist
Server: Accept-Ranges → items
```

In its response headers the server says it accepts the `Range`
restriction where the `Range-Unit` is expressed as "items." For
traditional file-downloading the unit is bytes, but the spec allows
custom units. What if the client disregards the hint?

```
Client: GET /biglist
Server: Status → 413 Request Entity Too Large
```

This is one option. If the response is just too large and would be
burdensome the server can refuse in a standard way. This means a
range is required. Alternately the server could send the whole list
with status 200 OK.

When the client requests a range there are three possible responses.

```
Client: GET /biglist; Range-Unit: items; Range: 0-99
Server: Status → 206; Content-Range: 0-99/1234
```

This is a successful result (a 2xx code) for partial content delivery.
It confirms the range delivered and includes the total number of
items (1234) or * if the total is unknown, hard to calculate, or
infinite. The other possible responses are status 416 for
incomprehensible range or those that go outside the available data,
and status 413 if the range is still too large to handle.

By putting metadata -- such as the total number of items -- in the
head, we can clean up the actual data returned by an API. Putting
the metadata in the JSON payload clutters the result. This is
awkward:

```json
{
  "meta": {
    "pages": 10,
    "cur_page": 2
  },
  "data": [ ... ]
}
```

It would be much nicer to get a straight array of the data as the
whole response. But once the client parses this JSON, it will need
to store the metadata, so how do we keep the data pure and clean?
In JavaScript the answer is non-enumerable properties. I'd argue
that these properties correspond roughly to HTTP headers. They won't
clutter the results of looping through a result-set, but they are
there if you ask for them directly.

```javascript
> var data = ['hello', 'mellow', 'yellow'];
undefined
> data
[ 'hello', 'mellow', 'yellow' ]
> Object.defineProperty(data, 'total_items', { enumerable: false, value: 1234 })
[ 'hello', 'mellow', 'yellow' ]
> for(var i in data) { console.log(data[i]); }
hello
mellow
yellow
undefined
> data.total_items
1234
```

Notice the loop was unaffected by the `total_items` metadata. Hence
I imagine client code will send HTTP requests for paginated data,
parse the headers and record the range discreetly using `defineProperty()`.

Finally, how should we calculate or communicate links for the first,
last, next, and previous pages? The HATEOAS constraint of RESTful
design says that this information should be provided by the server
as hypermedia affordances. Not to worry, headers to the rescue
again. RFC5988 describes link headers which point to other URLs and
describe their relation to the current document. So the server can
include

```
Server: Link → &lt;/biglist&gt;; rel="next"
```

But there's one problem, where is the range? It's no longer a query
string, so it can't be specified within the angle brackets. The
answer is to use a link-extension parameter. We can include a
parameter for the client code can interpret appropriately.

```
Server: Link → &lt;/biglist&gt;; rel="next"; items="100-199"
```

This concludes the overview of digging into HTTP to solve a common
feature of web apps.
