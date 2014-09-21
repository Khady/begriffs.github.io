---
title: Beyond HTTP Header Links
---

The idea of *Hypertext As The Engine Of Application State* is that
each hypermedia resource provides links to related resources. These
links are typically called affordances. Clients can decouple
themselves from a server by following affordances rather than
constructing -- or memorizing -- URLs. Servers are then free to
change URLs or gracefully degrade service.

One of the more popularly adopted affordances are pagination link
headers. The server includes links to the *prev*, *next*, *first*,
and *last* pages at each page in a series. The client can disregard
the URL scheme and just follow the links to access the pages as a
doubly-linked list.

Problem: pagination is often random-access. Look familiar?

![Linked list](/images/pagination-1.png)

Sure, it has previous and next links but there are also links to
go directly to numbered pages. Some interfaces embrace random access
even more directly.

![Random access](/images/pagination-2.png)

In a potentially unbounded sequence we can't include a header link
to each page. What link relations would we use, and how would the
client know what they mean? Ideally we would like a content agnostic
way to paginate large resources, a standards-compliant way that
doesn't have to adjust the URL.

If we can find a way to expose affordances for random-access
pagination then why include the traditional four links? I'd prefer
a standard way to access any page at all, which would make the
first, last, next and prev links superfluous.

The solution is, I think, range headers. They were originally
constructed to resume big downloads. Perfect! What is pagination
but a slow download into human eyes? The HTTP 1.1 spec provides a
standard way to request part of a resource and to discover how much
is left to go. Let's see how it works.

```
Request
  GET /resource
Response
  Status 206 (partial content)
  Accept-Ranges: items
  Range-Unit: items
  Content-Range: 0-249/1000000
```

Here the client asks for a resource. The server doesn't want to
send more than two hundred and fifty items at once, so it sends a
partial response. A client that understands the "Accept-Ranges"
affordance can now request specific ranges.

The client needn't inspect or adjust the URL to select a new range. It just sets a header

```
Request
  GET /resource
  Range-Unit: items
  Range: 0-24
Response
  Status: 206
  Range-Unit: items
  Content-Range: 0-24/1000000
```

Notice the negotiation. Both the client and server have limited
numbers of items they want to consume or serve at once. In the first
request the server limited the response, and in the second it honored
the client's request to further limit it. At this point the client
knows what range it has been given, along with the total number of
items. Requesting any page is just math.

I'd suggest this supersedes the classic pagination link headers.
It's equally restful yet more powerful.
