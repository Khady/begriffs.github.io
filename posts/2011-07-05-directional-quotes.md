---
title: Directional Quotes
---

Nesting ambidextrous quotes requires either escaping `"he said
\"hi\""` or alternating with single quotes `"he said 'hi'"`. Wouldn't
using book-style quotes `“he said “hi””`i be easier? If this all
seems unimportant, think of it this way: imagine how awkward it is
to use the pipe character `|` as ambidextrous parentheses. `|2*\|3+4\||
= 14`.

Not only does it seem easier to use left and right quotes, it might
even improve application security. SQL injection often uses quoting
tricks, which are easily unmasked when properly expressed. Injecting
value =

```sql
anything' OR 'x'='x
```

in

```sql
SELECT * FROM table WHERE column = 'value'
```

will create an unrestricted listing of the table. However, that
malicious value's quotes fail to balance when expressed as left and
right quotes:

```sql
anything” OR “x”=“x
```
