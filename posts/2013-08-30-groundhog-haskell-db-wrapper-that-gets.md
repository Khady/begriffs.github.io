---
title: Groundhog: a Haskell db wrapper that gets it right
---

{% raw %}
<div class="css-full-post-content js-full-post-content">
Once again I feel spoiled for starting to learn Haskell at the time I am. So many real-world tools are emerging that writing a commercial app on this stack looks promising.<br /><br />As I mentioned before, I am using Yesod to write a very basic chat API server that speaks JSON and lets people send and receive little messages. So far I learned to create an app skeleton and deploy it. I learned how to make some basic routes, and to parse JSON. Time for the real deal: database access.<br /><br />Yesod ships with the Persist library, a wrapper to marshall Haskell data types into the db and do migrations. It uses quasiquotation to invent a powerful DSL that writes both declarations of the data types and the code to insert them into the database. Adapters exist for Postgres, MySQL, sqlite, Mongo, and I think Redis. It gracefully degrades when you attach it to dbs with less structure.<br /><br />Very nice, but I think I found something even nicer called Groundhog. This one uses a DSL too, but one which gives you fine control over how your Haskell data matches up with a real database. It is refreshing to see an ORM that embraces its letter "R." It looks like it handles everything, from foreign keys to composite (natural) keys to constraints to deletion restrictions to indices. Not sure about views and joins yet.<br /><br />What's more, declaring database properties in the Groundhog DSL creates a single source of truth. It uses this definition to create migrations and generate your full relational structure. You know (if you're a Rubyist) how ActiveRecord model definitions keep declared constraints locked in Ruby code and don't inform the db? Groundhog is smarter. When you write a constraint, like "age" is greater than 21, the constraint will turn into real SQL and go into your database. Any code hitting the db outside your app and ORM cannot mess the data up.<br /><br />The catch is that Groundhog's documentation is just some code in an <span style="font-family: Courier New, Courier, monospace;">examples</span> folder in the repo. So I'm going to learn to make a real db with Groundhog and write a tutorial in the process. Yay, my first chance to contribute!
</div>
<div class="css-full-comments-content js-full-comments-content">
<div class="css-full-comment js-full-comment">
  <div class="css-comment-user-link js-comment-user-link">
  <a href="http://www.blogger.com/profile/03097657129811885396">
  <div class="css-comment-name js-comment-name">
    Chris
  </div>
  </a>
  <div class="css-comment-date js-comment-date">
    2013-08-31T01:51:11.689Z
  </div>
  </div>
  <div class="css-comment-content js-comment-content">
    I can&#39;t believe that I&#39;ve never heard of this lib before!
  </div>
  <br/>
</div>
</div>
{% endraw %}
