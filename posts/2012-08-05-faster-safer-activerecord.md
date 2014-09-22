---
title: Faster, Safer ActiveRecord
---

ActiveRecord ("AR") was introduced to abbreviate single-table
[CRUD](http://en.wikipedia.org/wiki/Create,_read,_update_and_delete) and
basic inner joins. However, if used naively in bigger queries it
can generate grossly inefficient SQL. This article will explore
tricks for keeping AR fast when using moderately complex queries.

For the full code behind this article, see my repository
[ORM-wars](https://github.com/begriffs/orm-wars). Feel free to add
your own AR tricks; pull requests are welcome.

#### Strengthening constraints

Consider a scenario with two models, User and Group where groups
have many users. We'll create migrations for the tables, and establish
the association between the models.

Migration:

```ruby
class CreateGroups < ActiveRecord::Migration
  def self.up
    create_table :groups
  end

  def self.down
    drop_table :groups
  end
end

class CreateUsers < ActiveRecord::Migration
  def self.up
    create_table :users
    add_column :users, :group_id, :integer
  end

  def self.down
    drop_table :users
  end
end
```

Models:

```ruby
class User < ActiveRecord::Base
  belongs_to :group
end
class Group < ActiveRecord::Base
  has_many :users
end
```

Notice how the association is (not) enforced. A pervasive opinion
in AR is that database constraints should be enforced by Ruby code,
not by the database. The `belongs_to` and `has_many` options do not
affect the SQL that AR generates:

```
==  CreateUsers: migrating ====================================================
-- create_table(:users)
   (3.2ms)  CREATE TABLE "users" ("id" serial primary key) 
   -> 0.0039s
-- add_column(:users, :group_id, :integer)
   (0.3ms)  ALTER TABLE "users" ADD COLUMN "group_id" integer
   -> 0.0009s
==  CreateUsers: migrated (0.0049s) ===========================================
```

This choice makes the database schema fragile. Data integrity is
not guarded from other scripts or systems which may interact with
the database. Without further precautions, the use of ActiveRecord
encourages insular Ruby applications.

Luckily there's a gem called
[foreigner](https://github.com/matthuhiggins/foreigner/) which makes
AR serious about foreign key constraints. It defines the function
`add_foreign_key` for migrations. We use it for our schema like so.

```ruby
class CreateUsers < ActiveRecord::Migration
  def self.up
    create_table :users
    add_column :users, :group_id, :integer
    add_foreign_key :users, :groups
  end

  def self.down
    drop_table :users
  end
end
```

Validations at the Ruby model level are certainly helpful. They
give form validators etc informative error messages. They are also
useful when using a less capable database backend. However, when
you are using a real database you would be wise to duplicate model
validations as database constraints in your migrations. Postgres
constraints, for instance, are [not
lacking](http://www.postgresql.org/docs/9.0/static/ddl-constraints.html).
This is most important if you plan to ever interact with your data
outside of your Ruby app.

#### Bulk imports

Continuing the story, let's populate one hundred users and three
groups, adding fifty users to one group, fifty to another, and none
to the last. The default ActiveRecord behavior is inefficient. Each
creation of a user or group issues a new command to the database,
including a new transaction. Luckily there is a gem to help:
[activerecord-import](https://github.com/zdennis/activerecord-import). It
drastically improves the performance of bulk import. Just build
arrays and import them:

```ruby
Group.import [Group.new]*3
groups = Group.find(:all, :limit => 2)
User.import (0...100).map { |i| User.new(group: groups[i%2]) }
```

#### Using joins to avoid redundant queries

Now that the database is seeded let's consider some queries and how
to write them in ActiveRecord. First, how do we find which groups
contain no users? The most obvious (but worst) way is to loop over
the groups in Ruby and filter based on subsequent queries:

```ruby
Group.all.select { |g| g.users.count == 0 }
```

However, ActiveRecord hides repeated queries behind its innocent
façade. It ends up running a total of four queries:

```
Group Load (0.3ms)  SELECT "groups".* FROM "groups" 
(0.6ms)  SELECT COUNT(*) FROM "users" WHERE "users"."group_id" = 1
(0.3ms)  SELECT COUNT(*) FROM "users" WHERE "users"."group_id" = 2
(0.3ms)  SELECT COUNT(*) FROM "users" WHERE "users"."group_id" = 3
```

The bulk of the time in these small queries is simply spent sending
them to the database. Consolidation is the best optimization.

One way to do this is by thinking more relationally, and less in
terms of objects. A left outer join can identify those groups with
no users -- the user's `group_id` will be NULL.

```ruby
Group.all(
  :joins => "LEFT OUTER JOIN users u ON u.group_id = groups.id",
  :conditions => "u.group_id IS NULL"
)
```

My benchmark shows this to be four times faster than the previous
approach, exactly as expected. Not only is it a single query, but
Postgres uses a scalable evaluation strategy:

```
------------------------------------------------------------------------------------
 Merge Anti Join  (cost=149.78..272.13 rows=1200 width=4)
   Merge Cond: (groups.id = u.group_id)
   ->  Index Scan using groups_pkey on groups  (cost=0.00..84.25 rows=2400 width=4)
   ->  Sort  (cost=149.78..155.13 rows=2140 width=4)
         Sort Key: u.group_id
         ->  Seq Scan on users u  (cost=0.00..31.40 rows=2140 width=4)
`</pre>However, we can still improve our query syntactically. ActiveRecord provides a more readable alternative to the longwinded join string
<pre>`Group.includes(:users).where('users.group_id' => nil)
`</pre>The _includes_ method does so-called [eager loading](http://api.rubyonrails.org/classes/ActiveRecord/Associations/ClassMethods.html#label-Eager+loading+of+associations). Internally it uses a left outer join. Here is the SQL that ActiveRecord generates for this statement:
<pre>`SELECT "groups"."id" AS t0_r0, "users"."id" AS t1_r0, "users"."group_id" AS t1_r1
FROM "groups" LEFT OUTER JOIN "users" ON "users"."group_id" = "groups"."id"
WHERE "users"."group_id" IS NULL
```

Essentially the same as the custom join, but with some unnecessary
columns selected. The performance difference is negligible, so using
the `includes` method is the best choice.

Note that `includes` does not work with group-by clauses because
it selects all columns, overriding previous choices of selection.
So, for instance, to count how many users are in each group, we
must use an explicit join:

```ruby
Group.all(
  :select => "groups.id, count(u.group_id) as users_count",
  :joins  => "LEFT OUTER JOIN users u ON u.group_id = groups.id",
  :group  => "groups.id"
)
```
