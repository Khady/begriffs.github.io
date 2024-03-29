---
title: You Don't Know Your Visitors, So Stop Pretending
---

#### Part 1: What is analytics really?

Web analytics should hurt a little. Not just the pain of seeing
your low traffic revealed in hard numbers, but the realization that
you don't really know your visitors.

We programmers tend to be preoccupied by our creations. We repetitively
visit our sites, refining and building. Reading our own copy again
and again becomes almost hypnotic. The sentences start to seem so
right, so laden with all the meanings of our intentions and our
history with the project.

Then we unveil our website to the public and shit gets real.

High bounce rates abound. Visitors mysteriously fail to get very
far into the site. Visitors talk trash about the experience (if
they talk about it at all). What can you do? Your secret weapon is
properly designed analytics.

First let's examine how people misunderstand analytics. The common
belief is that it's about measuring traffic. We plug some magic
javascript into our site and then sit mesmerized staring at maps
and charts which show people pouring in from around the world. How
gratifying. How passive.

These type of data are known as "vanity metrics," and they don't
really do you any good, except sometimes to boost your vanity. They
do not help you improve any more than would a doctor who merely
observes, "Looks like you're sick. Best of luck!"

Real improvement comes from asking questions. The amount of technology
involved varies. At its most primitive (and often most effective),
learning about your website involves sitting someone down, giving
them the barest outline of what your site is about, and watching
them use it. Make sure you have something to write on while the
user points out your ambiguous language, obscure navigation, and
unintended connotations.

After doing this basic in-person testing and after ironing out the
major flaws you are ready to enable more automated analytics. My
favorite tool is Google Analytics, but it takes some work to get
the most out of it.

Begin by identifying your goals for your site. Maybe you're selling
things and your goal is to maximize sales. Maybe you're informing
people, and your goal is for them to learn specific things. Make
your goals specific and explicit. Let's use an example.

Suppose you're a technology consulting company. You're looking for
clients. But you're looking for the right clients, those whose
projects align with your skills and inclination. Let's work backwards.
Your primary goal is that prospective clients contact you. You can
measure this, and I'll explain some techniques later. For now notice
there are other goals as well.

For instance you want to communicate your company's specialty and
strengths. Maybe you have a unique process of product design, maybe
you're exceptionally fast. In any case, you would like prospective
clients to know basic things about you so that you're not only
likely to get contacted, but get contacted by the right people.

The next step is to make the goal specific in terms of visitor
actions on your site.

* **Goal**: Visitors contact you.
* **Telltale**: Either they fill out your contact form and press
  the submit button, or they follow a "mailto:" link, or they select
  the email address on the page to copy it.
* **Goal**: Visitors know your firm's process and specialty.
* **Telltale**: They visit a specific page about it and stay on
  that page for a given amount of time, or they watch a video about
  you at least most of the way through.
* **Goal**: Informed visitors contact you.
* **Telltale**: The visitor has fulfilled both of the previous goals
  during their visit.

Notice two things about these goals. One, their telltales don't
cover all cases, but they are useful nonetheless. For instance, a
visitor may arrive on your site already knowing about you. They go
straight to the contact form, so according to "the rules" they are
counted as a contact, but not an informed contact. However, the
telltales aren't misleading in the other direction. If someone
spends a long time reading about your process and past clients and
watches your videos, then they are certainly informed.

The second thing to notice is that goals can be built out of other
goals. Sequences of desired actions (like the informed contact) are
known in web analytics as "funnels." By identifying a funnel such
as "the visitor learns about us _and then_ contacts us," you allow
analytics software to look for the weak links. That is, the steps
in the process where people most frequently quit. Our example funnel
is very short, but think of a funnel for buying things online. It
goes all the way from editing your cart to entering billing info,
to ..., to placing the order.

Once you identify goals and translate them into telltale actions
on your page you are ready to write code to report the appropriate
visitor actions to your analytics server. At least on Google
Analytics, many actions are so basic that you don't have to write
any code to track them. Google will track them by default. However
other more customized telltale actions require customized code.

Before discussing the specifics of Google Analytics code, let's
consider: what good do these goals do you?

First, starting with goals forces you to make the purpose of your
site explicit. Ever visit brochure websites that are incoherent and
smothered in inane social media icons? Why do people (usually
organizations) make these? It's because they don't know what they
want. In the absence of explicit prioritized goals a committee of
designers will create a well-intentioned slurry of failure.

Second, measurable goals let you use science. Defining the telltale
visitor actions that constitute a goal allows you to write code to
reliably measure success. Data allows you to do experiments. You
can finally answer questions definitively. "Does the front page
carousel increase visitors' time spent reading our blog articles?"
"Does the contact link in the footer increase contacts?" When your
design committee starts blathering, you just point to the charts.

Third, you can automate your experiments. Google Analytics offers
a feature called "content experiments" that let you specify alternate
versions of pages in your site. Google will choose which versions
visitors see and report how various configurations affect your
goals.
