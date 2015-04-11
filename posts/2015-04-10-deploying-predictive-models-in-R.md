---
title: Deploying Predictive Models in R
video: true
---

[Nick Elprin](https://www.linkedin.com/pub/nick-elprin/38/a0/b3),
co-founder of [Domino Data Lab](http://www.dominodatalab.com/) talks
about strategies for organizing R code to make it easier to deploy
and scale as low-latency web services. He then shows how these
techniques work in cooperation with his platform-as-a-service which
turns R functions into API calls.

<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/124671402.hd.mp4?s=e2a64ba66124dc454e3d6126d2001124"
         poster="https://i.vimeocdn.com/video/514501213.png?mw=700"
  ></video>
</div>

### Summary

* Principles of writing R code that makes it easier to deploy and operationalize predictive models you’re building
* An intro to Domino Data Labs (data science platform-as-a-service)
* What languages might you use for building a predictive model? What languages for deploying a web production app?
    * These sets form a Venn diagram with Scala, Java, Python in the intersection
    * R is all alone in the predictive-model-only circle
* Increasingly the two worlds of predictive models and production applications need to talk to each other
* There can also be organizational friction when a development is split between data science and software engineering
    * Deploying new models can have some hurdles
    * One approach is the software engineers porting an R model to, say, Java
    * Out of phase release cycles
* This is the backdrop for Domino, which wraps an R model with a clean REST interface
    * Allows data scientists to control their own destiny. They can deploy whenever they want.
* Demo
    * Predicting wine quality from chemical properties
    * Using a random forest classifier just as an example
    * Then save the classifier to a file
    * Another file, predict.r, requires this file to have been created
    * So we run the training program on the cloud
    * Then “publish” the prediction file and chosen function inside
* Design considerations
    * Zero downtime upgrades
    * Only switches to new model when it is ready
    * High availability (restarting dead R code)
    * Versioning, rollbacks, request logging
    * Scheduled training updates
* Best practices
    * Make prediction code thread-safe (don’t mutate shared state)
    * Separate logical steps of code into scripts or functions
    * Training/initialization vs prediction
    * Leverage serialization tools
    * R’s save() function is a great way to do this as RDA files
* Use cases
    * recommendation systems
    * insurance
    * Lease or credit card approvals
* Domino’s blog welcomes guest contributions
