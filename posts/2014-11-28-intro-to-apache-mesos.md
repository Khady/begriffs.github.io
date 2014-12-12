---
title: Intro to Apache Mesos, the distributed systems SDK
video: true
---

[Niklas Nielsen](https://twitter.com/quarfot), distributed systems
engineer at [Mesosphere](https://mesosphere.com/) gives an overview
of how [Mesos](https://mesos.apache.org/) manages resources to
ensure their fair and efficient use in a compute cluster. He also
demonstrates two higher-level frameworks on top of Mesos which keep
jobs alive and manage timing and dependencies.

<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/113052137.hd.mp4?s=7564b3ae933804f64a12ebfe42389af1"
         poster="https://i.vimeocdn.com/video/499956815.webp?mw=1200&q=70"
  ></video>
</div>

### Overview

* What is Apache Mesos (and what is it not)
* Abstracting from physical machines to resources
* “Everything fails all the time”
* Mesos’ heuristic for the NP-Hard cloud scheduling problem
* Delegation to local decision-making nodes
* Resource offers
* Scheduling tasks across racks or nodes using attributes
* Avoiding resource starvation using reservations
* Mesos is a kernel with which you rarely interact directly
    * You use frameworks on top
    * **Marathon** starts processes in a mesos cluster and does deployments and upgrades
    * **Chronos** is a distributed cron with dependencies
* Twitter uses Mesos to handle
    * 240 million monthly users
    * 150k tweets per second
    * 100TB per day of compressed data
