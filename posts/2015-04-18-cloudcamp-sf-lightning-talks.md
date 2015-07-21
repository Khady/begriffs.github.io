---
title: SF CloudCamp Lightning Talks
video: true
---

1. [Intro to Docker](#intro-to-docker)
2. [Exploring Strategies for Minimal Containerization](#exploring-strategies-for-minimal-containerization)
3. [Storage, Containers, and Microservices](#storage-containers-and-microservices)
4. [Docker Networking with Clocker and Project Calico](#docker-networking-with-clocker-and-project-calico)
5. [Continuous Integration with Docker](#continuous-integration-with-docker)

### Intro to Docker

[Nathan LeClaire](http://nathanleclaire.com/), an engineer at Docker explains the
tool from first principles as it were.

<video poster="https://i.vimeocdn.com/video/515466205.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="http://player.vimeo.com/external/125381777.hd.mp4?s=9e70068e605126b306f77cb0be7d8a7a" type="video/mp4">
</video>

* There is a lot of noise and media about Docker, but what is it fundamentally?
* The precursor to containers came from Google circa 2005 managing giant workloads of linux processes
    * A very good paper was just released about this, [Large-scale cluster management at Google with Borg](https://static.googleusercontent.com/media/research.google.com/en/us/pubs/archive/43438.pdf)
    * So you’re Google in 2005 and you have the abstraction of unix processes
    * How do you deal with noisy neighbors - aka processes which gobble up resources
    * Their answer was patching the Linux kernel to improve process isolation
    * Resources are isolated with so-called namespaces, and can assign priority
* All this tech was contributed to the Linux kernel and evolved over time into a project called LXC
    * However it was hard to use and inaccessible
    * Then Dockcloud came along and made a platform-as-a-service
    * Their PaaS did not do so hot, but it did wrap LXC in a clean way
* Docker’s is best thought of as an *interface*, not a container.
    * It took a bunch of esoteric things and wrapped them up in a fun interface
    * It sets the stage for repeatability and isolation in your infrastructure

### Exploring Strategies for Minimal Containerization

Presented by Brian "[Redbeard](http://www.brianredbeard.com/)" Harrington.

<video poster="https://i.vimeocdn.com/video/515477269.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="http://player.vimeo.com/external/125385727.hd.mp4?s=175a6d052a48e690466d3c2c3b5f3dfa" type="video/mp4">
</video>

* Kubernetes is a fascinating project
* It is based on the internal ideas of Google Borg and, later, Omega
* Google doesn’t traditionally release open source projects, but they did for Kubernetes
* Decomposing the technology
    * Applications and containers
    * There are a number of answers to this, including Docker
    * CoreOS made a competing product called Rocket, aka “rkt”
* The CoreOS folks have been informed by Omaha, a system for self-updating programs
    * Omaha is the mechanism that updates Chrome and Google Earth
* When you combine containerization, auto-updating and CoreOS you
  get [Tectonic](https://tectonic.com/), a version of Kubernetes
  designed to run on platforms other than Google Compute Engine

### Storage, Containers, and Microservices

[Luke Marsden](https://twitter.com/lmarsden) CTO, Founder at ClusterHQ
explains how his company is working to improve the interaction and
persistence of storage with Docker.

<video poster="https://i.vimeocdn.com/video/515499474.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="http://player.vimeo.com/external/125409828.hd.mp4?s=b19c6ea1230404dbaadc5eb4e88e71e4" type="video/mp4">
</video>

* Everybody loves containers because packaging is now so easy
* We used to have a quagmire of install scripts and packages
* And pushing the images to production is easy with today’s
  orchestration systems like Kubernetes, Mesos, Swarm, Deis etc
* But what about state? How do you store databases stored inside a
  container like the rest of your application?
    * It would be good to put databases through the continuous
      integration and continuous deployment pipelines like the rest
      of your services
    * Docker Volumes provides only file-based access whereas most
      storages services are block-level, like EBS
    * You can’t manage block devices from within your containers
* Zoning
    * It can be difficult to move data between availability zones
* Not all orchestration managers are stateful
    * They usually subscribe to the 12-factor app manifesto
    * So data services have to be an external thing not managed by your orchestration system
    * Kubernetes can manage state with storage drivers, but they ignore local storage
* What would be cool is to have a “Docker for storage,” a unified REST interface
    * ClusterHQ is trying to solve this problem. They are building
      an open source project called [Flocker](https://github.com/ClusterHQ/flocker)

### Docker Networking with Clocker and Project Calico

[Andrew Kennedy](https://twitter.com/grkvlt) Distributed Systems
Hacker at CloudSoft shares the various bits of technology they use
at his company, notably the open source tool
[Clocker](https://github.com/brooklyncentral/clocker).

<video poster="https://i.vimeocdn.com/video/515501140.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="http://player.vimeo.com/external/125411064.hd.mp4?s=00c723162f77b8e68695572418948c38" type="video/mp4">
</video>

* Clocker manages a Docker cloud, manages VMs, and installs Docker
  on them using whatever SDN (software defined networking) you want
* Multi-host multi-container apps with seamless networking
* The basic cloud management system is Apache Brooklyn
    * Clocker adds Docker blueprinting to Brooklyn
    * They use Brooklyn, jclouds, weave, opendove, and calico

### Continuous Integration with Docker

[Dave Nielson](https://twitter.com/davenielsen) the Co-Founder of
CloudCamp and organizer of this particular event also gives a
lightning talk to explain a strategy of combining the best properties
of Platforms-as-a-service with containers.

<video poster="https://i.vimeocdn.com/video/515473232.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="http://player.vimeo.com/external/125385728.hd.mp4?s=637d5b8d46a95703612941d609c2377b" type="video/mp4">
</video>

* Why am I such a big fan of the PaaS?
* When you do server management yourself, maybe you do a good job, but when you do it really well why keep it to yourself?
* Containerization kind of changed how people do cloud computing
* However containers can accumulate and take work
* It can also be forbidding to the command-line averse
* Mobile app creators are a different breed
    * Apps either blow up or die so fast that app builders don’t care too much about the longevity of their stack or about vendor lock-in
    * “Us: watch out you’ll get locked in! Them: Oh yeah? 18 billion dollars!”
* We can get the benefits of a Platform-as-a-Service without the lock-in by developing with a buildpack stuffed inside a container
    * It feels like you’re deploying to Heroku or Cloud Foundry but it’s all inside a container
    * But you don’t deploy the buildpack part to production, you deploy the app that it creates, all containerized
