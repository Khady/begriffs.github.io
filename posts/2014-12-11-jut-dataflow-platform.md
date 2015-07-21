---
title: Intro to the Jut Dataflow Platform
video: true
---

[Michael Demmer](https://github.com/demmer), VP Enginnering at
[Jut](http://www.jut.io/) unveils their dataflow platform which has
been under development for the past year and a half.

In this video Mike gives some demos of the system and shares how
they think about modeling timeseries data along with unstructured
events in a complex system.

<video poster="https://i.vimeocdn.com/video/499957414.jpg?mw=700"
       class="video-js vjs-default-skin" controls preload="auto">
  <source src="http://player.vimeo.com/external/114224611.hd.mp4?s=76850f410b1202d175397f1db626fc15" type="video/mp4">
</video>

### Overview

* Jut wants to tackle the problems of managing complex systems
* Vs existing products which focus on one small slice of the pie
* Data is diverse
    * We have regular metric data
    * ...and also unstructured events like server logs and site actions
    * Historical and current events
* Want to take all types of data, analyze it and automate alerts

#### Small Demos
* Graphing 90th percentile server response times across various services
* Single server response times overlayed with error events
* Modularity of the Juttle language
* Data joins
* Sharing and loading Juttle gists within the Jut Playground

#### Data Integration
* How to integrate in a SaaS/on-premise hybrid
* Logs, metrics, events generated behind a firewall can be processed back there in a Jut “engine”
* The UI and auth lives on jut.io, and it sends commands through your firewall via a control channel

#### Applications in Practice
* Getting meta: Using Jut to monitor Jut usage itself
* Example analyzing actual SSH break-in attempts on jut.io
* Using the same program to view and mash up events gathered in different ways
* Memory profiling of application
* Q&A
