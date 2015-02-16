---
title: Deploying Microservices
video: true
---

I explain my new open-source experiment, the
[microservice-template](https://github.com/begriffs/microservice-template)
which you can use to deploy, scale, and monitor any kind of cloud
microservices. The project contains preconfigured server definitions
for common programs for collecting statistics, making a dashboard,
and managing work queues.

<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/119660173.hd.mp4?s=c01b34d9882a7a179c3b885b8d523e0e"
         poster="https://i.vimeocdn.com/video/507185690.png?mw=700"
  ></video>
</div>

### Summary

* Initial motivation: building a distributed scraping system
* Need a scalable architecture for high bandwidth and to avoid rate-limiting
* Ran into the problem of insufficient feedback about jobs
* We need feedback to avoid accidental DDoS and for debugging the system
* The first half of the solution is to log absolutely everything
* The other half is system resource usage sampling
* Then the logs must be consolidated and presented in a dashboard
* Next big goal: simply to deploy, reproducible behavior, and flexible operation
* The technologies inside and how they work together
    * Grafana
    * InfluxDB
    * Statsd
    * Collectd
    * RabbitMQ
    * Packer
    * Consul
    * Terraform
    * Chef
* Servers should be disposable commodities
* They also need to be able to discover each other dynamically
* The system turns out to be promising for services beyond just scrapers
* The philosophy of rederiving data as needed from a master copy
* Demo
