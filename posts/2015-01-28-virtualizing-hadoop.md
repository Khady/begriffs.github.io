---
title: Virtualizing a Hadoop Cluster (two videos)
video: true
---

[Tom Phelan](http://joelburget.com/), Chief Architect at
[BlueData](https://www.linkedin.com/pub/tom-phelan/0/749/61b) talks
about the appropriate situations in which to virtualize Hadoop,
either in containers or in virtual machines. In evaluating the
situations he explains what questions you should and should not be
asking.

<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/117907449.hd.mp4?s=de0ba4566372c0f6ae14d8bb51f50497"
         poster="https://i.vimeocdn.com/video/504771461.png?mw=700"
  ></video>
</div>

### Summary

* What BlueData has learned about running Hadoop jobs
* Under what situations should one virtualize a Hadoop cluster
* The shape and components of a physical cluster
    * Both master and worker controllers contain
    * Disks, server, named node, and resource manager
* Types of virtualization:
    * public cloud
    * private cloud / hypervisor (strong fault isolation)
    * private cloud / containers (weak fault isolation)
    * paravirtualization
* The appropriate infrastructure for various situations
    * Questions NOT to ask
    * Questions to ask
* Performance and Data Locality
* Five use-cases and their infrastructure needs

### Pt 2: Orchestration with Docker

[Joel Baxter](https://twitter.com/joel_k_baxter), also at BlueData,
leads a breakout session about what an ideal orchestration manager
would look like for managing Hadoop clusters and associated data.
The state of the art is evolving but not there yet.

<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/118047022.hd.mp4?s=91cfdf09dcdfcc374d31c69c3008ce3a"
         poster="https://i.vimeocdn.com/video/504974721.png?mw=700"
  ></video>
</div>

### Summary

* Hadoop Cluster orchestration with Docker
* Wishlist for an ideal orchestration manager
    * Add/remove a host from a list of peers
    * Track resource consumption/availability per host
    * Library of pre-prepared application images
    * Multi-tenant protection
        * Performance isolation and data security
        * Containers are less secure than virtual machines
        * Quotas and priority
    * Matching containers with physical hosts
    * Spreading containers across fault-zones
    * Packing containers in contiguous memory
* Migrations vs stateless servers
* Circumventing docker and doing IP address allocation
* DNS settings for bidirectional Hadoop communication
* Wishlist for storage
    * Docker volumes
    * Volumes for fragments of HDFS are hard to reconstruct on container resurrection
    * Ship your data to another db, or paravirtualize the storage
* Solutions?
    * There is not yet a turn-key orchestration system that solves all the items in the wishlists
    * The area is rapidly evolving
