---
title: How Transparent Encryption Works in HDFS
video: true
---

[Charles Lamb](https://www.linkedin.com/pub/charles-lamb/0/49a/694),
software enginner at
[Cloudera](http://www.cloudera.com/content/cloudera/en/home.html),
describes the tradeoffs between various levels of encryption, the
choices he made when designing transparent encryption in HDFS, and
the concepts you need to understand to use it.


<div class="flowplayer" data-embed="false">
  <video type="video/mp4"
         src="http://player.vimeo.com/external/120274127.hd.mp4?s=3d80990f0d19452f2133316671a594f2"
         poster="https://i.vimeocdn.com/video/508061204.jpg?mw=700"
  ></video>
</div>

### Summary

* Transparent encryption: data is read and written to an encrypted subtree on HDFS
* This helps helps applications be regulation-compliant
* Encryption/decryption is always handled by the client, HDFS itself never sees plaintext
* The levels of encryption:
    * Application (hard to do and to add to legacy apps)
    * Database (prone to leaks through e.g. secondary indices)
    * Filesystem (higher performance, transparent, less flexible for various tenants)
    * Disk level (only protects against physical theft)
* HDFS transparent encryption lives somewhere between db and filesystem levels
* Design goals
* In-depth explanations of architectural concepts
    * Key-management server
    * Encryption zones
    * Keys
* HDFS encryption configuration
* Per-user and per-key ACLs
* Performance results
