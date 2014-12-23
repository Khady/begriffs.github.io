---
title: Datacenter to AWS Cloud Migration
video: true
---

[Theo Kim](https://twitter.com/theotypes), senior director of SaaS
at [Jobvite](http://www.jobvite.com/) describes the experience of
migrating the entire company infrastructure from a dedicated data
center to Amazon Web Services.

He is joined onstage by [Scott Gust](https://plus.google.com/+ScottGust/posts)
and [Brian Morehead](https://plus.google.com/108124405312975902364/posts) to
describe Jobvite's three biggest migration challenges and their solutions.

<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/113586351.hd.mp4?s=fc198dd18083a28ef5d171eef732c10f"
         poster="https://i.vimeocdn.com/video/499956612.png?mw=700"
  ></video>
</div>

### Overview

#### Theo Kim

* The pain of the old data center model
* Thinking of services rather than servers
* Recap of gains with AWS
* Cutting costs with reserve- and spot-instances

#### Scott Gust

* Three main architectural challenges
     * uri load balancing
     * autoscaling adoption
     * network attached storage migration
* Way to work around the elastic load balancer’s uri support
     * Used HAProxy behind ELB to do the matching
     * Important to insulate IP changes caused by ELB
* Injecting headers with the load balancer
* As servers become available they recreate the HAProxy config

#### Brian Morehead

* The migration was fun!
* Autoscaling made them reconsider how to automate datacenter processes
* Moving beyond the problem of the “snowflake server”
* Config management
     * All ec2 instances are constructed from a repo in s3
     * Chose puppet over chef or ansible
     * Standardizing hostname
     * LDAP auth
* Puppet installs a cron job to register machines with HAProxy
* Monitoring with Nagios and CheckMK
* NAS migration
     * At the time of migration they had 8TB of storage and 40M+ files (resumes, attachments etc)
     * How about S3? Not really a NAS, and no metadata
     * They chose Zadara Storage to avoid stitching together EBS volumes
     * But they’re moving to an all-s3 solution
* Copying files to s3 can be slow
     * Even multithreaded copies and bigger instances didn’t help
     * Solution: spot instances
     * Spun up 15-20 instances and it blasted the 40M files copied in three days

#### Theo

* Results
* Ended up with a 20% cost savings by moving off a managed service provider
* Great uptime
* Fast patches to security vulnerabilities
* Gets to have Christmas and Thanksgiving with the family (!)
* Ephemeral servers to do quick tests (load etc)
* Cheap services
     * sending 275000 emails per day for $150 per month
* Mountains of metrics from CloudWatch and CloudTrail
* Q&A
     * Network attached storage…don’t use it!
     * Stop worrying about encryption-at-rest in the cloud
     * Data centers have a “red zone” where equipment coming out literally goes through a wood chipper
     * Intentionally undersize staging resources to force devs to make leaner code (heh heh)
