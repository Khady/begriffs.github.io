---
title: To Stalk a Muni
---

![Bus Prediction LED](/images/bus-led.png)

You're waiting for the bus. The prediction monitor at the stop says
five minutes, four, three... then just as you're getting excited
it changes its estimation to half an hour. Perhaps it's raining and
dusk and everyone is getting angry. Why are you standing here, why
did the prediction API tell everyone it was time to get out to the
bus stop? Can predictions be improved?

I've decided to find out. The first step to more accurate muni
predictions is to accumulate historical data. With weeks' worth of
detailed bus position data we can find the patterns which cause
errors in the traditional prediction algorithm.

Happily the realtime position and prediction API is
[available](https://www.nextbus.com/xmlFeedDocs/NextBusXMLFeed.pdf)
for free and without so much as a consumer key. The historical
information is not available but we can write some code to collect
it.

Here's where Amazon EC2 is our friend. We can spin up a micro
instance, set a cron task, and then relax as it does the work. An
easy and reproducible way to build an EC2 machine for the job is
to use [Packer](https://packer.io/) and [Chef](https://www.chef.io/chef/).
We want our machine to hit the Nextbus API on a cron and save the
results to S3 for later processing. That means we have to install
the aws command line tool for S3 uploading, include our script and
set the cron. I've taken care of
all this at [begriffs/stalk27](https://github.com/begriffs/stalk27)
and the repo includes instructions to deploy so you can try it yourself.

My data collection philosophy is to save as much information as
possible when sampling the API. Rather than picking relevant parts
out of the XML document I've opted to save the whole thing. In fact
I save the entire HTTP response header payload along with the
document body. I think of data collection like a Mars mission --
we can never go back in time with this data source so we should err
on the side of over-collecting.

The next consideration is to choose dumb reliable storage and
decouple the storage from the worker machine. Rather than save into
a fancy database I just create a suitably-named file for each
response. Also I avoid the local filesystem. The output is independent
from the lifespan of the worker. While the worker runs it collects,
else the data merely persists.

The only missing piece is monitoring and alerting. If this project
were crucial and truly intolerant of missing data it should have
some failover, redundancy and monitoring. For those kind of
requirements I would choose
[begriffs/microservice-template](https://github.com/begriffs/microservice-template)
and enable Amazon CloudWatch.

After two weeks I will begin analysis and will share with you what
the data reveals about bus delays. I am collecting data on route
27 because I've had bad experiences waiting for wildly late buses
at Powell station. If the new prediction algorithm ends up working
well then I'll scale up the data collection to more routes and maybe
create a web interface for other people to check accurate departure
times.
