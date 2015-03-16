---
title: Tracking Joy at Work
---

You can't improve what you can't measure, and what better to measure
than an activity that consumes your waking life. Work.

Work, that oft-maligned but secretly important part of having meaning
in life. Why are some days frustrating and others swept in effortless
flow? Are bad days shared among an entire office or suffered
individually?

I've created a program to help answer this question. Throughout the
day at random intervals it privately sends me and my coworkers at
[Loop/Recur](http://looprecur.com) messages on Slack to see how
we're feeling. We respond on a scale of one to five and the results
are displayed anonymously on a realtime dashboard.

![happiness dashboard](/images/happiness-dashboard.png)

Just asking this basic question reveals a lot and has other good
side effects. Its simplest effect is to make us reflect on our mood.
Quantifying how we feel raises us above inarticulate reaction. Also
we can spot bad moods that we all share because they appear on the
dashboard. It sparks helpful discussions.

The technique of sampling moods or other subjective reports over
time is called the Experience Sampling Method and has been done
since the 1970s. It's been getting easier to conduct this research
as communication and data collection become more accessible to
computers. Not so long ago pagers were the best way to alert people
to respond.

While developing the program I consulted a fascinating book called,
"Experience Sampling Method: Measuring the Quality of Everyday
Life." I wanted to be acquainted with the methodology behind
these kind of experiments. Whether or not you're interested in data
science I'd recommend you check this book out. It's full of studies
that talk about things that are fundamental to human happiness,
from family life to work to gender differences.
<img src="/images/experience-sampling.jpeg" style="float:right" />

The three common types of sampling are event contingent, interval
contingent, and signal contingent. The first is where the participant
makes reports during a certain type of event. At our software company
it might be after committing code to git, or coming back from lunch.
The second is when the participant makes recordings at the end of
large intervals, like at the end of the day. We chose the last
method, signal contingency.

Signal contingent sampling is where participants get random prompts
to record data. It helps avoid memory biases and measures all
computer-based activities without preference.

To run the system we have a daily scheduler. It picks three random
times from a uniform distribution inside each user's time preference
window (the hours during which we each prefer to be solicited.)

The randomness of the sampling is interesting. Sometimes I may get
solicited twice within a few minutes, other times more evenly
throughout the day. We have considered enforcing a more even
solicitation schedule (making it impossible that they occur too
close together) but we don't have a clear justification for the
change. For the time being we're leaving it as a uniform, sometimes
lumpy, distribution.

We don't yet have enough data to make statistically significant
claims about how we feel, but it has sure been fun keeping an eye
on the dashboard. Many companies have big screens showing web
analytics, but we're the first I've seen to track how we feel --
and it feels good.
