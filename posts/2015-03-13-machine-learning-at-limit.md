---
title: Machine Learning at the Limit
video: true
---

Professor [John Canny](http://www.cs.berkeley.edu/~jfc/) presents
benchmarks for the [BIDMach](https://github.com/BIDData/BIDMach)
Scala machine learning library. He and his team achieve a nearly
theoretical maximum speed in this library relative to roofline
analysis of the available CPU and GPU resources. They have managed
to outperform entire machine learning clusters using a single
computer. Check out the video below for more detailed comparisons.

<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/122075036.hd.mp4?s=c560edc9e62183abe4542a1f8b7a9bd2"
         poster="https://i.vimeocdn.com/video/510970022.png?mw=700"
  ></video>
</div>
This talk was delivered at [Alpine Data Lab](http://alpinenow.com/).

<a class="embedly-card" href="http://www.slideshare.net/ChesterChen/sf-big-analytics">Slides</a>
<script async src="//cdn.embedly.com/widgets/platform.js" charset="UTF-8"></script>

### Summary

* Discovering the limits of machine learning performance using
  roofline design from computer architecture
* Taking fully accelerated single-node algorithms and scaling them
  up
* Tweaking a data model for machine learning should be quick and
  iterative
* Often prototype machine learning is done in Scikit-Learn or R.
  It is then translated for production code by a group that often
  lacks machine learning expertise, which degrades the quality of
  results.
* The reasons for developing a new ML toolkit (Bidmach):
    * GPUS now operate efficiently on sparse data
    * minibatch and stochastic gradient descent habe become a
      workhorse of choice for big data processing
    * easy customization of models
    * quick exploration, and sharing code between prototype and production
* To design the framework we start with roofline design and
  articulating the limits of CPUs etc
* We build the high level operations on top of a GPU-accelerated
  matrix library
* Roofline design establishes fundamental performance limits for a
  computational kernel
* Comparison of the strengths of CPUs and GPUs
* Comparing performance of BIDMach vs Vowpal Wabbit vs Scikit-Learn
* For many tasks a single node running BIDMach outperforms a whole
  cluster running traditional libraries
* Measurements of running time for Latent Dirichlet Allocation,
  random forests, single-class regression, logistic regression,
  multilabel regression, factor models, SVMs and clustering
* Rooflining network bandwidth
