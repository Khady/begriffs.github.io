---
title: A Survey of Data Science
---

What does it take to be an outstanding data scientist? I decided
to find out and reviewed syllabi from every data science bootcamp,
the requirements for dozens of data jobs, and scores of slideshares
and wikipedia articles. I think I've identified and classified the
core skills.

Fundamentally there are six abilities of a competent data scientist.

* acquiring data in a highly-available distributed pipeline
* cleaning it of duplication and errors
* enhancing it with knowledge gained from unsupervised learning and nlp
* inferring properties of a population from a sample, and
  finding relations of statistical variables
* predicting future events through supervised learning and
  statistical models
* communicating my findings to people and external programs

As many articles mentioned, dealing with data is a team job and no
single individual is expected to know everything. There are two
distinct specializations though, data *science* and *engineering*.
The latter deails with pipelines, wrangling, availability, and
cluster management. The former with asking the right questions,
designing experiments, and statistical analysis.

I compiled a list of specific technologies and techniques and fit
them into the sixfold classification of abilities. These appear to
be the core skills of the profession.

* Acquire
    - workers and other pieces are managed by mesos
    - web scrapers and other events as kafka producers
    - mesos runs them in docker containers with the marathon framework
    - feeds into a "lambda architecture"
        - the master dataset accumulates immutably in hdfs
            - encode the data with the thrift format to add a schema
            - thence through hadoop jobs to make precomputed views
            - store views in elephantdb and recalculate periodically
            - client access is read-only from elephant
        - data forks into storm for real time "speed layer" processing
            - which redundantly calculates the views we are doing on hadoop
            - the results go into cassandra which is read/write
            - the realtime layer relies on incremental algorithms to update
              the state in that database
* Clean
    - record linking and deduping
        - soundex
        - jaro–winkler distance
        - unicode normalization
    - outlier identification
        - normality testing
        - Grubbs' test for outliers (given normality)
    - correcting errors with mechanical turk
    - create a derived hdfs dataset, the "golden master"
* Enhance
    - feature extraction
        - principal component analysis
        - k-NN
    - unsupervised clustering
        - k-means, hierarchical (agglomerative, divisive)
        - bagging
    - nlp
        - stemming
        - bag of words
        - tf–idf
        - topic models
* Infer
    - point estimation
        - maximum likelihood
    - interval estimation
        - credible intervals
    - hypothesis testing
        - chi squared
        - t-test
    - correlation
* Predict
    - classification
        - logistic regression
        - decision trees and random forests
        - naive bayes
        - variance-bias decomposition
    - recommendations
        - collaborative filtering
        - content filtering
* Communicate
    - descriptive statistics
        - measures of central tendency
        - measures of dispersion
    - visualization
        - d3 (vega, rickshaw)
        - realtime dashboard
    - exposing results in restful api
        - export to postgres with sqoop
        - generate api with dbapi
        - document with raml

This seems like the state of the art although there is some dissenting
opinion such as [Questioning the Lambda
Architecture](http://radar.oreilly.com/2014/07/questioning-the-lambda-architecture.html).
Ultimately I think the engineering side will be moving to a purely
functional paradigm. Perhaps it could be written in Haskell, using
libraries like [hailstorm](https://hailstorm-hs.github.io/hailstorm/),
[haskakafka](https://github.com/cosbynator/haskakafka), and
[hadron](https://github.com/Soostone/hadron). Everything I read
agreed that Hadoop and associated Java tools are creaky beasts that
require a lot of fiddling.

Although the engineering side looks exhilerating (unleashing hordes
of data scrapers and coordinating a big pipeline), the science side
may be a calmer career bet with its longer-lasting knowledge.
However even there I discovered some fundamental disagreements
between Frequentists and Bayesians. Either way it's all fascinating
stuff and I'd like to create some test compute clusters to up my
game.
