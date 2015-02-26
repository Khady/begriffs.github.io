---
title: Text Mining in R (Sentiment Analysis, LDA, and Syuzhet)
video: true
---

[Harold Baize](https://www.linkedin.com/pub/harold-baize/4/480/529),
researcher at the [San Francisco Department of Public
Health](https://www.sfdph.org/dph/default.asp) shows how to use the
latest [R](http://www.r-project.org/) packages to analyze sentiments
and topics in text. He gives a demo of using mental health provider
notes to assess the effectiveness of treatments.  Along the way he
shows how disciplines from psychology to literary analysis have
pioneered the practices labeled under the umbrella of "text mining."


<div class="flowplayer" data-embed="false">
  <video src="http://player.vimeo.com/external/120614691.hd.mp4?s=d86271a555347fc538b2084083ef16bf"
         poster="https://i.vimeocdn.com/video/508549850.png?mw=700"
  ></video>
</div>

### Summary

* Origins of “text mining” in the “content analysis” of psychology
* We’ll be looking at sentiment analysis of content modeling
* Both are bag-of-words approaches
* Facebook’s quasi-unethical emotion manipulation experiment using sentiment analysis
* Great R packages: tm, topicmodels, LDAvis, syzuhet
* Definition of terms used
* Doing some actual text mining
    * Begin with a document term matrix
    * Built using “controls” like stop words, stemming etc
    * Then merge the term matrix with the lexicon of emotional values
    * There are some common sentiment lexicons ready made: ANEW, Afinn
* Sentiment analysis can go beyond merely positive/negative
    * We can record the holder, target, sentiment, polarity, and source of a corpus
* Beware that bag-of-words approach misses things like sarcasm, double entendre and negation
* A walkthrough of analyzing the sentiment of a psychological health review shows sometimes we need to modify an off the shelf lexicon.
* Comparing sentiment in this study
    * Visualizing sentiment scores on psych progress notes to compare different hospitals
    * Different ethnicities show show differences in positivity
    * Also little kids/toddlers are way more optimistic, then get steadily more negative toward middle age. Finally above age 60 people tend to get positive again, at the level of 5-12 year olds.
* Predictions using sentiment data
* Topic modeling
    * An unsupervised method to find latent topics
    * LDA (latent dirichlet allocation) is a popular form of topic modeling
    * It returns the probability that each word is found in each topic
    * We can visualize the results with LDAvis
* Example of doing LDA
    * Configure params like burn-in, iterations, number of topics, method of sampling and how frequently to keep samples
    * Sometimes the topics are difficult to interpret, and this can be exacerbated by poor data cleaning
* Demo of exploring the output of LDA
    * Adjusting relevance vs frequency in corpus or topic
    * The results are probabilistic, so each run produces different topics, but some of them persist
* Predicting topics
    * For instance a linear model reveals topics associated with newcomers to psychiatry vs old timers
* Concepts from literary analysis have been encoded into algorithms to help us understand a story
    * syuzhet vs fabula
    * A video that demonstrates the syuzhet of fictional story arcs
* Applying the syuzhet library in R
    * The Canadian National Research Council has created a rich measure of syuzhet along ten dimension of emotion, so you can watch e.g. disgust or anticipation change over time
* There is also a supervised version of LDA (SLDA) but it is not covered in this talk
