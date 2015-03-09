---
title: Better Tweets Through Data Science
---

![bird words](/images/bird-word.png)

Great tweets aren't born; they're made. It turns out there are
practical steps anyone can take to give their tweets a boost. I'm
going to share lessons I learned applying data science to Twitter
to discover what sets the top performing tweets apart from the
un-favorited masses. This article is basically a cookbook you can
use to construct some tasty tweets no matter what their topic.

First things first. To have any chance at creating top tweets you
need to start with interesting and useful content. Sorry, no amount
of code or statistics can make up for a fundamentally boring article.

Let's begin by assuming you have created or discovered something
great to share. It is probably interesting for quite a few reasons.
The trick is to recognize *which* reason most interests other people.
If you emphasize the unpopular aspects of content  it will appear
that nobody is listening. In fact the first step is to do some
listening yourself.

You certainly have some intuitions of what interests others, but
we can go beyond fuzzy intuition and tap into that huge record that
is the internet.

<div class="alert alert-info" role="alert">
<h4>
Observation One
</h4>
What interests a large group of people today is what interested
them yesterday.
</div>

It's our job to take the record of interest -- the favorites, the
retweets -- and distill their topics. Not only the literal subjects
under consideration but the psychological needs of the audience.
This becomes more straightforward using the right algorithms.

### Finding Topics

The first and most blunt tool for exploring topics is obviously the
hashtag. They're great both because people use them to self-identify
topics and because there are existing tools online to judge hashtag
popularity and relatedness.

Let's get started, try this along with me. Pick a broad (unspecific)
hashtag related to what you want to share. I'll pick the super
generic tag #data. We want to find other tags that are fairly popular
and describe our content more precisely. To explore this use
hashtagify.me

![Related hashtags](/images/hashtag-graph.png)

Sometimes you'll find that a tag isn't used the way you expected.
For instance I found that #cluster has more to do with jewelry than
it does with cloud computing.

The #data graph above shows that we might want to investigate
 #BigData, or #privacy instead. But for this example let's stick
with #data. To really get to know the personality of this hashtag
we'll need data. So visit https://twitter.com/search-home and try
 #data (or your own choice of tag).

You'll notice there is "Top" or "All" mode. We want to stay within
Top because we want to learn about winners, not the others. Now we
could use the Twitter API to programmatically download a bunch of
data but we would have to sign up for access keys etc.

Let's just do it the quick and easy way for now. Start scrolling
down the search page causing new results to be loaded. Just keep
scrolling for a long while until you have many pages of info.

Now use a quick snippet of JavaScript to grab the tweet text for
processing. Pop open the JavaScript console in your browser
(Cmd-Option-J in Chrome). Paste this in and press Enter:

```javascript
$('.tweet-text').map(function () {
  return $.trim($(this).text());
}).get().join("\n")
```

### Revealing Patterns

Select all the output and save it to a file. We're going to extract
meaning from this text. Remember that we want the essential truth
contained inside so we need to remove words which would distract
from or skew the essence. Save and run the following shell script
on your file to remove urls, competing hash tags, and @-mentions.

```bash
perl -p -i -e "s/@\\w+[ ,\\.\\?\":']//g" $1
perl -p -i -e 's/@\w+$//g' $1

perl -p -i -e 's/#\w+[ ,\.\?":]//g' $1
perl -p -i -e 's/#\w+$//g' $1

perl -p -i -e 's/pic.twitter[^ ]+ //g' $1
perl -p -i -e 's/pic.twitter[^ ]+$//g' $1

perl -p -i -e 's/https?:[^ ]+ //g' $1
perl -p -i -e 's/https?:[^ ]+$//g' $1
```

We'll use the R programming language to run topic detection through
Latent Dirichlet Allocation (LDA). Here are the pieces of code to
calculate them.

First load the tweets, one on each line. They will each be considered
small "documents" for topic detection, and are put together into a
"corpus."

```r
library(jsonlite)
library(tm)

tweets <- strsplit(readLines('/path/to/tweets.txt'), "\n")
corp <- Corpus(VectorSource(tweets))
```

To minimize inconsequential variations we will remove stop words,
small words and other detritus. The remaining words are stemmed.

```r
dtm.control <- list(tolower = TRUE,
                    removePunctuation = TRUE,
                    removeNumbers = TRUE,
                    stopwords = c(stopwords("SMART"),
                                  stopwords("en")),
                    stemming = TRUE,
                    wordLengths = c(3, "inf"),
                    weighting = weightTf)
```

Next we record the term occurrences per document in a sparse matrix
and set up parameters. This is a "bag-of-words" approach that ignores
the grammar of sentences.

```r
sparse.dtm <- DocumentTermMatrix(corp, control = dtm.control)
dtm <- as.matrix(sparse.dtm)
class(dtm) <- "integer"

vocab <- sparse.dtm$dimnames$Terms
# Compute some statistics related to the data set:
D <- length(corp)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- rowSums(dtm)  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- colSums(dtm)  # frequencies of terms in the corpus

# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02
```

Using the term frequencies we attempt to find clusters or words
that tend to co-occur. This is the part that really identifies
topics in tweets.

```r
# Fit the model:
library(lda)

lda.input <- lapply(1:nrow(dtm), function (i) {
    docfreq <- t(dtm[i,])
    keepers <- docfreq > 0
    rbind( (0:(ncol(dtm)-1))[keepers], t(dtm[i,])[keepers] )
  } )

fit <- lda.collapsed.gibbs.sampler(documents = lda.input,
                                   K = K, vocab = vocab,
                                   num.iterations = G, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
```

Finally plug the topic model into an interactive visualization that
we will examine with our human intuition.

```r
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = phi,
                   theta = theta,
                   doc.length = doc.length,
                   vocab = vocab,
                   term.frequency = term.frequency)

serVis(json)
```

Time for the fun. Here's a screenshot of the topics detected in top
tweets having the #data hashtag. I have selected topic number 19
to see the frequency of the words inside. Notice I set the relevance
metric slider low to focus on words that are frequent exclusively
in the topic rather than frequent in the corpus as a whole.

![LDA topics](/images/tweet-topics.png)

### Extracting Meaning

The topics can be strange. If they are too chaotic and strange it
probably means the data was not sufficiently cleaned or the sample
was too small. But the more you look coherent categories the more
you'll see a kind of alien logic at work picking out words with a
psychological bond.

At this point the algorithms have done their job and it is time for
us to be creative as humans. Let me walk you through how I made
sense of the topics for the #data hashtag.

For each detected topic I'll list the main word, the feelings I
associate with the topic, and the other major words inside that
guided my assessment.

* The first (TRIAL) topic is cajoling people to try data products. It emphasizes the ease and quickness, and how the solution will "unlock" things, make you smarter, and amaze your boss.
    *  storage, team, record, train, unlock, sport, free, clean, bottom, device, amazing, quick, boss, smarter
* Then we have the (CAPITAL) topic, all about disrupting  industries and big company speculation. 
    *  capital, disrupt, venture, checkout, seeker, relationship, patent, airline, google, bet, fresh, double, fund
* The (BITCOIN) topic deals with that currency but also with non-monetary communication
    *  communication, blah, exvers, search, speak, link, bitcoin, block, discount, listen, word, library, chain, song, teach, topic, print
* People worry about data, and the (BREACH) topic is full of credit cards, security compromises, and how much it costs us all.
    *  credit, hit, compromise, driver, uber, interpret, human, card, replace, commission, hardware, neutral, pool, cost
* Of course you can't have #data without (BIG). Apparently we're "drowning" in it and it's a veritable "lake," but "wow" it is so big.
    *  industry, lake, reach, love, move, drown, session, loyalty, wow
* The (OPPORTUNITY) topic deals with vision and red-hot engineers in the valley. You can not only wrangle data, you can be a data king.
    *  location-based, senior, apply, hadoop, develop, succeed, hire, engineer, need, wrangle, vision, creator, gather, king, cluster, kingdom, hot, always, valley, red
* Then we have (BRANDS) and their associated trust words. People want to rate them and make decisions.
    *  rate, trust, begin, smart, comfort, brain, decision, healthcare, focus, true, specific, rapid, crime, machine, vital, care, attribute, past, player, influence, patient
* Whereas opportunity was about creative struggle and power, (ADVANTAGE) is more about reassurance. A.k.a. debunking myths, providing answers, and strategizing.
    *  advertis, myth, valuable, password, fuel, answer, data-driven, tip, like, competitive, autom, raw, break, provid, deliv, strategi, integr, analysi, format
* Beyond the technical aspects we have (LEGAL) implications. Laws passed or killed, policities about encryption and telecommunications.
    *  phone, law, talktalk, combine, benefit, energi, save, stolen, plan, summari, encrypt, communicate, kill, pass, appeal, backup, transit, fall, bus, attack

<div class="alert alert-info" role="alert">
<h4>
Observation Two
</h4>
Topics generated by LDA reveal strange, sometimes psychological
connections in tweets.
</div>

Is this subjective? Very much so. But I am searching for archetypal
tweet structures, and some topics can be fairly conclusive. I have
run this process on several hashtags and can definitely sense their
distinct personalities.

For instance, check out the related tag #datascience. It should be
 #data's close cousin but it does reveal that there is an industry
of teaching data science. It is career-oriented.

* The (HIRE) shows people want to work in the field but are overwhelmed and turn to instructors.
    *  recruit, rocket, role, survey, rocket, bank, overwhelm, instructor, begin, bet, billion
* More specific (RESOURCES)
    *  bay, behavior, education, video, school, bootcamp, hot, write
* This topic, (MAKE), is similar to the OPPORTUNITY topic in #data but is less power-oriented. It tells a story of a journey to a beautiful outcome.
    *  journey, digital, consider, transform, data-driven, easier, qualiti, visualis, metadata, simpli, measur, beauti, outcome, effect
* Welcome to the breathless (FUTURE) where things are emerging, things to watch
    *  social, impact, emerging, tech, database, past, watch, attract, consum, answer, chicken&egg, law
* Data science has well paying (COMPETITIONS)
    *  learn, kaggle, spend, guest, call, competition, firm, persuade, architect, rule, half, competitor
* Sure these fancy science topics sound great, but show me a  practical (APPLICATION).
    *  develop, find, practical, succeed, economi, attend, webinar, cookbook
* Apparently (MACHINE LEARNING) is where the hard math lives 
    *  math, hard, google, library, word, oxford, advanced
* What is the biggest (STORY) of the week that you don't want to miss?
    *  week, news, bottleneck, back, biggest, tweet, reason, miss, tell, home, demand
* Statisticians explain (HISTORY) with infographics
    *  integral, infographic, statistician, api, century, automate, count, sexiest
* Not just a (PATTERN) but a deep one, found faster, bigger and smarter. Boom.
    *  program, visual, deep, language, beginn, random, linear, regress, faster, explore, bigger, smarter, storytell, connect, generate
* I call it the (ANALYTICS) topic but you might also call it the magic topic. Realtime things for experts. It tracks stuff and never stops.
    *  realtime, message, queue, test, predict, track, influence, expert, review, embrace, magic

### Crafting the Tweets

So we found the main psychological topics in top tweets from our
categories. How can we construct new irresistably sharable tweets?
Use the topic words as your guide. Let's think about this article
itself. We could consider it in the cringe-worthy genre of social
media marketing techniques and learn more about those hashtags, but
let's think of it as #datascience and #data.

This article does deal with making things (tweets) with an anticipated
outcome, so let's cast it as a MAKE topic. Review the words in the
topic again to see how I used them.

> My #datadriven journey to measure and transform my tweets

It is also an APPLICATION of a particular algorithm, so dressing
it in those clothes we get something like

> A cookbook for practical #datascience to make your tweets succeed

But it provides OPPORTUNITIES to improve, hence

> How to create successful tweets by wrangling Twitter #data with #textmining

Of course we're dealing with PATTERNS and we can mimic that topic
with something like,

> Exploring deep language patterns to tweet smarter (#datascience)

### Conclusion

We've seen how to explore the space of hashtags, find topics in
each one, and use our intuition to find psychological motives in
topics. We then used the topic words as inspiration for how to
communicate with others in the language that has proven most
effective.

Certainly a bag-of-words approach such as we've outlined is limited,
and leaning on it for every tweet will probably just sound generic.
Ultimately Twitter is a place to have normal conversations with
people. However I have found that algorithmically analyzing topics
helps me see other people's point of view and write more compelling
content.
