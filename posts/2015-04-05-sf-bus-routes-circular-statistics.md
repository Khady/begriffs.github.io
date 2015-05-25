---
title: Circular Statistics of SF Bus Routes
video: true
---

[Eric Theise](http://erictheise.com/), Senior Software Engineer at
[RepairPal](http://repairpal.com/) asks, "Which way is Inbound?"
Though SF Muni routes are prefaced with the modifiers "Inbound" and
"Outbound", their use of the terms often has little to do with the
commonly understood meaning. In this presentation Eric uses open
data, an open source geostack, and R's circular package to visually
and statistically analyze and discuss the resulting cognitive
dissonance.

<div class="flowplayer" data-embed="false">
  <video type="video/mp4"
         src="http://player.vimeo.com/external/124132910.hd.mp4?s=f047700972c48aaaf9c1e0c47a848240"
         poster="https://i.vimeocdn.com/video/513748329.jpg?mw=700"
  ></video>
</div>

### Summary

* Overview of the San Francisco Muni - good coverage, bad timing
* All routes are couched in terms of “inbound” and “outbound” which
  can be counterintuitive in curvy areas of some routes
* The origin of the term "inbound" in navigation, and some San
  Francisco history
* Quantifying the notion of “inbound”
    * we’ll attack the problem with statistical tools
    * mean direction, circular variance, and hypothesis testing
* Circular statistics avoids ambiguity in histograms that “wrap-around”
  where they can appear either bimodal or unimodal depending on where
  the wrapping point occurs
* Two packages in R for this: circular and CircStates
    * Eric does not know of any python or javascript packages to do this kind of thing
    * He is working on [mctad.js](Sr Engineer, RepairPal) to address this
* A great resource to learn more is the book [Circular Statistics
  in R](http://circstatinr.st-andrews.ac.uk/)
* To analyze the concept of inbound and outbound we need route information
    * Eric got the routes from the nextbus API
    * Stored it in PostgreSQL with the PostGIS extension
* What is the true geospatial centroid of downtown?
    * Turns out it’s the Mechanic’s Monument, created by “the Michelangelo of the West”
* Demo
    * Filter routes, and examine each stop
    * Find mean direction and variance, stop by stop
    * Use hypothesis testing to see if in general the bus route has
      a straightforward labeling of inbound and outbound
    * Outbound is less defined than inbound as a place (it’s everywhere NOT inbound)
* For more info about Bay Area pedestrian and transit history see
  the maps of [Eric Fisher](https://twitter.com/enf)
