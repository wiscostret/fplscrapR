---
title: "Mohamed Salah's points in each gameweek for the first 25 gameweeks"
author: "Rasmus Wiscostretford"
date: "2019-03-18"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

Suppose we are interested in the progression and variance of Mohamed Salah's FPL score in each gameweek? Here, we take the first 25 gameweeks of the season as an example.

First we fetch the gameweek-by-gameweek details of the player using get_player_details, mobilising the playerid using get_player_id:


```r
library(fplscrapR)

df <- get_player_details(get_player_id("Mohamed Salah")$id)
```

Next we use dplyr and ggplot2 to transform and plot the data, showing Salah's round score for each gameweek:


```r
library(dplyr)
library(ggplot2)
```


```r
df %>% 
  filter(round %in% 1:25) %>% # filtering for the GWs we are interested in
  select(round,total_points) %>% # selecting the relevant columns
  ggplot() + # plotting with ggplot2
    geom_col(aes(x=round,y=total_points),fill="red",size=1) +
    theme_bw() +
    scale_x_continuous(breaks=1:25) +
    labs(x="Gameweek",y="Round score",title="Salah's round score by gameweek",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))
```

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpIHlAPh\preview-252c5d2b4bd8.dir\salahpoints_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
