---
title: "Average and top entry scores for the first 25 gameweeks"
author: "Rasmus Wiscostretford"
date: "2019-03-18"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

Suppose we are interested in tracking the average and top FPL scores by gameweek?

get_round_info(1:25)

First we fetch the round information for a selection of gameweeks, say 1-25, using the get_round_info function:


```r
library(fplscrapR)

df <- get_round_info(1:25)
```

Next we use dplyr, tidyr and ggplot2 to transform and plot the data, showing the average and top entry progression by gameweek:


```r
library(dplyr)
library(tidyr)
library(ggplot2)
```


```r
df %>% 
  select(id,average_entry_score,highest_score) %>% # selecting the relevant columns
  gather("var","value",-id) %>% # transforming from wide to long format for ggplot
  ggplot() +
    geom_line(aes(x=id,y=value,colour=var),size=1) +
    theme_bw() +
    scale_x_continuous(breaks=1:25) +
    labs(x="Gameweek",y="Score",title="Average and top FPL entry scores by gameweek",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))
```

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpIHlAPh\preview-252c6dcd431e.dir\entryscores_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
