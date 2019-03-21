Most expensive teams by average FPL player cost
-----------------------------------------------

Suppose we are interested in comparing which Premier League teams have
the most expensive FPL players on average?

First we fetch the full summary data on all FPL players using the
get\_player\_info function:

    library(fplscrapR)

    df <- get_player_info()

Next we use dplyr and ggplot2 to transform and plot the data, ranking
each Premier League by its average FPL player cost:

    library(dplyr)
    library(ggplot2)

    df %>% 
      select(id, team, now_cost) %>% # selecting the relevant columns
      mutate(now_cost=now_cost/10) %>% # transforming the cost to millions (for some reason the FPL data structures FPL cost as 100.000s)
      group_by(team) %>% # transformation to group and mean-summarize the costs at the 'team' variable level 
      summarize(mean(now_cost)) %>%
        ggplot() + # plotting using ggplot2
        geom_col(aes(x=reorder(team,-`mean(now_cost)`),y=`mean(now_cost)`),fill="blue") +
        theme_bw() +
        scale_x_discrete(
          labels=c("1"="ARS","2"="BOU","3"="BHA","4"="BUR","5"="CAR","6"="CHE",
            "7"="CRY","8"="EVE","9"="FUL","10"="HUD","11"="LEI","12"="LIV",
            "13"="MCI","14"="MUN","15"="NEW","16"="SOU","17"="TOT","18"="WAT",
            "19"="WHU","20"="WOL")) +
        labs(x="Team",y="Mean player cost (£m)",
          title="Mean FPL player cost for Premier League teams",
          caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-3-1.png)

------------------------------------------------------------------------

Comparing historical FPL total season scores for Eden Hazard and Alexis Sanchez
-------------------------------------------------------------------------------

Suppose we are interested in how the FPL histories of Eden Hazard and
Alexis Sanchez compare?

First we fetch the player histories of those two players using
get\_player\_hist, mobilising their playerids using get\_player\_id:

    library(fplscrapR)

    df <- get_player_hist(playerid=get_player_id(c("Eden Hazard","Alexis Sánchez"))$id)

Next we use dplyr and ggplot2 to transform and plot the data, showing
the total FPL scores by season for the two players across their FPL
careers:

    library(dplyr)
    library(ggplot2)

    df %>% 
      select(playername,season_name,total_points) %>% # selecting the relevant columns
      ggplot() + # plotting using ggplot2
        geom_line(aes(x=season_name,y=total_points,group=playername,colour=playername),size=1) +
        theme_bw() +
        labs(x="Season",y="Total score",title="Historical FPL season scores by Hazard and Sanchez",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-6-1.png)

------------------------------------------------------------------------

Aubameyang ownership and net transfers for the first 25 gameweeks"
------------------------------------------------------------------

Suppose we are interested in how the ownership of Pierre-Emerick
Aubameyang has changed through transfers over the course of the first 25
gameweeks of the year?

First we fetch the gameweek-by-gameweek details of the player using
get\_player\_details, mobilising the playerid using get\_player\_id:

    library(fplscrapR)

    df <- get_player_details(get_player_id("Pierre-Emerick Aubameyang")$id)

Next we use dplyr, tidyr and ggplot2 to transform and plot the data,
showing the total number of owners and net transfers out for each
gameweek:

    library(dplyr)
    library(tidyr)
    library(ggplot2)

    df %>% 
      filter(round %in% 1:25) %>% # filtering for the GWs we are interested in
      select(round,transfers_balance,selected) %>% # selecting the relevant columns
      gather("var","value",-round) %>% # transforming from wide to long format for ggplot
      ggplot() + # plotting using ggplot2
        geom_line(aes(x=round,y=value,group=var,colour=var),size=1) +
        theme_bw() +
        scale_x_continuous(breaks=1:25) +
        labs(x="Gameweek",title="Aubameyang ownership and net transfers",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-9-1.png)

------------------------------------------------------------------------

Mohamed Salah's points in each gameweek for the first 25 gameweeks
------------------------------------------------------------------

Suppose we are interested in the progression and variance of Mohamed
Salah's FPL score in each gameweek? Here, we take the first 25 gameweeks
of the season as an example.

First we fetch the gameweek-by-gameweek details of the player using
get\_player\_details, mobilising the playerid using get\_player\_id:

    library(fplscrapR)

    df <- get_player_details(get_player_id("Mohamed Salah")$id)

Next we use dplyr and ggplot2 to transform and plot the data, showing
Salah's round score for each gameweek:

    library(dplyr)
    library(ggplot2)

    df %>% 
      filter(round %in% 1:25) %>% # filtering for the GWs we are interested in
      select(round,total_points) %>% # selecting the relevant columns
      ggplot() + # plotting with ggplot2
        geom_col(aes(x=round,y=total_points),fill="red",size=1) +
        theme_bw() +
        scale_x_continuous(breaks=1:25) +
        labs(x="Gameweek",y="Round score",title="Salah's round score by gameweek",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-12-1.png)

------------------------------------------------------------------------

List of top performers by FPL goals and assists
-----------------------------------------------

Suppose we are interested in listing the top FPL performers by FPL goals
and assists? Again, we take the first 25 gameweeks of the season as an
example.

First we fetch the gameweek-by-gameweek details of ALL players using
get\_player\_details:

    library(fplscrapR)

    df <- get_player_details() # this may take a while to load as it fetches ALL player details

Next we use dplyr and ggplot2 to transform and plot the data, showing
the top performers:

    library(dplyr)
    library(ggplot2)

    df %>%
      filter(round %in% 1:25) %>% # filtering for the GWs we are interested in
      select(playername,goals_scored,assists) %>% # selecting the relevant columns
      group_by(playername) %>% # transformation to group and summarize the performance at the 'playername' variable level
      summarize_all(sum) %>% 
      mutate("involvements"=goals_scored+assists) %>% # adding a new variable that sums the goals scored and assists
      arrange(-involvements) %>%  # ordering (arranging) the table by top involvements
      slice(1:20) # showing the top20 only

    ## # A tibble: 20 x 4
    ##    playername                goals_scored assists involvements
    ##    <fct>                            <int>   <int>        <int>
    ##  1 Mohamed Salah                       16       8           24
    ##  2 Eden Hazard                         12      10           22
    ##  3 Raheem Sterling                     10      12           22
    ##  4 Sergio Agüero                       14       8           22
    ##  5 Pierre-Emerick Aubameyang           15       6           21
    ##  6 Harry Kane                          14       6           20
    ##  7 Leroy Sané                           8      11           19
    ##  8 Callum Wilson                       10       8           18
    ##  9 Paul Pogba                           9       9           18
    ## 10 Alexandre Lacazette                  9       8           17
    ## 11 Heung-Min Son                       10       7           17
    ## 12 Marcus Rashford                      9       7           16
    ## 13 Raúl Jiménez                         9       7           16
    ## 14 Ryan Fraser                          5       9           14
    ## 15 Roberto Firmino                      9       5           14
    ## 16 Sadio Mané                          11       2           13
    ## 17 Aleksandar Mitrovic                 10       3           13
    ## 18 Gylfi Sigurdsson                     9       3           12
    ## 19 Christian Eriksen                    4       8           12
    ## 20 Richarlison de Andrade              10       2           12

------------------------------------------------------------------------

Building a very simple expected assists (xA) model
--------------------------------------------------

Suppose we are interested in trying to build an 'expected assists' (xA)
model from the FPL data? Again, we take the first 25 gameweeks of the
season as an example.

First we fetch the gameweek-by-gameweek details of ALL players using
get\_player\_details, then create our model dataset by selected
information that could predict assist performance, such as Key Passes
and Open Play Crosses:

    library(fplscrapR)
    library(dplyr)

    df <- get_player_details() # this may take a while to load as it fetches ALL player details

    dfmodel <- df %>% 
      filter(round %in% 1:25) %>% # filtering out the rounds we are interested in
      mutate(potentialassists = key_passes+open_play_crosses) %>% # creating a new variable that give us a potential indicator for expected assists, namely the number of key passes and open play crosses for each player
      select(playername,assists,potentialassists) %>% # selecting the variables we need for our analysis
      group_by(playername) %>% # transformation to group and summarize all our variables (i.e. the actual and potential assists) at the 'playername' variable level
      summarize_all(sum) %>%
      mutate(xA=potentialassists*(sum(assists)/sum(potentialassists))) # creating a new variable called 'xA' that attempts to predict the actual number of assists based on the relationship (from our data) between the number of assists and key passes+open play cross a player has produced

Next we use dplyr and ggplot2 to transform and plot the data, comparing
the actual FPL assists with our 'xA' model predictions. We also use the
'ggrepel' package to avoid cluttering of our plot:

    library(ggplot2)
    library(ggrepel)

    dfmodel %>% 
      ggplot(aes(x=xA,y=assists)) +
      geom_point(alpha=0.5,size=2) +
      theme_bw() +
      labs(x="Expected assists",y="Actual assists",title="Expected vs. actual assists",subtitle="Season 2018/2019 - Gameweeks 1-25", caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
      geom_abline(intercept=0,slope=1,colour="grey",size=0.5) + 
      geom_text_repel(size=3.75,aes(
      label=ifelse(xA>quantile(xA,prob=1-1.5/100),as.character(playername),ifelse(assists>quantile(assists,prob=1-1.5/100),as.character(playername),"")))) + #
      scale_x_continuous(limits=c(0,12),breaks=0:12) +
      scale_y_continuous(limits=c(0,12),breaks=0:12)

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-18-1.png)

Here we can see that there is a significant variation around our
expected assists. Ryan Fraser's ouput is, for instance, well predicted.
Raheem Sterling and Leroy Sanés are not - the model firmly
under-estimates their assists. Meanwhile, the assists of players like
Lucas Digne and Andros Townsend are far over-estimated.

Finally, we test our xA model to see how it fares in explaining the
variation of actual assists, using the summary function from R base:

    summary(lm(dfmodel,formula=xA ~ assists))

    ## 
    ## Call:
    ## lm(formula = xA ~ assists, data = dfmodel)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8918 -0.4299 -0.3289  0.2453  5.1987 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.42995    0.04677   9.192   <2e-16 ***
    ## assists      0.58198    0.02221  26.203   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9964 on 594 degrees of freedom
    ## Multiple R-squared:  0.5361, Adjusted R-squared:  0.5354 
    ## F-statistic: 686.6 on 1 and 594 DF,  p-value: < 2.2e-16

------------------------------------------------------------------------

Creating an 'Alternative FDR' (aFDR) ranking of opponents by FPL points allowed
-------------------------------------------------------------------------------

Suppose now we are interested in trying to create an alternative to
FPL's "FDR" (Fixture Difficulty Ranking) based on actual FPL points
allowed? Again, we take the first 25 gameweeks of the season as an
example.

First we fetch the gameweek-by-gameweek details of ALL players using
get\_player\_details:

    library(fplscrapR)

    df <- get_player_details() # this may take a while to load as it fetches ALL player details

Next we use dplyr and ggplot2 to transform and plot the data, ranking
the teams by FPL points allowed:

    library(dplyr)
    library(ggplot2)

    df %>% 
      filter(round %in% 1:25) %>% # filter out the gameweeks we are interested in
      group_by(opponent_team) %>% # transformation to group and summarize the total_points (scored by a given player in a given round) by the opponent ('opponent_team') variable level
      summarize(sum(total_points)) %>% 
      ggplot(aes(x=reorder(opponent_team,`sum(total_points)`),y=`sum(total_points)`,fill=factor(opponent_team))) + 
        geom_col(colour="black") +
        theme_bw() +
        coord_flip() +
        scale_x_discrete(labels=c("1"="ARS","2"="BOU","3"="BHA","4"="BUR","5"="CAR","6"="CHE","7"="CRY","8"="EVE","9"="FUL","10"="HUD","11"="LEI","12"="LIV","13"="MCI","14"="MUN","15"="NEW","16"="SOU","17"="TOT","18"="WAT","19"="WHU","20"="WOL")) + # here we transform the numbered team labels to the FPL short letter names
        labs(x="Team",y="Total FPL points allowed",title="Alternative FDR ranking - by total FPL points allowed",subtitle="Season 2018/2019 - Gameweeks 1-25",caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
        scale_fill_manual(values=c("1"="#EF0107","2"="#000000","3"="#0057B8","4"="#6C1D45","5"="#0070B5","6"="#034694","7"="#1B458F","8"="#003399","9"="#FFFFFF","10"="#0E63AD","11"="#003090","12"="#C8102E","13"="#6CABDD","14"="#DA291C","15"="#241F20","16"="#D71920","17"="#132257","18"="#FBEE23","19"="#7A263A","20"="#FDB913"),guide=F) # here we add the team-specific colours to the plot

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-22-1.png)

------------------------------------------------------------------------

Creating a simple fixture table
-------------------------------

Suppose now we are interested in creating a simple overview of FPL
fixtures? Again, we take the first 25 gameweeks of the season as an
example.

First we fetch the full fixture list using get\_game\_list:

    library(fplscrapR)

    gamelist <- get_game_list()

FPL only lists the fixtures once, as HOME-AWAY, but we need both
HOME-AWAY and AWAY-HOME to plot the full fixture table. We need to plot
every fixture twice, once as a home game for the one team, and once as
an away game for the other team.

Using R base's rbind and dplyr's mutate (creating new variables) we
essentially just duplicate the fixture table:

    library(dplyr)

    allfixtures <- rbind(
      gamelist %>% mutate(team=home,oppo=away,homeaway="home"),
      gamelist %>% mutate(team=away,oppo=tolower(home),homeaway="away"))

Next we use dplyr and ggplot2 to transform and plot the data, ranking
the teams by FPL points allowed:

    library(ggplot2)

    allfixtures %>% 
      filter(GW %in% 1:25) %>% # filtering for the gameweeks we are interested in
        ggplot() +
          geom_tile(aes(x=GW,y=team),fill="white",colour="lightgrey") +
          geom_text(aes(x=GW,y=team,label=oppo),size=2) +
          theme_void() +
          theme(axis.text = element_text(face = "bold")) +
          theme(axis.text.y = element_text(margin=margin(0,-20,0,0))) + # fixing the margins on the tile
          scale_x_continuous(position="top",breaks=1:25) +
          labs(caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-26-1.png)

------------------------------------------------------------------------

Creating an 'Alternative FDR' (aFDR) fixture table
--------------------------------------------------

Suppose now we are interested in creating an FPL fixture table that
ranks fixtures by the 'Alternative FDR' (aFDR) metric? Again, we take
the first 25 gameweeks of the season as an example.

First we create the aFDR rankings table, by fetching the
gameweek-by-gameweek details of ALL players using get\_player\_details,
and then transforming that using dplyr:

    library(fplscrapR)
    library(dplyr)

    df <- get_player_details() # this may take a while to load as it fetches ALL player details

    afdrranks <- df %>% 
      filter(round %in% 1:25) %>% # filtering for the gameweeks we are interested in.
      group_by(opponent_team) %>% # transformation to group and summarize the total_points (scored by a given player in a given round) by the opponent ('opponent_team') variable level
      summarize(sum(total_points)) %>% 
      arrange(`sum(total_points)`) %>% # ordering (arranging) by total_points allowed
      mutate(oppo=recode(opponent_team,"1"="ARS","2"="BOU","3"="BHA","4"="BUR","5"="CAR","6"="CHE","7"="CRY","8"="EVE","9"="FUL","10"="HUD","11"="LEI","12"="LIV","13"="MCI","14"="MUN","15"="NEW","16"="SOU","17"="TOT","18"="WAT","19"="WHU","20"="WOL")) # here we transform the numbered team labels to the FPL short letter names

Next, we fetch the full fixture list using get\_game\_list:

    gamelist <- get_game_list()

FPL only lists the fixtures once, as HOME-AWAY, but we need both
HOME-AWAY and AWAY-HOME to plot the full fixture table. We need to plot
every fixture twice, once as a home game for the one team, and once as
an away game for the other team.

Using R base's rbind and dplyr's mutate (creating new variables) we
essentially just duplicate the fixture table:

    afdrfixtures <- rbind(
      gamelist %>% mutate(team=home,oppo=away,homeaway="home"),
      gamelist %>% mutate(team=away,oppo=tolower(home),homeaway="away"))

Then we need to add in the aFDR ranking for each fixture, doing so
through a loop (which is decidedly not the most efficient method). For
each row, we identify the data point in the afdrranks data frame that
provides the aFDR ranking for each team in question:

    for (i in 1:nrow(afdrfixtures)){
      afdrfixtures$afdr[i] <- afdrranks$`sum(total_points)`[which(afdrranks$oppo==toupper(afdrfixtures$oppo[i]))]
    }

Next we use dplyr and ggplot2 to plot the data in a fixture table:

    library(ggplot2)

    afdrfixtures %>% 
      filter(GW %in% 1:25) %>% # filtering for the gameweeks we are interested in 
      ggplot() +
        geom_tile(aes(x=GW,y=team,fill=afdr),colour="lightgrey") +
        geom_text(aes(x=GW,y=team,label=oppo),size=2) +
        theme_void() +
        theme(axis.text = element_text(face = "bold")) +
        theme(axis.text.y = element_text(margin=margin(0,-20,0,0))) + # fixing the margins
        scale_x_continuous(position="top",breaks=1:25) +
        labs(caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
        scale_fill_gradient2(guide=F,low="#00FF87",mid="#D6DCD8",high="#7F002D",midpoint=median(afdrfixtures$afdr)) # creating a gradient colour-coding that spans from lowest aFDR ranking (coloured red) to highest (coloured green)

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-31-1.png)

------------------------------------------------------------------------

Creating your own 'FDR' fixture table
-------------------------------------

Suppose now we are interested in creating an FPL fixture table like the
FPL's official 'Fixture Difficulty Ranking'? Again, we take the first 25
gameweeks of the season as an example.

First we fetch the official FDR rankings using get\_fdr:

    library(fplscrapR)

    fdr <- get_fdr()

Next we fetch the full fixture list using get\_game\_list:

    gamelist <- get_game_list()

FPL only lists the fixtures once, as HOME-AWAY, but we need both
HOME-AWAY and AWAY-HOME to plot the full fixture table. We need to plot
every fixture twice, once as a home game for the one team, and once as
an away game for the other team.

Using R base's rbind and dplyr's mutate (creating new variables) we
essentially just duplicate the fixture table:

    library(dplyr)

    fdrfixtures <- rbind(
      gamelist %>% mutate(team=home,oppo=away,homeaway="home"),
      gamelist %>% mutate(team=away,oppo=tolower(home),homeaway="away"))

Then we need to add in the FDR ranking for each fixture, doing so
through a loop (which is decidedly not the most efficient method). For
each row, we identify the data point in the fdr data frame that provides
the FDR ranking for each team in question, depending on whether the
fixture is home or away:

    for (i in 1:nrow(fdrfixtures)){
      ifelse(fdrfixtures$homeaway[i]=="home",
        fdrfixtures$fdr[i] <- fdr$strength_overall_away[which(fdr$short_name==toupper(fdrfixtures$oppo[i]))],
        fdrfixtures$fdr[i] <- fdr$strength_overall_home[which(fdr$short_name==toupper(fdrfixtures$oppo[i]))])
    }

Next we use dplyr and ggplot2 to plot the data in a fixture table:

    library(ggplot2)

    fdrfixtures %>% 
      filter(GW %in% 1:25) %>%  # filtering for the gameweeks we are interested in
        ggplot() +
          geom_tile(aes(x=GW,y=team,fill=fdr),colour="lightgrey") +
          geom_text(aes(x=GW,y=team,label=oppo),size=2) +
          theme_void() +
          theme(axis.text = element_text(face = "bold")) +
          theme(axis.text.y = element_text(margin=margin(0,-20,0,0))) + # fixing the margins
          scale_x_continuous(position="top",breaks=1:25) +
          labs(caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
          scale_fill_gradient2(guide=F,low="#00FF87",mid="#D6DCD8",high="#7F002D",midpoint=median(fdrfixtures$fdr)) # creating a gradient colour-coding that spans from toughest FDR ranking (coloured red) to easiest (coloured green)

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-37-1.png)

------------------------------------------------------------------------

Finding the names of goalscorers in a given fixture
---------------------------------------------------

Suppose now we are interested in finding the names of the goalscorers in
a given game? Here we take the example of the fixture West Ham - Crystal
Palace.

First we find the gameid using get\_game\_list and dplyr:

    library(fplscrapR)
    library(dplyr)

    gameid <- get_game_list() %>% filter(home=="WHU",away=="CRY") %>% select(id)

Next we fetch the game stats using get\_game\_stats:

    df <- get_game_stats(gameid=gameid)

Next we identify the goalscorers' playerids:

    goalscorerids <- unlist(list(df$goals_scored$a[[1]]$element,df$goals_scored$h[[1]]$element))

Finally we fetch the playernames of goalscorers using get\_player\_name:

    get_player_name(playerid=goalscorerids)

    ##                      playername  id
    ## 1               Jeffrey Schlupp 141
    ## 2                James McArthur 147
    ## 3     Javier Hernández Balcázar 419
    ## 4 Felipe Anderson Pereira Gomes 465
    ## 5              Robert Snodgrass 489

------------------------------------------------------------------------

List your classic mini-leagues and current ranks
------------------------------------------------

Suppose now we are interested in listing the classic mini-leagues of
which you are a member and your current rank in those mini-leagues?

First we fetch the entry information using get\_entry:

    library(fplscrapR)

    df <- get_entry(entryid=1076)

Next we select the classic league details and the relevant data columns
using dplyr:

    library(dplyr)

    df$leagues$classic %>% 
      select(name,entry_last_rank) # selecting the columns of interest here

    ##                                    name entry_last_rank
    ## 1                               Man Utd            2085
    ## 2                               Denmark              64
    ## 3                            Gameweek 1           11784
    ## 4                               Overall           11788
    ## 5        Who Got The Assist? Minileague             210
    ## 6                twitter.com/FPLSecrets             135
    ## 7               The FF Community Shield             206
    ## 8  Official /r/FantasyPL Classic League             558
    ## 9              The League of Doing Bits            1991
    ## 10                      101 Great Goals              36
    ## 11                            Grøn stue               1
    ## 12                       AB senior 2018               1
    ## 13                                FPLdk               2

------------------------------------------------------------------------

Visualizing the FPL histories of entries
----------------------------------------

Suppose now we are interested in visualizing the FPL history of select
entries, e.g myself (current FPL ID: 1076) and an FPL legend such as
James Eggersdorff (current FPL ID: 64342)?

First we fetch the entry histories using get\_entry\_hist:

    library(fplscrapR)

    entryhist <- get_entry_hist(entryid=c(1076,64342))

Next we plot the historical performance using dplyr and ggplot2:

    library(dplyr)
    library(ggplot2)

    entryhist %>% 
      ggplot() +
      geom_line(aes(x=season_name,y=rank,group=name,colour=name)) +
      theme_bw() +
      theme(legend.position="top") +
      labs(x="Season",y="Overall Rank",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-46-1.png)

------------------------------------------------------------------------

Visualizing your in-season Overall Rank progress
------------------------------------------------

Suppose now we are interested in visualizing the rank progression within
the current season of our own FPL teams?

First I fetch my own entry season using get\_entry\_season:

    library(fplscrapR)

    myseason <- get_entry_season(entryid=1076)

Next we plot the historical performance using dplyr and ggplot2:

    library(dplyr)
    library(ggplot2)

    myseason %>% 
      ggplot() +
      geom_line(aes(x=event,y=overall_rank,group=name)) +
      theme_bw() +
      labs(x="Gameweek",y="Overall Rank",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-49-1.png)

------------------------------------------------------------------------

Listing the top 20 of your favourite mini-league
------------------------------------------------

Suppose now we are interested in listing the top20 teams in your
favourite mini-league? Here we take the example of the current 'Who Got
the Assist? Mini-League'.

First we fetch the league data using get\_league:

    library(fplscrapR)

    df <- get_league(leagueid = 441,leaguetype = "classic")

Next we show the top20 ranking teams at the moment using dplyr:

    library(dplyr)

    df$standings$results %>% # selecting the standings and results data
      select(entry,entry_name,player_name,rank,total) %>% # selecting the columns we are interested in displaying
      filter(rank %in% 1:20) # filtering for the top20 ranks

    ##      entry         entry_name          player_name rank total
    ## 1   164452 Schürrle this year        Mark Sinclair    1  2004
    ## 2    63031   Bootstrap Theory           Liam Berry    2  2003
    ## 3   925417           Mendygos         Salva Ibarra    3  1999
    ## 4   902198    Bombers Wonders          James Wells    4  1991
    ## 5  1961786         NederLaget  Sebastian Berentsen    5  1980
    ## 6  1422278        ADIDA Oscar          Nate Thomas    6  1972
    ## 7   573693    @SkyPlayerInFPL           Paul Jones    7  1967
    ## 8  2350048             Owl FC          Alex Suarez    8  1965
    ## 9  1626971        Snoop N'Gog         George White    9  1960
    ## 10   70299   McWigan Athletic     McWigan Athletic   10  1960
    ## 11  100095   Dunk-King Donuts      Henry Wakefield   11  1959
    ## 12  111850    Keita Mooy Hart      Ashley Humphrey   12  1958
    ## 13  295552   The Pep Roulette Clas Lindberg Odhner   13  1956
    ## 14  398698       Sarrivederci         Sean Francis   14  1956
    ## 15 1112407      Piotrcovianka     Michal Sztajnert   15  1956
    ## 16  223489       Fabios flops        Stephen Ollis   16  1950
    ## 17  163818         SquadGoals        Viraj Bhayani   17  1949
    ## 18 2645314  Expected Toulouse      Chris Macartney   18  1946
    ## 19  421554      Hakuna Morata           Tommy Mroz   19  1945
    ## 20 1491075       Serenity Now      Joakim Ahlström   20  1945

------------------------------------------------------------------------

Finding the most owned players amongst the top 20 teams of your favourite mini-league
-------------------------------------------------------------------------------------

Suppose now we are interested in finding the most owned players in the
top20 teams in your favourite mini-league? Here we take the example of
the current 'Who Got the Assist? Mini-League'.

First we fetch the league data using get\_league:

    library(fplscrapR)

    df <- get_league(leagueid = 441,leaguetype = "classic")

Next we save in a new data frame the entry ids' of the top20 ranking
teams at the moment, using dplyr:

    library(dplyr)
    library(tidyr)

    top20entries <- df$standings$results %>% # selecting the standings and results data
      filter(rank %in% 1:20) %>%  # filtering for the top20 ranks
      select(entry) # selecting only the entry ids

Now we want to know the most selected players by these teams in a given
gameweek, say gameweek 31. So we set the gameweek number and collect the
players picks by each of the top20 entries using get\_entry\_picks. We
do so through a loop (which is decidedly not the most efficient method),
binding the picks together in a new data frame called 'top20picks':

    gameweek <- 31

    top20picks <- data.frame()

    for (i in 1:nrow(top20entries)){
      picks <- get_entry_picks(entryid=top20entries[i,],gw=gameweek)
      top20picks <- rbind(top20picks,picks$picks)
    }

Finally we list the top20 most owned players amongst the top20 managers
in the WGTA Mini-League using dplyr:

    top20picks %>% # working with our newly created data.frame
      group_by(element) %>% # transformation to group and summarize the count (number of times a given player was selected) at the element (the player) variable level
      summarize(n()) %>%
      arrange(-`n()`) %>% # arranging by most selected
      slice(1:20) %>% # slicing so we just see the top20 selected players
      arrange(element) %>% # arranging by element id in order to allow us to mutate (create a new variable) the element ids into player names 
      mutate(name=lapply(element,get_player_name)) %>% # adding the playername data
      tidyr::unnest(name) %>% # unnesting the playername data via tidyr
      select(playername,`n()`) %>% # selecting the variables of interest for our final list
      arrange(-`n()`) # arranging by most selected again

    ## # A tibble: 20 x 2
    ##    playername                       `n()`
    ##    <chr>                            <int>
    ##  1 Andrew Robertson                    20
    ##  2 Mohamed Salah                       20
    ##  3 Lukasz Fabianski                    18
    ##  4 Eden Hazard                         17
    ##  5 Jamie Vardy                         14
    ##  6 Callum Wilson                       13
    ##  7 Felipe Anderson Pereira Gomes       12
    ##  8 David Luiz Moreira Marinho          11
    ##  9 Paul Pogba                          11
    ## 10 Ricardo Domingos Barbosa Pereira    10
    ## 11 Sadio Mané                          10
    ## 12 Aaron Wan-Bissaka                    8
    ## 13 Trent Alexander-Arnold               8
    ## 14 Ryan Fraser                          7
    ## 15 Artur Boruc                          6
    ## 16 Matt Doherty                         6
    ## 17 Raúl Jiménez                         6
    ## 18 David Brooks                         5
    ## 19 James Maddison                       5
    ## 20 Jan Bednarek                         5

------------------------------------------------------------------------

Comparing total team BPS scores for a given game
------------------------------------------------

Suppose now we are interested in comparing the total BPS scores for each
team in a given game? Here we take the example of Manchester United -
Leicester from round 1, game id 6.

First we fetch the game stats using get\_game\_stats:

    library(fplscrapR)

    df <- get_game_stats(gameid=6)

Next we structure a coherent data frame of the two teams' total BPS
points, using R base's rbind and dplyr:

    library(dplyr)

    rbind( # combining the two objects containing the home and away team's BPS points
      data.frame(df$bps$a[[10]],team="away"),
      data.frame(df$bps$h[[10]],team="home")) %>% 
      group_by(team) %>% # transformation to group and summarize the value (bps points scored by a given player in the game) at the team variable level
      summarize(sum(value))

    ## # A tibble: 2 x 2
    ##   team  `sum(value)`
    ##   <fct>        <int>
    ## 1 away           173
    ## 2 home           203

------------------------------------------------------------------------

Average and top entry scores for the first 25 gameweeks
-------------------------------------------------------

Suppose we are interested in tracking the average and top FPL scores by
gameweek?

get\_round\_info(1:25)

First we fetch the round information for a selection of gameweeks, say
1-25, using the get\_round\_info function:

    library(fplscrapR)

    df <- get_round_info(1:25)

Next we use dplyr, tidyr and ggplot2 to transform and plot the data,
showing the average and top entry progression by gameweek:

    library(dplyr)
    library(tidyr)
    library(ggplot2)

    df %>% 
      select(id,average_entry_score,highest_score) %>% # selecting the relevant columns
      gather("var","value",-id) %>% # transforming from wide to long format for ggplot
      ggplot() + # plotting using ggplot2
        geom_line(aes(x=id,y=value,colour=var),size=1) +
        theme_bw() +
        scale_x_continuous(breaks=1:25) +
        labs(x="Gameweek",y="Score",title="Average and top FPL entry scores by gameweek",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

![](C:\Users\rcc.dbp\AppData\Local\Temp\RtmpyIobnS\preview-22d47d5d10ce.dir\allvignettes_files/figure-markdown_strict/unnamed-chunk-62-1.png)
