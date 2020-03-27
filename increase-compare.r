#!/usr/local/bin/Rscript
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(anytime)

options(stringsAsFactors = FALSE)

stateAbbvs <- list("Alabama,US"="AL", "Alaska,US"="AK", "Arizona,US"="AZ",
                   "Arkansas,US"="AR", "California,US"="CA", "Colorado,US"="CO",
                   "Connecticut,US"="CT", "District of Columbia,US"="DC",
                   "Delaware,US"="DE", "Florida,US"="FL", "Georgia,US"="GA",
                   "Hawaii,US"="HI", "Idaho,US"="ID", "Illinois,US"="IL",
                   "Indiana,US"="IN", "Iowa,US"="IA", "Kansas,US"="KS",
                   "Kentucky,US"="KY", "Louisiana,US"="LA", "Maine,US"="ME",
                   "Maryland,US"="MD", "Massachusetts,US"="MA",
                   "Michigan,US"="MI", "Minnesota,US"="MN",
                   "Mississippi,US"="MS", "Missouri,US"="MO",
                   "Montana,US"="MT", "Nebraska,US"="NE", "Nevada,US"="NV",
                   "New Hampshire,US"="NH", "New Jersey,US"="NJ",
                   "New Mexico,US"="NM", "New York,US"="NY",
                   "North Carolina,US"="NC", "North Dakota,US"="ND",
                   "Ohio,US"="OH", "Oklahoma,US"="OK", "Oregon,US"="OR",
                   "Pennsylvania,US"="PA", "Rhode Island,US"="RI",
                   "South Carolina,US"="SC", "South Dakota,US"="SD",
                   "Tennessee,US"="TN", "Texas,US"="TX", "Utah,US"="UT",
                   "Vermont,US"="VT", "Virginia,US"="VA", "Washington,US"="WA",
                   "West Virginia,US"="WV", "Wisconsin,US"="WI",
                   "Wyoming,US"="WY", "Puerto Rico,US"="PR",
                   "Guam,US"="GU", "Virgin Islands,US"="VI", "US"="US");


covid.clean <- function(inputData) {
    ## Filter out rows we don't want, like "Diamond Princess" and "Kitsap
    ## County, WA" Also fix some names.
    fixNames <- read.csv("names.csv", head=T);
    selection <- rep(FALSE, length(inputData$Province.State));

    newNames <- c();
    for (i in 1:length(selection)) {
        if ((inputData$Country.Region[i] %in% fixNames$Country) &&
            (inputData$Province.State[i] %in% fixNames$Province)) {

            for (j in 1:length(fixNames$Country)) {
                if ((fixNames$Country[j] == inputData$Country.Region[i]) &&
                    (fixNames$Province[j] == inputData$Province.State[i])) {
                    newNames <- c(newNames, fixNames$fixedName[j]);
                    selection[i] <- TRUE;
                }
            }
        }
    }

    ## Lose the bad rows.
    inputData <- inputData[selection,];

    ## Create summary entries for Canada, China, Australia.
    china <- inputData[inputData$Province.State=="Beijing",];
    china$Province.State <- "";
    australia <-
        inputData[inputData$Province.State=="New South Wales",];
    australia$Province.State <- "";
    canada <-
        inputData[inputData$Province.State=="Ontario",];
    canada$Province.State <- "";


    for (i in 5:length(colnames(inputData))) {
        china[,i] <-
            sum(inputData[inputData$Country.Region == "China",i]);
        australia[,i] <-
            sum(inputData[inputData$Country.Region == "Australia",i]);
        canada[,i] <-
            sum(inputData[inputData$Country.Region == "Canada",i]);
    }

    inputData <- rbind(inputData, china);
    newNames <- c(newNames, "China");
    inputData <- rbind(inputData, australia);
    newNames <- c(newNames, "Australia");
    inputData <- rbind(inputData, canada);
    newNames <- c(newNames, "Canada");

    output <- data.frame(
        t(inputData[,5:length(colnames(inputData))]));
    colnames(output) <- newNames;

    output <- cbind(data.frame(time=c(1:(dim(output)[1]))), output);

    return(output);
}

## Returns a melted frame ready for ggplot, where each time series
## starts at a threshold.  Use excludeChina to ignore individual
## provinces of China.  Use states to toggle seeing the US as a whole
## or individual states.
threshMelt <- function(data, threshold, states=FALSE, entireUS=TRUE,
                       excludeChina=TRUE, excludeAus=TRUE, excludeCan=TRUE,
                       onlyUS=FALSE) {

    ## Select places for which the count is over the input threshold.
    selected <- data[,data[length(rownames(data)),]>threshold];
    if (excludeChina)
        ## Ignore individual provinces in China.
        selected <- selected[,!grepl(",China",colnames(selected))];
    if (excludeAus)
        selected <- selected[,!grepl(",Australia",colnames(selected))];
    if (excludeCan)
        selected <- selected[,!grepl(",Canada",colnames(selected))];

    if (onlyUS) {
        selected <- selected[, grepl("US",colnames(selected))];
    }

    ## The 'drop=FALSE' thing is to prevent the data frame from being
    ## coerced to a vector when there is only one column.
    if ((!entireUS) && states) {
        selected <- selected[, colnames(selected) != "US", drop=FALSE];
    } else if (entireUS && (!states)) {
        selected <- selected[, !grepl(",US",colnames(selected)),drop=FALSE];
    } else if ((!entireUS) && (!states)) {
        selected <- selected[, !grepl("US",colnames(selected)),drop=FALSE];
    }

    melted <- data.frame(time=c(), region=c(), value=c());
    for (name in colnames(selected)) {
        if (name == "time") next;
        m <- melt(data[data[,name]>threshold,c("time", name)], id.vars="time",
                  variable.name="region");
        m$time <- 1:length(m$time);
        m <- mutate(m,
                    label=if_else(time == max(time),
                                  as.character(region), NA_character_));
        melted <- rbind(melted, m);
    }
    return(melted);
}

## Same as above, but without the threshold and time manipulation.
## Use excludeChina to ignore individual provinces of China.  Use
## states to toggle seeing the US as a whole or individual states.
meltNoThresh <- function(data, states=FALSE, entireUS=TRUE,
                         excludeChina=TRUE, excludeAus=TRUE, excludeCan=TRUE,
                         onlyUS=FALSE) {

    ## Select places for which the count is over the input threshold.
    selected <- data;
    if (excludeChina)
        ## Ignore individual provinces in China.
        selected <- selected[,!grepl(",China",colnames(selected))];
    if (excludeAus)
        selected <- selected[,!grepl(",Australia",colnames(selected))];
    if (excludeCan)
        selected <- selected[,!grepl(",Canada",colnames(selected))];

    if (onlyUS) {
        selected <- selected[, grepl("US",colnames(selected))];
    }

    ## The 'drop=FALSE' thing is to prevent the data frame from being
    ## coerced to a vector when there is only one column.
    if ((!entireUS) && states) {
        selected <- selected[, colnames(selected) != "US", drop=FALSE];
    } else if (entireUS && (!states)) {
        selected <- selected[, !grepl(",US",colnames(selected)),drop=FALSE];
    } else if ((!entireUS) && (!states)) {
        selected <- selected[, !grepl("US",colnames(selected)),drop=FALSE];
    }

    melted <- data.frame(time=c(), region=c(), value=c());
    for (name in colnames(selected)) {
        if (name == "time") next;
        m <- melt(data[,c("time", name)], id.vars="time",
                  variable.name="region");
        m$time <- 1:length(m$time);
        m <- mutate(m,
                    label=if_else(time == max(time),
                                  as.character(region), NA_character_));
        melted <- rbind(melted, m);
    }
    return(melted);
}


input.confirmed <- read.csv("data/time_series_covid19_confirmed_global.csv", head=T);
input.deaths <- read.csv("data/time_series_covid19_deaths_global.csv", head=T);

latestDate <- paste0(substr(last(colnames(input.confirmed)),2,100),"20");

confirmed <- covid.clean(input.confirmed);
deaths <- covid.clean(input.deaths);

confirmedMelt <- threshMelt(confirmed, 100);
deathsMelt <- threshMelt(deaths, 20);

makeNicePlot <- function(inData, xlabel, ylabel, ldate) {
    outp <- ggplot() +
        geom_line(data=inData, aes(x=time,y=value, color=region)) +
        geom_text_repel(data=inData,
                        aes(label=label, x=time, y=value,
                            color=region, hjust=1), na.rm=TRUE) +
        guides(color=FALSE) + theme_bw() +
        scale_y_log10(breaks=c(20,50,100,200,500,1000,2000,5000,10000,20000,
                               50000,100000,200000,500000,1000000),
                      labels=c("20", "50", "100","200","500","1000","2000","5000",
                               "10,000","20,000","50,000","100,000",
                               "200,000","500,000","1,000,000")) +
        scale_x_continuous(breaks=seq(0,200,5)) +
        theme(plot.margin = unit(c(1,3,1,1), "lines")) +
        labs(x=paste0(xlabel, " (", format(anytime(as.character(ldate)),
                                           "%d %B %Y"), ")"),
             y=ylabel);
    return(outp);
}

gpConfirmed <- makeNicePlot(confirmedMelt, "days since 100 confirmed cases",
                            "confirmed cases", latestDate);
gpDeaths <- makeNicePlot(deathsMelt, "days since 20 deaths",
                         "deaths", latestDate);

ggsave("images/confirmed.png", plot=gpConfirmed, device="png");
ggsave("images/deaths.png", plot=gpDeaths, device="png");

declutter <- data.frame(time=confirmed$time, Canada=confirmed$Canada, US=confirmed$US, Spain=confirmed$Spain, Italy=confirmed$Italy, Denmark=confirmed$Denmark, "South Korea"=confirmed[,"South Korea"], Iran=confirmed$Iran, Japan=confirmed$Japan, Germany=confirmed$Germany, "United Kingdom"=confirmed[,"United Kingdom"], China=confirmed$China, Singapore=confirmed$Singapore);

declutterMelt <- threshMelt(declutter, 100);
gpDeclutter <- makeNicePlot(declutterMelt, "days since 100 confirmed cases",
                            "confirmed cases", latestDate);
ggsave("images/declutter.png", plot=gpDeclutter, device="png");


### The JHU data seems to have stopped including US states.  So quit
### here and switch to the covid tracking project source.

## Population estimates from US Census Bureau, 2019
statePop <- list("CA"=39512223,   "TX"=28995881,
                 "FL"=21477737,   "NY"=19453561,
                 "PA"=12801989,   "IL"=12671821,
                 "OH"=11689100,   "GA"=10617423,
                 "NC"=10488084,   "MI"=9986857,
                 "NJ"=8882190,    "VA"=8535519,
                 "WA"=7614893,    "AZ"=7278717,
                 "MA"=6949503,    "TN"=6833174,
                 "IN"=6732219,    "MO"=6137428,
                 "MD"=6045680,    "WI"=5822434,
                 "CO"=5758736,    "MN"=5639632,
                 "SC"=5148714,    "AL"=4903185,
                 "LA"=4648794,    "KY"=4467673,
                 "OR"=4217737,    "OK"=3956971,
                 "CT"=3565287,    "UT"=3205958,
                 "IA"=3155070,    "PR"=3193694,
                 "NV"=3080156,    "AR"=3017825,
                 "MS"=2976149,    "KS"=2913314,
                 "NM"=2096829,    "NE"=1934408,
                 "ID"=1792065,    "WV"=1787147,
                 "HI"=1415872,    "NH"=1359711,
                 "ME"=1344212,    "MT"=1068778,
                 "RI"=1059361,    "DE"=973764,
                 "SD"=884659,     "ND"=762062,
                 "AK"=731545,     "DC"=705749,
                 "VT"=623989,     "WY"=578759,
                 "GU"=165718,     "VI"=104914,
                 "AS"=55641,      "MI"=55194,
                 "US"=331814684);


states <- read_csv("data/daily.csv");

## Convert NAs to 0.
states <- states %>% mutate_if(is.numeric, replace_na, 0)

## Generate a 'days' variable to be the integer number of days since 3/3/20.
states <- states %>%
    group_by(state) %>%
    mutate(days=date-min(date)) %>%
    mutate(tdays=ifelse(positive>10,0,1)) %>%
    mutate(dayCount=sum(tdays)) %>%
    mutate(zdays=days-dayCount) %>%
    filter(zdays > 0)

latestDate <- max(states$date);

gpConfirmedStates <- ggplot(states) +
    geom_line(mapping = aes(x=zdays,y=positive, color=state)) +
    geom_text_repel(states %>%
                    group_by(state) %>%
                    mutate(nstate=ifelse(days==max(days),state,NA)),
                    mapping = aes(label=nstate, x=zdays, y=positive,
                                  color=state, hjust=1), na.rm=TRUE) +
    guides(color=FALSE) + theme_bw() +
    scale_y_log10() +
    scale_x_continuous(breaks=seq(0,200,5)) +
    theme(plot.margin = unit(c(1,3,1,1), "lines")) +
    labs(x=paste0("days since 10 confirmed cases (",
                  format(anytime(as.character(latestDate)),
                         "%d %B %Y"), ")"),
         y="log confirmed cases");

ggsave("images/confirmedStates.png", plot=gpConfirmedStates, device="png");

## Calculate per capita number.  There's probably a more tidyverse-ish
## way to do this, but I couldn't figure it out.
states <- states %>% mutate(percap = 0);

for (i in 1:length(states$state)) {
    states$percap[i] <- states$positive[i] / statePop[[states$state[i]]];
}

gpConfirmedStatesPerCap <- ggplot(states) +
    geom_line(mapping = aes(x=zdays,y=percap, color=state)) +
    geom_text_repel(states %>%
                    group_by(state) %>%
                    mutate(nstate=ifelse(days==max(days),state,NA)),
                    mapping = aes(label=nstate, x=zdays, y=percap,
                                  color=state, hjust=1), na.rm=TRUE) +
    guides(color=FALSE) + theme_bw() +
    scale_y_log10() +
    scale_x_continuous(breaks=seq(0,200,5)) +
    theme(plot.margin = unit(c(1,3,1,1), "lines")) +
    labs(x=paste0("days since 10 confirmed cases (",
                  format(anytime(as.character(latestDate)),
                         "%d %B %Y"), ")"),
         y="log confirmed cases per capita");


ggsave("images/confirmedStatesPerCap.png", plot=gpConfirmedStatesPerCap,
       device="png");

gpConfirmedStatesPerCapRI <- ggplot(states) +
    geom_line(mapping = aes(x=zdays,y=percap,
                            color=ifelse(state=="RI", "black", state))) +
    geom_text_repel(states %>%
                    group_by(state) %>%
                    mutate(nstate=ifelse(days==max(days),state,NA)),
                    mapping = aes(label=nstate, x=zdays, y=percap,
                                  color=state, hjust=1), na.rm=TRUE) +
    guides(color=FALSE) + theme_bw() +
    scale_y_log10() +
    scale_x_continuous(breaks=seq(0,200,5)) +
    theme(plot.margin = unit(c(1,3,1,1), "lines")) +
    labs(x=paste0("days since 10 confirmed cases (",
                  format(anytime(as.character(latestDate)),
                         "%d %B %Y"), ")"),
         y="log confirmed cases per capita");

## Same thing, but highlight RI.
stCol <- rep("blue", 56);
stCol[states$state == "RI"] <- "red";

gpConfirmedStatesPerCapRI <- ggplot(states %>% mutate(RI=state=="RI")) +
    geom_line(mapping = aes(x=zdays,y=percap,
                            color=state,
                            size=RI)) +
    geom_text_repel(states %>%
                    mutate(RI=state=="RI") %>%
                    group_by(state) %>%
                    mutate(nstate=ifelse(days==max(days),state,NA)),
                    mapping = aes(label=nstate, x=zdays, y=percap,
                                  color=state, hjust=1), na.rm=TRUE) +
    guides(color=FALSE) + theme_bw() +
    scale_size_manual(values=c(.5,2)) +
    scale_color_manual(values=stCol) +
    scale_y_log10() +
    scale_x_continuous(breaks=seq(0,200,5)) +
    theme(plot.margin = unit(c(1,3,1,1), "lines"),
          legend.position="none") +
    labs(x=paste0("days since 10 confirmed cases (",
                  format(anytime(as.character(latestDate)),
                         "%d %B %Y"), ")"),
         y="log confirmed cases per capita");

ggsave("images/confirmedStatesPerCapRI.png", plot=gpConfirmedStatesPerCapRI,
       device="png");



## rank <- list();
## selected <- rep(TRUE, length(conStates$region));
## for (lev in levels(conStates$region)) {
##     if (max(conStates[conStates$region==lev,"value"]) < 5) {
##         selected[conStates$region==lev] <- FALSE;
##     }

##     rank[[stateAbbvs[[as.character(lev)]]]] <-
##         max(conStates[conStates$region==lev,"value"]);
## }
## conStates <- conStates[selected,];

## ## Create 3 tiers of states
## rankNames <- names(rank)[order(unlist(rank), decreasing=TRUE)];
## tier1 <- rankNames[(0:(length(rankNames)-1) %% 3) == 0];
## tier2 <- rankNames[(0:(length(rankNames)-1) %% 3) == 1];
## tier3 <- rankNames[(0:(length(rankNames)-1) %% 3) == 2];

## ## Make this per capita
## conStates <- conStates %>%
##     mutate(value=purrr::pmap_dbl(list(value, region),
##                                  function(x,r) {x/statePop[[r]]}));

## gpConStates <- ggplot() +
##     geom_line(data=conStates, aes(x=time,y=value, color=region)) +
##     geom_text_repel(data=subset(conStates, label %in% tier1),
##                     aes(label=label, x=time, y=value,
##                         color=region, hjust=1),
##                     na.rm=TRUE,
##                     nudge_x = 1.0,
##                     direction="y",
##                     hjust=0,
##                     segment.size=0.2) +
##     geom_text_repel(data=subset(conStates, label %in% tier2),
##                     aes(label=label, x=time, y=value,
##                         color=region, hjust=1),
##                     na.rm=TRUE,
##                     nudge_x = 2.0,
##                     direction="y",
##                     hjust=0,
##                     segment.size=0.2) +
##     geom_text_repel(data=subset(conStates, label %in% tier3),
##                     aes(label=label, x=time, y=value,
##                         color=region, hjust=1),
##                     na.rm=TRUE,
##                     nudge_x = 3.0,
##                     direction="y",
##                     hjust=0,
##                     segment.size=0.2) +
##     guides(color=FALSE) + theme_bw() +
##     scale_y_log10(breaks=c(20,50,100,200,500,1000,2000,5000,10000,20000,
##                            50000,100000,200000,500000,1000000),
##                   labels=c("20", "50", "100","200","500","1000","2000","5000",
##                            "10,000","20,000","50,000","100,000",
##                            "200,000","500,000","1,000,000")) +
##     ## The limit here is expanded to make room for the labels.
##     scale_x_continuous(breaks=seq(0,200,5),
##                        limits=c(0, max(conStates$time)+4)) +
##     theme(plot.margin = unit(c(1,3,1,1), "lines")) +
##     labs(x=paste0("days since March 4, 2020",
##                   " (", format(anytime(as.character(latestDate)),
##                                "%d %B %Y"), ")"),
##          y="log confirmed cases per capita");




## ggsave("images/confirmedStatesTime.png", plot=gpConStates, device="png");



## These are commented out just because there isn't really enough data
## yet to tell a story. Should work, though.
##
## ## Let's make deaths for states, too.
## deathsStatesMelt <- threshMelt(deaths, 2, states=TRUE,
##                                onlyUS=TRUE, entireUS=FALSE);

## ## Change the state names to state abbreviations.
## for (i in 1:length(deathsStatesMelt$label)) {
##     if (!is.na(deathsStatesMelt$label[i])) {
##         deathsStatesMelt$label[i] <-
##             stateAbbvs[[as.character(deathsStatesMelt$label[i])]];
##     }
## }

## gpDeathsStates <- makeNicePlot(deathsStatesMelt, 2,
##                                "deaths", latestDate);

## ggsave("deathsStates.png", plot=gpDeathsStates, device="png");

## ## Let's make one for 'recovered' for states, too.
## recoveredStatesMelt <- threshMelt(recovered, 10, states=TRUE,
##                                   onlyUS=TRUE, entireUS=FALSE);

## ## Change the state names to state abbreviations.
## for (i in 1:length(recoveredStatesMelt$label)) {
##     if (!is.na(recoveredStatesMelt$label[i])) {
##         recoveredStatesMelt$label[i] <-
##             stateAbbvs[[as.character(recoveredStatesMelt$label[i])]];
##     }
## }

## gpRecoveredStates <- makeNicePlot(recoveredStatesMelt, 10,
##                                   "recovered cases", latestDate);

## ggsave("recoveredStates.png", plot=gpRecoveredStates, device="png");


## # Code to turn off clipping
## gt <- ggplotGrob(p)
## gt$layout$clip[gt$layout$name == "panel"] <- "off"
## grid.draw(gt)
