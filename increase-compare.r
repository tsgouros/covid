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

## Population estimates from US Census Bureau, 2019
statePop <- list("California,US"=39512223,     "Texas,US"=28995881,
                 "Florida,US"=21477737,    "New York,US"=19453561,
                 "Pennsylvania,US"=12801989,    "Illinois,US"=12671821,
                 "Ohio,US"=11689100,    "Georgia,US"=10617423,
                 "North Carolina,US"=10488084,    "Michigan,US"=9986857,
                 "New Jersey,US"=8882190,    "Virginia,US"=8535519,
                 "Washington,US"=7614893,    "Arizona,US"=7278717,
                 "Massachusetts,US"=6949503,    "Tennessee,US"=6833174,
                 "Indiana,US"=6732219,    "Missouri,US"=6137428,
                 "Maryland,US"=6045680,    "Wisconsin,US"=5822434,
                 "Colorado,US"=5758736,    "Minnesota,US"=5639632,
                 "South Carolina,US"=5148714,    "Alabama,US"=4903185,
                 "Louisiana,US"=4648794,    "Kentucky,US"=4467673,
                 "Oregon,US"=4217737,    "Oklahoma,US"=3956971,
                 "Connecticut,US"=3565287,    "Utah,US"=3205958,
                 "Iowa,US"=3155070,    "Puerto Rico,US"=3193694,
                 "Nevada,US"=3080156,    "Arkansas,US"=3017825,
                 "Mississippi,US"=2976149,    "Kansas,US"=2913314,
                 "New Mexico,US"=2096829,    "Nebraska,US"=1934408,
                 "Idaho,US"=1792065,    "West Virginia,US"=1787147,
                 "Hawaii,US"=1415872,    "New Hampshire,US"=1359711,
                 "Maine,US"=1344212,    "Montana,US"=1068778,
                 "Rhode Island,US"=1059361,    "Delaware,US"=973764,
                 "South Dakota,US"=884659,    "North Dakota,US"=762062,
                 "Alaska,US"=731545,    "District of Columbia,US"=705749,
                 "Vermont,US"=623989,    "Wyoming,US"=578759,
                 "Guam,US"=165718,    "Virgin Islands,US"=104914,
                 "American Samoa,US"=55641,    "Northern Mariana Islands,US"=55194,
                 "US"=331814684);

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

    ## Create summary entries for Canada, China, US, Australia.
    china <- inputData[inputData$Province.State=="Beijing",];
    china$Province.State <- "";
    us <- inputData[inputData$Province.State=="Kansas",];
    us$Province.State <- "";
    australia <-
        inputData[inputData$Province.State=="New South Wales",];
    australia$Province.State <- "";
    canada <-
        inputData[inputData$Province.State=="Ontario",];
    canada$Province.State <- "";


    for (i in 5:length(colnames(inputData))) {
        china[,i] <-
            sum(inputData[inputData$Country.Region == "China",i]);
        us[,i] <-
            sum(inputData[inputData$Country.Region == "US",i]);
        australia[,i] <-
            sum(inputData[inputData$Country.Region == "Australia",i]);
        canada[,i] <-
            sum(inputData[inputData$Country.Region == "Canada",i]);
    }

    inputData <- rbind(inputData, china);
    newNames <- c(newNames, "China");
    inputData <- rbind(inputData, us);
    newNames <- c(newNames, "US");
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


input.confirmed <- read.csv("data/time_series_19-covid-Confirmed.csv", head=T);
input.deaths <- read.csv("data/time_series_19-covid-Deaths.csv", head=T);
input.recovered <- read.csv("data/time_series_19-covid-Recovered.csv", head=T);

latestDate <- paste0(substr(last(colnames(input.confirmed)),2,100),"20");

confirmed <- covid.clean(input.confirmed);
deaths <- covid.clean(input.deaths);
recovered <- covid.clean(input.recovered);

confirmedMelt <- threshMelt(confirmed, 100);
deathsMelt <- threshMelt(deaths, 20);
recoveredMelt <- threshMelt(recovered, 100);

confirmedStatesMelt <- threshMelt(confirmed, 10, states=TRUE,
                                  onlyUS=TRUE, entireUS=FALSE);

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
gpRecovered <- makeNicePlot(recoveredMelt, "days since 100 recovered cases",
                            "recovered cases", latestDate);

ggsave("images/confirmed.png", plot=gpConfirmed, device="png");
ggsave("images/deaths.png", plot=gpDeaths, device="png");
ggsave("images/recovered.png", plot=gpRecovered, device="png");

declutter <- data.frame(time=confirmed$time, Canada=confirmed$Canada, US=confirmed$US, Spain=confirmed$Spain, Italy=confirmed$Italy, Denmark=confirmed$Denmark, "South Korea"=confirmed[,"South Korea"], Iran=confirmed$Iran, Japan=confirmed$Japan, Germany=confirmed$Germany, "United Kingdom"=confirmed[,"United Kingdom"], China=confirmed$China);

declutterMelt <- threshMelt(declutter, 100);
gpDeclutter <- makeNicePlot(declutterMelt, "days since 100 confirmed cases",
                            "confirmed cases", latestDate);
ggsave("images/declutter.png", plot=gpDeclutter, device="png");


## Let's make one for states, too.
confirmedStatesMelt <- threshMelt(confirmed, 10, states=TRUE,
                                  onlyUS=TRUE, entireUS=FALSE);

## Change the state names to state abbreviations.
for (i in 1:length(confirmedStatesMelt$label)) {
    if (!is.na(confirmedStatesMelt$label[i])) {
        confirmedStatesMelt$label[i] <-
            stateAbbvs[[as.character(confirmedStatesMelt$label[i])]];
    }
}

gpConfirmedStates <- makeNicePlot(confirmedStatesMelt,
                                  "days since 10 confirmed cases",
                                  "confirmed cases", latestDate);

ggsave("images/confirmedStates.png", plot=gpConfirmedStates, device="png");

confirmedStatesPerCap <- confirmedStatesMelt %>%
    mutate(value=purrr::pmap_dbl(list(value, region),
                                 function(x,r) {x/statePop[[r]]}));
gpConfirmedStatesPerCap <- ggplot() +
        geom_line(data=confirmedStatesPerCap, aes(x=time,y=value, color=region)) +
        geom_text_repel(data=confirmedStatesPerCap,
                        aes(label=label, x=time, y=value,
                            color=region, hjust=1), na.rm=TRUE) +
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

## Make one for the states that share a real time axis, no threshold.
conStates <- meltNoThresh(confirmed, states=TRUE, onlyUS=TRUE, entireUS=FALSE);
conStates <- conStates[conStates$time >= 49,];
conStates$time <- conStates$time - 49;

## We're going to make a log plot out of this, so get rid of zeros.
conStates$value[conStates$value==0] <- 1.0

## Change the state names to state abbreviations.
for (i in 1:length(conStates$label)) {
    if (!is.na(conStates$label[i])) {
        conStates$label[i] <-
            stateAbbvs[[as.character(conStates$label[i])]];
    }
}

rank <- list();
selected <- rep(TRUE, length(conStates$region));
for (lev in levels(conStates$region)) {
    if (max(conStates[conStates$region==lev,"value"]) < 5) {
        selected[conStates$region==lev] <- FALSE;
    }

    rank[[stateAbbvs[[as.character(lev)]]]] <-
        max(conStates[conStates$region==lev,"value"]);
}
conStates <- conStates[selected,];

## Create 3 tiers of states
rankNames <- names(rank)[order(unlist(rank), decreasing=TRUE)];
tier1 <- rankNames[(0:(length(rankNames)-1) %% 3) == 0];
tier2 <- rankNames[(0:(length(rankNames)-1) %% 3) == 1];
tier3 <- rankNames[(0:(length(rankNames)-1) %% 3) == 2];

## Make this per capita
conStates <- conStates %>%
    mutate(value=purrr::pmap_dbl(list(value, region),
                                 function(x,r) {x/statePop[[r]]}));

gpConStates <- ggplot() +
    geom_line(data=conStates, aes(x=time,y=value, color=region)) +
    geom_text_repel(data=subset(conStates, label %in% tier1),
                    aes(label=label, x=time, y=value,
                        color=region, hjust=1),
                    na.rm=TRUE,
                    nudge_x = 1.0,
                    direction="y",
                    hjust=0,
                    segment.size=0.2) +
    geom_text_repel(data=subset(conStates, label %in% tier2),
                    aes(label=label, x=time, y=value,
                        color=region, hjust=1),
                    na.rm=TRUE,
                    nudge_x = 2.0,
                    direction="y",
                    hjust=0,
                    segment.size=0.2) +
    geom_text_repel(data=subset(conStates, label %in% tier3),
                    aes(label=label, x=time, y=value,
                        color=region, hjust=1),
                    na.rm=TRUE,
                    nudge_x = 3.0,
                    direction="y",
                    hjust=0,
                    segment.size=0.2) +
    guides(color=FALSE) + theme_bw() +
    scale_y_log10(breaks=c(20,50,100,200,500,1000,2000,5000,10000,20000,
                           50000,100000,200000,500000,1000000),
                  labels=c("20", "50", "100","200","500","1000","2000","5000",
                           "10,000","20,000","50,000","100,000",
                           "200,000","500,000","1,000,000")) +
    ## The limit here is expanded to make room for the labels.
    scale_x_continuous(breaks=seq(0,200,5),
                       limits=c(0, max(conStates$time)+4)) +
    theme(plot.margin = unit(c(1,3,1,1), "lines")) +
    labs(x=paste0("days since March 4, 2020",
                  " (", format(anytime(as.character(latestDate)),
                               "%d %B %Y"), ")"),
         y="log confirmed cases per capita");




ggsave("images/confirmedStatesTime.png", plot=gpConStates, device="png");



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
