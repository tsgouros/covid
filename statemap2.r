#!/usr/local/bin/Rscript

library(tidyverse)
library(urbnmapr)
library(anytime)
library(gganimate)

inputData <- read.csv("data/daily.csv",
                      header=TRUE, stringsAsFactors=FALSE);
colnames(inputData)[6] <- "deaths";


## Set all the NAs to zero.
inputData[is.na(inputData)] <- 0;

## The input data uses the state abbreviations as the state name.
inputData$state_abbv <- inputData$state;

## Grab the top date, and use that.
targetDate <- inputData$date[1];

## Join the input covid data to the state borders from urbnmapr
covid <- left_join(inputData[inputData$date==targetDate,],
                   urbnmapr::states,
                   by="state_abbv");

cat("target date:", targetDate, "\n");

## Execute the plot.
gp <- ggplot() +
    geom_polygon(data=covid,
                 mapping=aes(x=long, y=lat, group=group, fill=positive),
                 color="white", size=0.25) +
    coord_map(projection="albers", lat0=39, lat1=45) +
    scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") +
    labs(title=paste0("positive diagnoses as of ",
                      format(anytime(as.character(targetDate)), "%d %B %Y")));

ggsave("images/state-positives.png", plot=gp, device="png");

gp <- ggplot() +
    geom_polygon(data=covid,
                 mapping=aes(x=long, y=lat, group=group, fill=deaths),
                 color="white", size=0.25) +
    coord_map(projection="albers", lat0=39, lat1=45) +
    scale_fill_continuous(low = "white", high = "darkblue", guide="colorbar") +
    labs(title=paste0("deaths as of ",
                      format(anytime(as.character(targetDate)), "%d %B %Y")));

ggsave("images/state-deaths.png", plot=gp, device="png");

gp <- ggplot() +
    geom_polygon(data=covid,
                 mapping=aes(x=long, y=lat, group=group, fill=positive/total),
                 color="white", size=0.25) +
    coord_map(projection="albers", lat0=39, lat1=45) +
    scale_fill_continuous(low = "green", high = "red", guide="colorbar") +
    theme(plot.margin = unit(c(0,0,0,0), "lines")) +
    labs(title=paste0("testing ratio (positives/total) as of ",
                      format(anytime(as.character(targetDate)), "%d %B %Y")));

ggsave("images/state-testing.png", plot=gp, device="png");


## Now we're going to do the animation.  To begin with, we start with
## the whole dataset, minus the early incomplete days.

##inputData <- inputData[order(inputData$date),];

## Ignore the incomplete early days.
inputData <- inputData[inputData$date > 20200307,];

## Ignore PR, GU, VI, AS.  (Sorry guys.)
stateNames <- factor(urbnmapr::states$state_abbv);
inputData <- inputData[inputData$state %in% levels(stateNames),];

## Join the entire input covid data to the state borders from urbnmapr
covid <- left_join(inputData,
                   urbnmapr::states,
                   by="state_abbv");

covid$date <- anytime(as.character(covid$date));

gp.anim <- ggplot() +
    geom_polygon(data=covid,
                 mapping=aes(x=long, y=lat, group=group, fill=positive/total),
                 color="white", size=0.25) +
    transition_time(date) + ease_aes('linear') +
    coord_map(projection="albers", lat0=39, lat1=45) +
    scale_fill_continuous(low = "green", high = "red", guide="colorbar") +
    theme(plot.margin = unit(c(0,0,0,0), "lines")) +
    labs(title=paste0("testing ratio (positives/total) as of ",
                      format(anytime(as.character(targetDate)), "%d %B %Y"),
                      ": {frame_time}"));

animate(gp.anim, nframes=100, fps = 4, width = 750, height = 450, end_pause=20)
anim_save("images/state-testing.gif", plot=gp.anim, path = ".");
