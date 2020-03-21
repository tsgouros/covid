#!/usr/local/bin/Rscript
library(tidyverse)
library(ggplot2)
library(ggridges)
library(anytime)

fullData <- read.csv("data/full_data.csv", header=TRUE, stringsAsFactors=TRUE);

fullData <- fullData %>% filter(!is.na(new_cases));

fullData$logCases <-
    unlist(lapply(fullData$new_cases, function(x) {log(max(x,1.0), base=10)}));

countries <- c("Australia","Austria","Brazil","Canada","China",
 "Cuba","Denmark","France", "Germany", "Greece", "Iceland", "India",
 "Iran", "Israel", "Italy", "Jamaica", "Japan", "Mexico", "Netherlands", "Norway",
 "Russia", "Singapore", "South Korea","Spain", "Sweden","Switzerland",
 "Thailand", "Turkey", "United Kingdom","United States");

#fullData$date <- anytime(fullData$date);

fullData <- fullData[fullData$location %in% countries,];

fullData$time <- as.numeric(anytime(fullData$date));
fullData$time <- (fullData$time - min(fullData$time)) / (24 * 60 * 60)

## Squeeze out the momentary zeros.
squeeze <- function(data) {
    keep <- c();
    for (loc in levels(data$location)) {
        subData <- data[data$location == loc,];
        subKeep <- rep(TRUE, length(subData$logCases));
        if (length(subKeep) >= 3) {
            for (i in 2:(length(subKeep)-1)) {
                if ((subData$logCases[i] == 0) &&
                    ((subData$logCases[i-1] > 0) &&
                     (subData$logCases[i+1] > 0))) {
                    subKeep[i] <- FALSE;
                }
                if (i < (length(subKeep)-1)) {
                    if ((subData$logCases[i] == 0) &&
                        ((subData$logCases[i-1] > 0) &&
                         (subData$logCases[i+2] > 0))) {
                        subKeep[i] <- FALSE;
                    }
                }
            }

            if ((subData$logCases[length(subData$logCases)] == 0) &&
                (subData$logCases[length(subData$logCases) - 1] > 0)) {
                subKeep[length(subData$logCases)] <- FALSE;
            }
        }

        keep <- c(keep, subKeep);
    }
    return(data[keep,]);
}

fullData <- squeeze(fullData);
fullData <- squeeze(fullData);

gridge <- ggplot(fullData,
                 mapping=aes(x=time, y=location,
                             fill=location, height=logCases,
                             scale=1.5)) +
    geom_density_ridges_gradient(stat="identity", show.legend=FALSE) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_x_continuous(expand = c(0.01, 0)) +
    labs(x="days since 1/22/20", y="log of daily new cases") +
    coord_cartesian(clip = "off") +
    theme_ridges();

ggsave("images/ridgeplot.png", plot=gridge, device="png");


stateData <- read.csv("data/daily.csv", header=TRUE, stringsAsFactors=TRUE);

stateData$date <- as.numeric(anytime(as.character(stateData$date)));
stateData$date <- (stateData$date - min(stateData$date)) / (24 * 60 * 60)

stateData <- stateData[with(stateData,
                            order(state, date)),];

stateRidge <- ggplot(stateData,
                     mapping=aes(x=date, y=state, fill=state,
                                 height=positive, scale=1.5)) +
    geom_density_ridges_gradient(stat="identity", show.legend=FALSE) +
    scale_y_discrete(expand = c(0.01,0)) +
    scale_x_continuous(expand = c(0.01, 0)) +
    labs(x="days since 3/4/20", y="total confirmed cases") +
    coord_cartesian(clip = "off") +
    theme_ridges();

ggsave("images/stateridges.png", plot=stateRidge, device="png");



