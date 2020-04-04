#!/usr/local/bin/Rscript
library(tidyverse)
library(ggplot2)
library(ggridges)
library(anytime)

countries <- c("Australia","Austria","Brazil","Canada","China",
 "Cuba","Denmark","France", "Germany", "Greece", "Iceland", "India",
 "Iran", "Israel", "Italy", "Jamaica", "Japan", "Mexico", "Netherlands", "Norway",
 "Russia", "Singapore", "South_Korea","Spain", "Sweden","Switzerland",
 "Thailand", "Turkey", "United_Kingdom","United_States_of_America");

fullData <- read.csv("data/full_data.csv", header=TRUE, stringsAsFactors=TRUE);

fullData <- fullData %>%
    filter(!is.na(cases)) %>%
    filter(countriesAndTerritories %in% countries) %>%
    mutate(time = as.numeric(anytime(paste0(year, "-", month, "-", day)))) %>%
    mutate(time = ceiling((time - min(time)) / (24 * 60 * 60)));# %>%
## The 'ceiling' is to deal with daylight savings time.
##    mutate(countriesAndTerritories = ifelse(countriesAndTerritories == "United_States_of_America", "USA", countriesAndTerritories));

## Cannot figure out how to do this with mutate. It always reports
## that it resulted in NaNs, but there aren't any.
fullData$logCases <- unlist(lapply(fullData$cases, function(x)
    {log(max(x,1.0), base=10)}));

## Squeeze out the momentary zeros.
squeeze <- function(data) {
    keep <- c();
    for (loc in levels(data$countriesAndTerritories)) {
        subData <- data[data$countriesAndTerritories == loc,];
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
                 mapping=aes(x=time, y=countryterritoryCode,
                             fill=countryterritoryCode, height=logCases,
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



