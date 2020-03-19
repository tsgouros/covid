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

