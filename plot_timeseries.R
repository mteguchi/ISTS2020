# plot time series


rm(list = ls())
library(ggplot2)
#library(ggridges)
library(tidyverse)

source("data_extract.R")

color.JM <- "orangered"
color.W <- "darkseagreen3"


all.years <- 2001:2017
idx <- 1:length(all.years)

year.begin.JM <- 2001
year.end <- 2017
data.jags.JM <- data.extract(location = "JM", 
                             year.begin = year.begin.JM, 
                             year.end = year.end)

JM.keep <- 2001:2017
idx.JM <- idx[all.years %in% JM.keep]
n.keep.JM <- length(idx.JM)
dt.JM <- idx.JM[2:length(idx.JM)] - idx.JM[1:(length(idx.JM)-1)]

year.begin.W <- 2006
data.jags.W <- data.extract(location = "W", 
                            year.begin = year.begin.W, 
                            year.end = year.end)
W.keep <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2016, 2017)
idx.W <- idx[all.years %in% W.keep]
n.keep.W <- length(idx.W)
dt.W <- idx.W[2:length(idx.W)] - idx.W[1:(length(idx.W)-1)]
