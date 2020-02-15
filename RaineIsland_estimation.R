# Raine Island hatchling counts

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
library(reshape2)
library(jagsUI)
library(bayesplot)


MCMC.params <- list(n.chains = 3,
                    n.samples = 500000,
                    n.burnin = 300000,
                    n.thin = 5)

col.def <- cols(Date = col_date(format = "%m/%d/%Y"),
                Counts_100m = col_double(),
                Season = col_integer(),
                Sector = col_character())

data.1 <- read_csv(file = "data/RaineIsland_Hatchling_Data_v2.csv",
                   col_types = col.def)
data.1.wide <- dcast(data.1, Date ~ Sector, value.var = "Counts_100m")
#mutate(Year = year(Date)) %>%
#mutate(DOSeason = as.numeric(Date - as.Date(paste0(Season, "-12-01"))))


# create a data frame that repeats nesting season, which will be merged with data.1 to
# create missing data. There are 132 days (from 12/20 to 4/30, if not leap year) within
# each season
Day1 <- "12-20"
Day2 <- "04-30"

n.days <- as.numeric(as.Date(paste0("2018-", Day2)) - as.Date(paste0("2017-", Day1)))

seasons <- unique(data.1$Season)
# 
# data.0 <- data.frame(Season = rep(seasons, each = n.days),
#                      DOSeason = rep(seq(from = 1, to = n.days, by = 1), 
#                                     times = length(seasons)),
#                      Counts_100m = NA) %>%
#   #mutate(Year = ifelse(DOSeason < 12, Season, Season + 1)) %>%
#   mutate(Date = as.Date(paste0(Season, "-", Day1)) + days(DOSeason))
# 
# data.0 %>% left_join(data.1.wide, by = "Date") -> data.2
# 
# data.2 %>% select(Season, DOSeason, `2014R`, `2017R`, North, South, West) -> data.2.a
# 
# obs.y <- array(NA, c(length(seasons), 5, n.days))
# 
# for (i in 1:length(seasons)){
#   obs.y[i, ,] <- t(filter(data.2.a, Season == seasons[i]) %>% select(-c("Season", "DOSeason")))
#   
# }

data.1 %>% 
  mutate(DOSeason = as.numeric(Date - as.Date(paste0(Season, "-", Day1)))) %>%
  select(-Date) -> data.3
Sectors <- unique(data.3$Sector)

obs.t <- obs.y.1 <- matrix(NA, nrow = length(Sectors) * length(seasons), ncol = 10)
n.vec <- vector(mode = "numeric", length = length(Sectors) * length(seasons))
season.sector.idx <- matrix(NA, nrow = length(Sectors) * length(seasons), ncol = 2)

max.mat <- matrix(NA, nrow = length(seasons), ncol = length(Sectors))

r <- season_idx <- sector_idx <- 1
for (season_idx in 1:length(seasons)){
  for (sector_idx in 1:length(Sectors)){
    data.3 %>% filter(Season == seasons[season_idx]) %>%
      filter(Sector == Sectors[sector_idx]) -> tmp
    if (nrow(tmp) > 0){
      obs.t[r, 1:nrow(tmp)] <- tmp$DOSeason
      obs.y.1[r, 1:nrow(tmp)] <- tmp$Counts_100m
      n.vec[r] <- nrow(tmp)
      max.mat[season_idx, sector_idx] <- max(tmp$Counts_100m, na.rm = T)      
    }
    season.sector.idx[r,] <- c(season_idx, sector_idx)
    r <- r + 1
  }
}

max.mat[max.mat < 0] <- NA

for (k in 1:ncol(max.mat)){
  max.mat[is.na(max.mat[,k]), k] <- mean(max.mat[, k], na.rm = T)
}

jags.data <- list(y = (obs.y.1),
                  t = obs.t,
                  n.vec = n.vec,
                  idx = season.sector.idx,
                  min = rep(1, length(Sectors)),
                  n = length(n.vec),
                  n.sectors = length(Sectors),
                  n.years = length(seasons),
                  max.mat = max.mat)
# 
jags.params <- c("sigma.y", "P", "S", "K", "max",
                 "y", "X", "sigma.max", "deviance")
# 
# MCMC.params <- list(n.chains = 3,
#                     n.samples = 100000,
#                     n.burnin = 80000,
#                     n.thin = 5)

# This uses estimated model parameters to predict the numbers, rather than imputation

jm <- jags(jags.data,
             inits = NULL,
             parameters.to.save= jags.params,
             model.file = 'models/model_Girondot_Par_estimation_dnorm.txt',
             n.chains = MCMC.params$n.chains,
             n.burnin = MCMC.params$n.burnin,
             n.thin = MCMC.params$n.thin,
             n.iter = MCMC.params$n.samples,
             DIC = T, parallel=T)
  
saveRDS(jm, paste0("RData/Girondot_Par_estimation_dnorm_", Day1, "_", Day2, ".rds"))
  
mcmc_trace(jm$samples, pars = c("P[1]", "P[2]", "P[3]", "P[4]"))

mcmc_trace(jm$samples, pars = c("sigma.y", "S", "K"))



