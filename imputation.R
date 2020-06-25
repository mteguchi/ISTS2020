# Data imputation done here


rm(list = ls())

library(jagsUI)
library(tidyverse)


source("ISTS2020_funcs.R")
save.data <- F
run.date <- Sys.Date()

#Set up the MCMC parameters

MCMC.params <- list(n.chains = 5,
                    n.samples = 100000,
                    n.burnin = 50000,
                    n.thin = 5)

#Bring in the data


all.years <- 2001:2017
idx <- 1:length(all.years)

year.begin.JM <- 2001
year.end <- 2017
data.JM <- data.extract(location = "JM", 
                             year.begin = year.begin.JM, 
                             year.end = year.end)

JM.keep <- 2001:2017
idx.JM <- idx[all.years %in% JM.keep]
n.keep.JM <- length(idx.JM)
dt.JM <- idx.JM[2:length(idx.JM)] - idx.JM[1:(length(idx.JM)-1)]

year.begin.W <- 2006
data.W <- data.extract(location = "W", 
                            year.begin = year.begin.W, 
                            year.end = year.end)

W.keep <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2016, 2017)
idx.W <- idx[all.years %in% W.keep]
n.keep.W <- length(idx.W)
dt.W <- idx.W[2:length(idx.W)] - idx.W[1:(length(idx.W)-1)]

df.year.JM <- data.frame(start = seq(from = 2001.292, to = 2017.292, by = 1),
                         end = seq(from = 2002.208, to = 2018.208, by = 1),
                         season = seq(from = 2001, to = 2017, by = 1)) %>%
  mutate(odd.year = ifelse(season%%2 == 1, "Y", "N"))


filename.root <- paste0("jags_out_DFS_imputation_", run.date)

#Combine datasets for analysis
# JM has more data than W, so we need to pad W data
y.W <- rbind(array(data = NA, 
                   dim = c(nrow(data.JM$jags.data2$y) - nrow(data.W$jags.data2$y),
                           ncol(data.JM$jags.data2$y))),
             data.W$jags.data2$y)

y <- cbind(as.vector(t(data.JM$jags.data2$y)), as.vector(t(y.W)))

years <- rep(2001:2017, each = 12)
n.years <- 17

# for estimating U  ######
n.timeseries <- ncol(y)

jags.data <- list(y = y,
                  m = rep(1:12, times = n.years),
                  n.steps = nrow(y),
                  n.months = 12,
                  pi = pi,
                  period = c(period.JM, period.W),
                  n.timeseries = n.timeseries,
                  n.years = n.years)

#Define which parameters to monitor

jags.params <- c("c", "beta.cos", "beta.sin",
                 'sigma.X', "sigma.y", "N", 
                 "y", "X", "deviance")

jm <- jags(jags.data,
           inits = NULL,
           parameters.to.save= jags.params,
           model.file = 'models/model_norm_norm_DFS_imputation.txt',
           n.chains = MCMC.params$n.chains,
           n.burnin = MCMC.params$n.burnin,
           n.thin = MCMC.params$n.thin,
           n.iter = MCMC.params$n.samples,
           DIC = T, parallel=T)

out.list <- list(jm = jm,
                 jags.data = jags.data,
                 data.JM = data.JM,
                 data.W = data.W)

if (save.data)
  saveRDS(out.list,
          file = paste0("RData/", filename.root, '.rds'))


# bayesplot::mcmc_dens(jm$samples, c("N.true.JM[1]", "N.true.JM[2]",
#                                    "N.true.JM[3]", "N.true.JM[4]",
#                                    "N.true.JM[5]", "N.true.JM[6]",
#                                    "N.true.JM[7]", "N.true.JM[8]",
#                                    "N.true.JM[9]", "N.true.JM[10]",
#                                    "N.true.JM[11]", "N.true.JM[12]",
#                                    "N.true.JM[13]", "N.true.JM[14]",
#                                    "N.true.JM[15]", "N.true.JM[16]",
#                                    "N.true.JM[17]"))

# process SD for two beaches:

bayesplot::mcmc_dens(jm$samples, c("sigma.y[1]", "sigma.y[2]"))


# process SD for two beaches:

bayesplot::mcmc_dens(jm$samples, c("sigma.X[1]", "sigma.X[2]"))
# cos and sin for the discrete Foureir series 
bayesplot::mcmc_dens(jm$samples, c("beta.cos[1]", "beta.sin[1]",
                                   "beta.cos[2]", "beta.sin[2]"))

#Posteror on log(N) for JM
bayesplot::mcmc_dens(jm$samples, c("N[1,1]", "N[2,1]",
                                   "N[3,1]", "N[4,1]",
                                   "N[5,1]", "N[6,1]",
                                   "N[7,1]", "N[8,1]",
                                   "N[9,1]", "N[10,1]",
                                   "N[11,1]", "N[12,1]",
                                   "N[13,1]", "N[14,1]",
                                   "N[15,1]", "N[16,1]",
                                   "N[17,1]"))
# 
# 
# #Posteror on log(N) for W
# 
# bayesplot::mcmc_dens(jm$samples, c("N[2,1]", "N[2,2]",
#                                    "N[2,3]", "N[2,4]",
#                                    "N[2,5]", "N[2,6]",
#                                    "N[2,7]", "N[2,8]",
#                                    "N[2,9]", "N[2,10]",
#                                    "N[2,11]", "N[2,12]",
#                                    "N[2,13]", "N[2,14]",
#                                    "N[2,15]", "N[2,16]"))

#Posteror on log(N) for W
bayesplot::mcmc_dens(jm$samples, c("N[1,2]", "N[2,2]",
                                   "N[3,2]", "N[4,2]",
                                   "N[5,2]", "N[6,2]",
                                   "N[7,2]", "N[8,2]",
                                   "N[9,2]", "N[10,2]",
                                   "N[11,2]", "N[12,2]",
                                   "N[13,2]", "N[14,2]",
                                   "N[15,2]", "N[16,2]",
                                   "N[17,2]"))

