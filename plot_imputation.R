# plot imputed data

rm(list = ls())
library(ggplot2)
library(tidyverse)
source("ISTS2020_funcs.R")

save.fig <- T
jags.out <- readRDS(file = "RData/jags_out_DFS_imputation_2020-02-14.rds")

year.begin.JM <- 2001
year.end <- 2017

#pull together results
# extract ys - include estimated missing data
ys.stats.JM <- data.frame(low =jags.out$jm$q2.5$y[,1],
                          median = jags.out$jm$q50$y[,1],
                          high = jags.out$jm$q97.5$y[,1])

ys.stats.JM$time <- jags.out$data.JM$data.1$Frac.Year
ys.stats.JM$obsY <- jags.out$data.JM$data.1$Nests
ys.stats.JM$month <- jags.out$data.JM$data.1$Month
ys.stats.JM$year <- jags.out$data.JM$data.1$Year
ys.stats.JM$Season <- jags.out$data.JM$data.1$Season
ys.stats.JM$location <- "Jamursba-Medi"

ys.stats.W <- data.frame(low = jags.out$jm$q2.5$y[,2],
                         median = jags.out$jm$q50$y[,2],
                         high = jags.out$jm$q97.5$y[,2])

# Because data were padded to non-existing portion of W data (before 2006), time
# has to be provided from JM data.
ys.stats.W$time <- jags.out$data.JM$data.1$Frac.Year
# fill in NAs when there was no data
ys.stats.W$obsY <- c(rep(NA, 
                         (12*(nrow(jags.out$data.JM$jags.data2$y) - nrow(jags.out$data.W$jags.data2$y)))),
                     jags.out$data.W$data.1$Nests)
ys.stats.W$month <- jags.out$data.JM$data.1$Month
ys.stats.W$year <- jags.out$data.JM$data.1$Year
ys.stats.W$Season <- jags.out$data.JM$data.1$Season
ys.stats.W$location <- "Wermon"

#ys.stats <- rbind(ys.stats.JM, ys.stats.W)
# # extract Xs - the state model
# 
# Xs.stats.JM <- data.frame(low = jags.out$jm$q2.5$X[,1],
#                           median = jags.out$jm$q50$X[,1],
#                           high = jags.out$jm$q97.5$X[,1])
# 
# Xs.stats.JM$time <- jags.out$data.JM$data.1$Frac.Year
# Xs.stats.JM$obsY <- jags.out$data.JM$data.1$Nests
# Xs.stats.JM$month <- jags.out$data.JM$data.1$Month
# Xs.stats.JM$year <- jags.out$data.JM$data.1$Year
# Xs.stats.JM$Season <- jags.out$data.JM$data.1$Season
# Xs.stats.JM$location <- "Jamursba-Medi"
# 
# Xs.stats.W <- data.frame(low = jags.out$jm$q2.5$X[,2],
#                          median = jags.out$jm$q50$X[,2],
#                          high = jags.out$jm$q97.5$X[,2])
# Xs.stats.W$location <- "Wermon"
# 
# # Again, using data.JM is correct - just picking up the Frac.Year variable
# Xs.stats.W$time <- jags.out$data.JM$data.1$Frac.Year
# Xs.stats.W$obsY <- c(rep(NA, 
#                          (12*(nrow(jags.out$data.JM$jags.data2$y) - nrow(jags.out$data.W$jags.data2$y)))),
#                      jags.out$data.W$data.1$Nests)
# Xs.stats.W$month <- jags.out$data.JM$data.1$Month
# Xs.stats.W$year <- jags.out$data.JM$data.1$Year
# Xs.stats.W$Season <- jags.out$data.JM$data.1$Season
# 
# Xs.stats <- rbind(Xs.stats.JM, Xs.stats.W)
# 
# Ns.stats.JM <- data.frame(low = jags.out$jm$q2.5$N[,1],
#                           median = jags.out$jm$q50$N[,1],
#                           high = jags.out$jm$q97.5$N[,1],
#                           Season = year.begin.JM:year.end)
# Ns.stats.JM$location <- "Jamursba-Medi"
# 
# Ns.stats.W <- data.frame(low = jags.out$jm$q2.5$N[,2],
#                          median = jags.out$jm$q50$N[,2],
#                          high = jags.out$jm$q97.5$N[,2],
#                          Season = year.begin.JM:year.end)
# Ns.stats.W$location <- "Wermon"
# 
# Ns.stats <- rbind(Ns.stats.JM, Ns.stats.W)
# 
#Save results if needed

# results.all <- list(jm = jags.out$jm,
#                     Xs.stats = Xs.stats,
#                     ys.stats = ys.stats,
#                     Ns.stats = Ns.stats)

df.year.JM <- data.frame(start = seq(from = 2001.292, to = 2017.292, by = 1),
                         end = seq(from = 2002.208, to = 2018.208, by = 1),
                         season = seq(from = 2001, to = 2017, by = 1)) %>%
  mutate(odd.year = ifelse(season%%2 == 1, "Y", "N"))

p1 <- ggplot() +
  geom_rect(data = df.year.JM,
            aes(xmin = start, 
                xmax = end, 
                ymin = 0, 
                ymax = 1000, fill = odd.year),
            alpha = 0.5) +
  scale_fill_manual(values = c("Y" = "khaki", "N" = NA)) +
  geom_point(data = ys.stats.JM,
             aes(x = time, y = exp(median)),
             color = impute.color,
             size = 2) + 
  # geom_path(data = ys.stats.JM,
  #           aes(x = time, y = exp(median)),
  #           color = impute.color,
  #           size = 1) + 
  geom_point(data = jags.out$data.JM$data.1,
             aes(x = Frac.Year, y = Nests),
             color = color.JM,
             size = 2) + 
  geom_path(data = jags.out$data.JM$data.1,
            aes(x = Frac.Year, y = Nests),
            color = color.JM,
            size = 1) +
  geom_ribbon(data = ys.stats.JM,
             aes(x = time, ymin = exp(low), ymax = exp(high)),
             fill = "midnightblue",
             alpha = 0.5) + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold")) + 
  labs(y = "Number of nests", title = "Beach 1")

df.year.W <- data.frame(start = seq(from = 2006.292, to = 2017.292, by = 1),
                        end = seq(from = 2007.208, to = 2018.208, by = 1),
                        season = seq(from = 2006, to = 2017, by = 1)) %>%
  mutate(odd.year = ifelse(season%%2 == 1, "Y", "N"))

ys.stats.W %>% filter(Season >= 2006) ->  ys.stats.W
p2 <- ggplot() +
  geom_rect(data = df.year.W,
            aes(xmin = start, 
                xmax = end, 
                ymin = 0, 
                ymax = 300, fill = odd.year),
            alpha = 0.5) +
  scale_fill_manual(values = c("Y" = "khaki", "N" = NA)) +
  geom_point(data = ys.stats.W,
             aes(x = time, y = exp(median)),
             color = impute.color,
             size = 2) + 
  # geom_path(data = ys.stats.JM,
  #           aes(x = time, y = exp(median)),
  #           color = impute.color,
  #           size = 1) + 
  geom_point(data = jags.out$data.W$data.1,
             aes(x = Frac.Year, y = Nests),
             color = color.W,
             size = 2) + 
  geom_path(data = jags.out$data.W$data.1,
            aes(x = Frac.Year, y = Nests),
            color = color.W,
            size = 1) +
  geom_ribbon(data = ys.stats.W,
              aes(x = time, ymin = exp(low), ymax = exp(high)),
              fill = "midnightblue",
              alpha = 0.5) + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold")) + 
  labs(y = "Number of nests", title = "Beach 2")

#save plots if requested

if (save.fig){
  ggsave(filename = paste0("figures/Beach1_imputation.png"),
          plot = p1,
          dpi = 600,
          device = "png",
         width = 6.29, height = 3.68, units = "in")
  
  ggsave(filename = paste0("figures/Beach2_imputation.png"),
         plot = p2,
         dpi = 600,
         device = "png",
         width = 6.29, height = 3.68, units = "in")
  
}
