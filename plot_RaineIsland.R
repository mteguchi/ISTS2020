# make Raine Island plots

rm(list=ls())
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
source("ISTS2020_funcs.R")

col.def <- cols(Date = col_date(format = "%m/%d/%Y"),
                Counts_100m = col_double(),
                Season = col_integer(),
                Sector = col_character())

data.1 <- read_csv(file = "data/RaineIsland_Hatchling_Data_v2.csv",
                   col_types = col.def)

Day1 <- "12-20"
Day2 <- "04-30"

n.days <- as.numeric(as.Date(paste0("2018-", Day2)) - as.Date(paste0("2017-", Day1)))
seasons <- unique(data.1$Season)

data.1 %>% mutate(DOSeason = as.numeric(Date - as.Date(paste0(Season, "-", Day1)))) -> data.3

data.season <- data.frame(begin = as.Date(c("2014-12-21", "2015-12-21", 
                                            "2016-12-21", "2017-12-21")),
                          end = as.Date(c("2015-04-30", "2016-04-30",
                                          "2017-04-30", "2018-04-30")))
p1 <- ggplot() + 
  geom_rect(data = data.season,
            aes(xmin = begin, xmax = end,
                ymin = 0, ymax = 5000),
            fill = "khaki", alpha = 0.5) + 
  geom_point(data = data.1,
             aes(x = Date, y = Counts_100m)) +
  
  facet_grid(Sector ~.) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "italic"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(y = "Number of hatchlings")

# height and width specs don't seem to change the output here... 
ggsave(p1, filename = "figures/RaineIsland_data.png", 
       device = "png", dpi = 600)
#       height = 5.88, width = 10.04, units = "in")


# bring back the results
jm <- readRDS(paste0("RData/Girondot_Par_estimation_dnorm_", Day1, "_", Day2, ".rds"))
Sectors <- unique(data.3$Sector)

# Need to compute derived (Xs) values
Girondot_stats <- list(length = 5)
i <- j <- k <- 1
#k <- 50
for (j in 1:length(Sectors)){
  tmp.mat <- matrix(data = NA, nrow = n.days * length(seasons), ncol = 5)
  c <- 1
  for (i in 1:length(seasons)){
    
    for (k in 1:n.days){
      tmp <- Girondot_fcn(k, 
                          extract.samples("S", jm$samples), 
                          extract.samples("K", jm$samples), 
                          extract.samples(paste0("P[", i, "]"), jm$samples), 
                          1, 
                          extract.samples(paste0("max[", i, ",", j, "]"), jm$samples)) 
      tmp.mat[c, ] <- c(seasons[i], k, quantile(tmp, c(0.025, 0.5, 0.975)))
      c <- c + 1  
    }
  }
  tmp.df <- data.frame(tmp.mat)
  tmp.df$Sector <- as.factor(Sectors[j])
  colnames(tmp.df) <- c("Season", "DOSeason", "Xs_q2.5", "Xs_q50", "Xs_q97.5", "Sector")
  Girondot_stats[[j]] <- tmp.df
}

names(Girondot_stats) <-paste0("Xs_", Sectors)

Girondot.all.data <- do.call(rbind, Girondot_stats) %>%
  mutate(Date = as.Date(paste0(Season, "-", Day1)) + DOSeason - 1)

# need to NA 2017R prior to the 2017 season
Girondot.all.data[Girondot.all.data$Season < 2017 & Girondot.all.data$Sector == "2017R", 
                  c("Xs_q2.5", "Xs_q50", "Xs_q97.5")] <- NA

p2 <- ggplot() + 
  geom_rect(data = data.season,
            aes(xmin = begin, xmax = end,
                ymin = 0, ymax = 5000),
            fill = "khaki", alpha = 0.5) + 
  
  geom_path(data = Girondot.all.data,
            aes(x = Date, y = Xs_q50),
            size = 1.2) +
  geom_ribbon(data = Girondot.all.data , 
              aes(x = Date,
                  ymin = Xs_q2.5, ymax = Xs_q97.5),
              fill = "darkseagreen",
              alpha = 0.7,
              show.legend = F) +
  geom_point(data = data.1,
             aes(x = Date, y = Counts_100m)) +
  facet_grid(Sector ~.) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "italic"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(y = "Number of hatchlings")

p2
# height and width specs don't seem to change the output here... 
ggsave(p2, filename = "figures/RaineIsland_predicted.png", 
       device = "png", dpi = 600,
       height = 5.73, width = 9.78, units = "in")


ggplot() + 
  geom_path(data = Girondot.all.data,
            aes(x = DOSeason, 
                y = Xs_q50,
                color = as.factor(Season)),
            size = 1.2) +
  geom_ribbon(data = Girondot.all.data , 
              aes(x = DOSeason,
                  ymin = Xs_q2.5, 
                  ymax = Xs_q97.5,
                  fill = as.factor(Season)),
              alpha = 0.3,
              show.legend = F) +  
  geom_point(data = data.3,
             aes(x = DOSeason, 
                 y = (Counts_100m),
                 color = as.factor(Season)),
             size = 2, shape = 16) +
  facet_wrap(Sector ~., nrow = 3, scales = "free") + 
  labs(x = paste0("Days since ", Day1),  
       y = "Median counts and 95% CI",
       color = "Season") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 2,
                              label.theme = element_text(size = 13),
                              title.theme = element_text(size = 13, face = "bold"))) +
  theme(axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position = c(0.75, 0.1),
        legend.direction = "horizontal",
        strip.text = element_text(size = 12, face = "italic"))


                   