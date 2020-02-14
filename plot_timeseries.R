# plot time series


rm(list = ls())
library(ggplot2)
#library(ggridges)
library(tidyverse)

source("ISTS2020_funcs.R")



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
  geom_point(data = data.jags.JM$data.1,
             aes(x = Frac.Year, y = (Nests)),
             color = color.JM,
             size = 2) + 
  geom_path(data = data.jags.JM$data.1,
            aes(x = Frac.Year, y = (Nests)),
            color = color.JM,
            size = 1) + 
  # geom_text(data = data.frame(x = 2013, y = 900),
  #           aes(x = x, y = y, label = "Beach 1"),
  #           size = 6, fontface = "bold") +
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

p2 <- ggplot() + 
  geom_rect(data = df.year.W,
            aes(xmin = start, 
                xmax = end, 
                ymin = 0, 
                ymax = 300, fill = odd.year),
            alpha = 0.5) +
  scale_fill_manual(values = c("Y" = "khaki", "N" = NA)) +
  geom_point(data = data.jags.W$data.1,
             aes(x = Frac.Year, y = (Nests)),
             color = color.W,
             size = 2) + 
  geom_path(data = data.jags.W$data.1,
            aes(x = Frac.Year, y = (Nests)),
            color = color.W,
            size = 1) + 
  # geom_text(data = data.frame(x = 2014.5, y = 260),
  #           aes(x = x, y = y, label = "Beach 2"),
  #           size = 6, fontface = "bold") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold")) + 
  labs(y = "Number of nests", title = "Beach 2")


# per season plot:
p3 <- ggplot() + 
  geom_rect(data = data.frame(xmin = 4, xmax = 9, 
                              ymin = 0, ymax = 7),
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
            fill = fill.color.JM, alpha = 0.5) +
  
  geom_point(data = data.jags.JM$data.1,
            aes(x = Month, 
                y = log(Nests)),
            color = color.JM, size = 3) +
  scale_x_continuous(breaks = seq(1:12)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold")) + 
  labs(y = "log(Number of nests)", title = "Beach 1")

p4 <- ggplot() + 
  geom_rect(data = data.frame(xmin = 4, xmax = 9, 
                              ymin = 0, ymax = 7),
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
            fill = fill.color.W, alpha = 0.5) +
  geom_rect(data = data.frame(xmin = 1, xmax = 3, 
                              ymin = 0, ymax = 7),
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
            fill = fill.color.W, alpha = 0.5) +
  geom_rect(data = data.frame(xmin = 10, xmax = 12, 
                              ymin = 0, ymax = 7),
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
            fill = fill.color.W, alpha = 0.5) +
  scale_x_continuous(breaks = seq(1:12)) +
  geom_point(data = data.jags.W$data.1,
             aes(x = Month, 
                 y = log(Nests)),
             color = color.W, size = 3) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold")) + 
  labs(y = "log(Number of nests)", title = "Beach 2")

# ggsave(p1, filename = "figures/beach1_nests.png", device = "png", dpi = 600)
# 
# ggsave(p2, filename = "figures/beach2_nests.png", device = "png", dpi = 600)

ggsave(p3, filename = "figures/beach1_monthly_nests.png", device = "png", dpi = 600)

ggsave(p4, filename = "figures/beach2_monthly_nests.png", device = "png", dpi = 600)



