# For ISTS 2020 presentation
# Creates a figure for the assumption of state-space model and discrete Fourier models
# constant variance.

rm(list = ls())
library(ggplot2)
library(ggridges)
library(tidyverse)

font.size <- 18
color.JM <- "orangered"
color.W <- "darkseagreen3"

# bring in the actual results:
DFS_results <- readRDS(file = "~/R/Indonesia_nesting/RData/time_series_Four_imputation_2019-08-29.rds")
DFS_summary <- DFS_results$jm$summary %>% 
  as.data.frame() %>%
  rownames_to_column(var = "variable")
rm(DFS_results)

#c.JM <- DFS_summary[grep(pattern = "c[/[]1,", x = DFS_summary$variable),] 
beta.sin.JM <- DFS_summary[grep(pattern = "beta.sin[/[]1[]/]", x = DFS_summary$variable),]
beta.cos.JM <- DFS_summary[grep(pattern = "beta.cos[/[]1[]/]", x = DFS_summary$variable),]

beta.sin.W <- DFS_summary[grep(pattern = "beta.sin[/[]2[]/]", x = DFS_summary$variable),]
beta.cos.W <- DFS_summary[grep(pattern = "beta.cos[/[]2[]/]", x = DFS_summary$variable),]

n.months <- 12
c.JM <- c.const.JM <- c.W <- c.const.W <-vector(mode = "numeric", length = n.months)
for (k in 1:n.months){
  c.const.JM[k] <- 2 * pi * k/12
  c.JM[k] <- beta.cos.JM$mean * cos(c.const.JM[k]) + beta.sin.JM$mean * sin(c.const.JM[k])
  c.const.W[k] <- 2 * pi * k/6
  c.W[k] <- beta.cos.W$mean * cos(c.const.W[k]) + beta.sin.W$mean * sin(c.const.W[k])
}

X0 <- 3
X.JM <- X.W <- vector(mode = "numeric", length = 12)
X.JM[1] <- X0 + c.JM[1]   # state mean
X.W[1] <- X0 + c.W[1]
for (t in 2:12){
  X.JM[t] <- c.JM[t] + X.JM[t-1]
  X.W[t] <- c.W[t] + X.W[t-1]
} 

sigma.X.JM <- DFS_summary[grep(pattern = "sigma.X[/[]1[]/]", x = DFS_summary$variable),]
sigma.X.W <- DFS_summary[grep(pattern = "sigma.X[/[]2[]/]", x = DFS_summary$variable),]

# set.seed(1234)
# n.random <- 5000
x.vals <- seq(from = 0.8, to = 6.5, by = 0.01)
X.JM.samples <- lapply(X.JM,  
                       FUN = function(x) dnorm(x = x.vals, 
                                               mean = x, 
                                               sd = sigma.X.W$mean)) %>%
  unlist() #%>% matrix(nrow = n.random, byrow = F)  

X.JM.df <- data.frame(month = rep(c(4:12, 1:3), each = length(x.vals)),
                      mean = rep(X.JM, each = length(x.vals)),
                      x.vals = rep(x.vals, 12),
                      density = X.JM.samples)

X.JM.mean.df <- data.frame(month = c(4:12, 1:3),
                           mean = X.JM) 

X.JM.mean.df <- X.JM.mean.df[order(X.JM.mean.df$month),]
                             
p.JM <- ggplot() + 
  geom_path(data = X.JM.mean.df,
            aes(x = mean, y = month),
            size = 2) + 
  geom_density_ridges(data = X.JM.df,
              aes(y = month, x = x.vals, height = density, group = month),
              stat = "identity",
              fill = color.JM,
              alpha = 0.8) +  
  scale_y_continuous(breaks = c(1:12),
                   labels = c("Jan", "Feb", "Mar", "Apr",
                              "May", "Jun", "Jul", "Aug",
                              "Sep", "Oct", "Nov", "Dec")) + 
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = font.size, face = "bold"),
        axis.text.x = element_blank())

ggsave(filename = "figures/JM_DFS_assumptions.png",
       device = "png", plot = p.JM, dpi = 600)

#set.seed(1234)

X.W.samples <- lapply(X.W,  
                      FUN = function(x) dnorm(x = x.vals, 
                                              mean = x, 
                                              sd = sigma.X.W$mean)) %>%
  unlist() #%>% matrix(nrow = n.random, byrow = F)  

X.W.df <- data.frame(month = rep(c(4:12, 1:3), each = length(x.vals)),
                     mean = rep(X.W, each = length(x.vals)),
                     x.vals = rep(x.vals, 12),
                     density = X.W.samples)

X.W.mean.df <- data.frame(month = c(4:12, 1:3),
                           mean = X.W)
X.W.mean.df <- X.W.mean.df[order(X.W.mean.df$month),]


p.W <- ggplot() +
  geom_path(data = X.W.mean.df,
            aes(x = mean, y = month),
            size = 2) + 
  geom_density_ridges(data = X.W.df,
                      aes(y = month, x = x.vals, height = density, group = month),
                      stat = "identity",
                      fill = color.W,
                      alpha = 0.8)+  
  scale_y_continuous(breaks = c(1:12),
                     labels = c("Jan", "Feb", "Mar", "Apr",
                                "May", "Jun", "Jul", "Aug",
                                "Sep", "Oct", "Nov", "Dec")) + 
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = font.size, face = "bold"),
        axis.text.x = element_blank())

p.W
ggsave(filename = "figures/W_DFS_assumptions.png",
       device = "png", plot = p.W, dpi = 600)

# ggplot(data = X.JM.df) + 
#   geom_vridgeline(aes(x = month, y = sample, width = ..density..),
#                   stat = "ydensity")
