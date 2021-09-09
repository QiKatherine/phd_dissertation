# library
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(clusterprofiler)


# stats <- read_excel("D:/Google drive local/Financial constraints/1st paper/stats.xlsx", 
#                    + col_names = FALSE)

names(stats) <- c("Sector", "Turnover", "Profit", "Fixed tangible assets",
                "Turnover/Fixed tangible assets", "Profit/Fixed tangible assets")

dat <- stats %>% 
  pivot_longer(cols = 2:4, 
                names_to = "variable", 
                names_prefix = "v", 
                values_to = "value") %>% 
  mutate(cbPalette = ifelse(variable == "Turnover","#173207",variable ),
         cbPalette = ifelse(variable == "Profit","#173207",variable ),
         cbPalette = ifelse(variable == "Turnover","#173207",variable ))


if(dat$variable == "Turnover"){dat$cbPalette = "#173207"} 
  else if (dat$variable == "Profit"){dat$cbPalette = "#994D52"} 
  else {dat$cbPalette = "#994D52"}


ggplot() + 
  geom_bar(data = dat, mapping = aes(fill = variable, y = value, x = reorder(Sector, value)), position = "dodge", stat = "identity") + 
    scale_x_discrete(labels = function(Sector) str_wrap(Sector, width = 10)) + 
coord_flip()



dat2 <- stats %>% 
  pivot_longer(cols = 5:6, 
               names_to = "variable", 
               names_prefix = "v", 
               values_to = "value") %>% 
  mutate(cbPalette = rep(1))

ggplot() +
  geom_bar(data = dat2, mapping = aes(fill = variable, y = value, x = reorder(Sector, value)), position = "dodge", stat = "identity") + 
  scale_x_discrete(labels = function(Sector) str_wrap(Sector, width = 10)) + 
  coord_flip()
