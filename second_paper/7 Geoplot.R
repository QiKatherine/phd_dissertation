setwd("D:/Google drive local/second paper/819plot")
#library(plyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(parallel)
library(doSNOW)
library(readxl)

#_____________________ 开始 地理问题 ________________________________________
data_forols <- read_csv("data_forols.csv") 
see <- data_forols %>% filter(mk > quantile(mk, 0.85, na.rm = T))
see <- data_forols %>% filter(turnover > quantile(turnover, 0.85, na.rm = T))
see <- data_forols %>% filter(profit_loss_before_interest_paid > quantile(profit_loss_before_interest_paid, 0.85, na.rm = T))
see <- data_forols %>% filter(roce_denonminator > quantile(roce_denonminator, 0.85, na.rm = T))

data_merge <- see %>% 
  mutate(yn = as.factor(yn))
data_merge$yn <- factor(data_merge$yn, levels = rev(levels(data_merge$yn)))

worldmap <-  map_data('world')
ggplot() + 
  geom_polygon(data = worldmap,
               aes(x = long, y = lat, group = group),
               fill = 'gray90', color = 'black') +
  coord_fixed(ratio = 1.9, xlim = c(-8,2), ylim = c(50.3, 59)) + 
  theme_void() + 
  geom_point(data = data_merge,
             aes(x = as.numeric(longitude),
                 y = as.numeric(latitude), color = yn), alpha = 0.9) +
  scale_color_manual(values=c("#036564", "#CE3C4F"), name = "Help to buy", labels = c("Yes", "No")) +
  scale_size_area(max_size = 20) +
  theme(legend.position = c(0.95, 0.75),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(4, 4, 4, 4)) + 
  theme(title = element_text(size = 12))

#_____________________________________________________________________________
data_forols <- read_csv("data_forols.csv") 
data_2 <- data_forols %>% 
  mutate(yn = as.factor(yn)) %>% 
filter(mk < quantile(mk, 0.99, na.rm = T)) 

whichq <- function(data){
  # l <-  length(sum(!is.na(data)))
  a <- rep(NA_real_, length(data))
  
  for (i in seq_along(data)) {
    if (is.na(data[i])) 
      a[i] = NA_integer_
    else if (data[i] <= quantile(data, 0.5, na.rm = T))
      a[i] = 1 
    else if (data[i] <= quantile(data, 0.75, na.rm = T))
      a[i] = 2
    else if (data[i] <= quantile(data, 0.90, na.rm = T))
      a[i] = 3
    else
      a[i] = 4
  }
  
  return(a)
}

data_mk00 <- data_2 %>% filter(yn == 0) %>% 
  group_by(year) %>% 
  mutate(qt = whichq(mk)) 
  
data_mk0 <- data_mk00 %>% group_by(year, qt) %>% 
  mutate(sumsale = sum(turnover, na.rm = T),
         #         totalcost = cost_of_sales + admin
         sumcost = sum(cost_of_sales, na.rm = T),
         saleportion = turnover/sumsale,
         costportion = cost_of_sales/sumcost,
         salemk = mk*saleportion,
         costmk = mk*costportion) %>%
  select(salemk) %>% 
  summarize(across(salemk, ~ sum(.x, na.rm = T)))

data_mk11 <- data_2 %>% filter(yn == 1) %>% 
  group_by(year) %>% 
  mutate(qt = whichq(mk)) 

dd1 <- data_mk0 %>% filter(qt == 1)
dd2 <- data_mk0 %>% filter(qt == 2)
dd3 <- data_mk0 %>% filter(qt == 3)
dd4 <- data_mk0 %>% filter(qt == 4)

theme_set(theme_minimal())
ggplot() +
  geom_line(data = dd1, aes(x = as.factor(year), y = salemk, colour = "X50", group = 1), size = 1.2) +
  geom_line(data = dd2, aes(x = as.factor(year), y = salemk, colour = "X75", group = 1), size = 1.2) +
#  geom_line(data = dd3, aes(x = as.factor(year), y = salemk, colour = "X90", group = 1), size = 1.2) +
#  geom_line(data = dd4, aes(x = as.factor(year), y = salemk, colour = "X95", group = 1), size = 1.2) +
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("X50" = "#E5BB81", "X75" = "#764D39",
                              "X90" = "#A11715","X95" = "#220807"),
                     labels = c("X50" = "X50", "X75" = "X75",
                                "X90" = "X90","X95" = "X100"),
                     name = "Weighting methods") +
  # scale_linetype_manual(values=c("sale" = "solid", "cost" = "twodash"),
  #                       labels = c("sale" = "Sales weighted", "cost" = "Cost weighted"), 
  #                       name = "Weighting methods") +
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Non H-t-B group: percentoles markup distribution (sales weighted)",
       x = "Year",
       y = "Sales weighted marup in quantiles")


data_mk11 <- data_2 %>% filter(yn == 1) %>% 
  group_by(year) %>% 
  mutate(qt = whichq(mk)) 

data_mk1 <- data_mk11 %>% group_by(year, qt) %>% 
  mutate(sumsale = sum(turnover, na.rm = T),
         #         totalcost = cost_of_sales + admin
         sumcost = sum(cost_of_sales, na.rm = T),
         saleportion = turnover/sumsale,
         costportion = cost_of_sales/sumcost,
         salemk = mk*saleportion,
         costmk = mk*costportion) %>%
  select(salemk) %>% 
  summarize(across(salemk, ~ sum(.x, na.rm = T)))

dd1 <- data_mk1 %>% filter(qt == 1)
dd2 <- data_mk1 %>% filter(qt == 2)
dd3 <- data_mk1 %>% filter(qt == 3)
dd4 <- data_mk1 %>% filter(qt == 4)

theme_set(theme_minimal())
ggplot() +
  geom_line(data = dd1, aes(x = as.factor(year), y = salemk, colour = "X50", group = 1), size = 1.2) +
  geom_line(data = dd2, aes(x = as.factor(year), y = salemk, colour = "X75", group = 1), size = 1.2) +
  geom_line(data = dd3, aes(x = as.factor(year), y = salemk, colour = "X90", group = 1), size = 1.2) +
  geom_line(data = dd4, aes(x = as.factor(year), y = salemk, colour = "X95", group = 1), size = 1.2) +
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("X50" = "#E5BB81", "X75" = "#764D39",
                              "X90" = "#A11715","X95" = "#220807"),
                     labels = c("X50" = "X50", "X75" = "X75",
                                "X90" = "X90","X95" = "X100"),
                     name = "Weighting methods") +
  # scale_linetype_manual(values=c("sale" = "solid", "cost" = "twodash"),
  #                       labels = c("sale" = "Sales weighted", "cost" = "Cost weighted"), 
  #                       name = "Weighting methods") +
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "H-t-B group: percentoles markup distribution (sales weighted)",
       x = "Year",
       y = "Sales weighted marup in quantiles")
