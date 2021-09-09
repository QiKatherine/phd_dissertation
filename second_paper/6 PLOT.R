##______________________________________________________________________________
setwd("D:/Google drive local/second paper/819plot")
#library(plyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(parallel)
library(doSNOW)
library(readxl)

uk_code <- read_csv("postcodes.csv")
data_download <- read_excel("list_postcode.xlsx", sheet = "Results", na = c("", "n.a."))

yes_no_token <- read_csv("D:/Google drive local/second paper/UK.csv")
yes_no_token <- yes_no_token %>% 
  mutate(reg = str_pad(yes_no_token$reg, 8, pad = "0",  side = c("left"))) %>%
  select(reg, yn) %>% 
  rename(registered_number = reg) 

std_colnames <- function(data) {
  colnames(data) <- colnames(data) %>% 
    str_to_lower() %>% 
    str_remove_all("&") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("\\/", "_") %>%
    str_replace_all("\\.", "_") %>% 
    str_replace_all("\\n", "_") %>% 
    str_replace_all("_-_", "_") %>% 
    str_replace_all("__", "_") %>% 
    str_remove_all("\\(") %>%
    str_remove_all("\\)") 
  return(data)
}

data_download <- std_colnames(data_download)
uk_code <- std_colnames(uk_code)

data_download <- data_download %>% 
  left_join(yes_no_token, by = "registered_number") %>% 
  rename(postcode = r_o_full_postcode)

data_download[which(data_download$registered_number == "RS007561"), "postcode"] <- "CR0 9XP"
data_download[which(data_download$registered_number == "IE152387"), "postcode"] <- "E14 9SJ"
data_download[which(data_download$registered_number == "IP29573R"), "postcode"] <- "SN10 2AZ"
data_download[which(data_download$registered_number == "06081775"), "postcode"] <- "DH8 5XP"
data_download[which(data_download$registered_number == "IE047784"), "postcode"] <- "KA21 5DS"
data_download[which(data_download$registered_number == "JE127156"), "postcode"] <- "LL12 8UA"

list <- as.list(unique(data_download$postcode))

datamap <- subset(uk_code, uk_code$postcode %in% list, select= c("postcode", "latitude",  "longitude"))  

data_merge <- data_download %>% 
  left_join(datamap, by = "postcode") 
#write_csv(data_merge, "data_list_postcode.csv")
data_merge <- read_csv("data_list_postcode.csv") %>% 
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
#______________________________________________________________________________
#data_ALL_fame <- read_excel("819plot.xlsx", sheet = "Results", na = c("", "n.a."))
#回到2文件整个跑一遍，注意pwd是819plot
data_ALL_fame_long <- read_csv("data_ALL_fame_long_all.csv")
#回到5文件，从97行开始跑到112, I 此时以元为单位
data_trim <- data_w_I
#从148deflator开始继续跑到

deflator <- read_csv("D:/Google drive local/second paper/42120/deflator.csv")
deflator <- deflator %>% 
  mutate(year = stringr::str_extract(date, "\\d{4}")) %>% 
  group_by(year) %>% 
  summarise(input_mean = mean(input), output_mean = (output)) %>% 
  distinct(year, .keep_all = T) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 2010)

gdp <- read_csv("D:/Google drive local/Financial constraints/Data/GBRGDPDEFQISMEI.csv") %>%
  mutate(year = stringr::str_extract(as.character(lubridate::ymd(DATE)), "\\d{4}")) %>% 
  group_by(year) %>% 
  summarise(input_mean = mean(GBRGDPDEFQISMEI)) %>% 
  distinct(year, .keep_all = T) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 2010)

deflator1 <- ts(deflator$input_mean, start = 2010, end = 2020, frequency = 1)
deflator2 <- ts(deflator$output_mean, start = 2010, end = 2020, frequency = 1)
deflator3 <- ts(gdp$input_mean, start = 2010, end = 2020, frequency = 1)

transform_ts <- function(data, regi){
  
  my_ts <- purrr::partial(.f = ts, start = 2010, end = 2020, frequency = 1)
  
  data_try <- data.table::setDT(data) %>% 
    .[registered_number == regi] %>% 
    .[, c("turnover", "gross_profit", "stock_w_i_p", "profit_loss_before_interest_paid", 
          "I", "roce_denonminator", "cost_of_sales") := lapply(.SD, my_ts), 
      .SDcols = c("turnover", "gross_profit", "stock_w_i_p", "profit_loss_before_interest_paid",
                  "I", "roce_denonminator", "cost_of_sales")] %>% 
    
    .[, turnover := deflate(turnover, deflator2, type = "index")] %>% 
    .[, gross_profit := deflate(gross_profit, deflator2, type = "index")] %>% 
    .[, stock_w_i_p_df := deflate(stock_w_i_p, deflator1, type = "index")] %>% 
    .[, profit_loss_before_interest_paid := deflate(profit_loss_before_interest_paid, deflator2, type = "index")] %>%
    .[, I := deflate(I, deflator3, type = "index")] %>%
    .[, roce_denonminator := deflate(roce_denonminator, deflator2, type = "index")] %>% 
    .[, cost_of_sales := deflate(cost_of_sales, deflator1, type = "index")] %>%  
  
    select(registered_number, year, I, gross_profit, profit_loss_before_interest_paid, roce_denonminator,
           turnover, stock_w_i_p, yn, time_dummy, cost_of_sales)
}

cl <- makeCluster(4)
parallel::clusterExport(cl, varlist = c("transform_ts", "data_trim", "deflator2", "deflator1", "deflator3"))
clusterEvalQ(cl, expr = {library(dplyr)
  library(BETS)})

system.time(
  outputlist2 <- parLapplyLB(cl, unique(data_trim$registered_number), 
                             function(x) transform_ts(data = data_trim, regi = x))
)

stopCluster(cl)

final_dat <- data.table::rbindlist(outputlist2)

#unique(final_dat$registered_number) %>% length() #3428

final_dat1 <- final_dat %>% 
  dplyr::mutate(mk = 0.804 * as.vector(turnover)/(-as.vector(cost_of_sales))) %>% 
  dplyr::mutate(roce = as.vector(profit_loss_before_interest_paid)/as.vector(roce_denonminator))

#从这里开始考虑经纬度和距离，数据已经deflate, 先加载data_download和uk_code
uk_code <- read_csv("postcodes.csv")

data_download <- read_excel("list_postcode.xlsx", sheet = "Results", na = c("", "n.a."))

data_download <- std_colnames(data_download)
uk_code <- std_colnames(uk_code)

data_download <- data_download %>%
  rename(postcode = r_o_full_postcode) %>% 
  select(postcode, registered_number)

data_download[which(data_download$registered_number == "RS007561"), "postcode"] <- "CR0 9XP"
data_download[which(data_download$registered_number == "IE152387"), "postcode"] <- "E14 9SJ"
data_download[which(data_download$registered_number == "IP29573R"), "postcode"] <- "SN10 2AZ"
data_download[which(data_download$registered_number == "06081775"), "postcode"] <- "DH8 5XP"
data_download[which(data_download$registered_number == "IE047784"), "postcode"] <- "KA21 5DS"
data_download[which(data_download$registered_number == "JE127156"), "postcode"] <- "LL12 8UA"

list <- as.list(unique(data_download$postcode))

datamap <- subset(uk_code, uk_code$postcode %in% list, 
                  select= c("postcode", "latitude",  "longitude", "country", "district"))  

data_merge <- data_download %>% 
  left_join(datamap, by = "postcode") 

data_forols <- final_dat1 %>% 
  left_join(data_merge, by = "registered_number")   
#write_csv(data_forols, "data_forols.csv")

#________到此819清洗，有地标 mk roce____________________________________________________
data_forols <- read_csv("data_forols.csv")
# a <- filter(data_forols, abs(I)>1e07) #298
# a <- filter(data_forols, abs(I)>1e08) #60
# a <- filter(data_forols, abs(I)>2e08) #31 投资大于2千万
data_0 <- data_forols %>% filter(yn == 0) %>% group_by(year) %>% 
  summarise(across(c(turnover, gross_profit, stock_w_i_p, I, roce, mk), ~mean(.x, na.rm = TRUE))) %>% 
  mutate(turnover = turnover/1000,
         gross_profit = gross_profit/1000,
         stock_w_i_p = stock_w_i_p/1000,
         I = I/1000000,
         year = as.factor(year),
         yn = 0) 

data_1 <- data_forols %>% filter(yn == 1) %>% group_by(year) %>% 
  summarise(across(c(turnover, gross_profit, stock_w_i_p, I, roce, mk), ~mean(.x, na.rm = TRUE))) %>% 
  mutate(turnover = turnover/1000,
         gross_profit = gross_profit/1000,
         stock_w_i_p = stock_w_i_p/1000,
         I = I/1000000,
         year = as.factor(year),
         yn = 1)

data11 <- rbind(data_0, data_1) %>% 
  mutate(yn = as.factor(yn))
data11$yn <- factor(data11$yn, levels = rev(levels(data11$yn)))
  
#_______1____________________________________________________________________
theme_set(theme_minimal())
ggplot() + 
  geom_line(data = data11, aes(x = year, y = turnover, group = yn, color = yn, linetype = yn),size = 1.2) + 
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("1" = "#036564", "0" = "#CE3C4F"), labels = c("Yes", "No"), name = "Help to buy") +
  scale_linetype_manual(values=c("1" = "solid", "0" = "twodash"), labels = c("Yes", "No"), name = "Help to buy",) +   
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparison of deflated turnover means for two groups",
       x = "Year",
       y = "Average turnover (Milion Pounds)") +
  scale_y_continuous(labels=scales::dollar_format(prefix="£", suffix = "M"))

#_______2____________________________________________________________________
theme_set(theme_minimal())
ggplot() + 
  geom_line(data = data11, aes(x = year, y = gross_profit, group = yn, color = yn, linetype = yn),size = 1.2) + 
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("1" = "#036564", "0" = "#CE3C4F"), labels = c("Yes", "No"), name = "Help to buy") +
  scale_linetype_manual(values=c("1" = "solid", "0" = "twodash"), labels = c("Yes", "No"), name = "Help to buy",) +   
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparison of deflated gross profit means for two groups",
       x = "Year",
       y = "Average gross profit (Milion Pounds)") +
  scale_y_continuous(labels=scales::dollar_format(prefix="£", suffix = "M"))

#_______3____________________________________________________________________
theme_set(theme_minimal())
ggplot() + 
  geom_line(data = data11, aes(x = year, y = stock_w_i_p, group = yn, color = yn, linetype = yn),size = 1.2) + 
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("1" = "#036564", "0" = "#CE3C4F"), labels = c("Yes", "No"), name = "Help to buy") +
  scale_linetype_manual(values=c("1" = "solid", "0" = "twodash"), labels = c("Yes", "No"), name = "Help to buy",) +   
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparison of deflated inventory means for two groups",
       x = "Year",
       y = "Average inventory (Milion Pounds)") +
  scale_y_continuous(labels=scales::dollar_format(prefix="£", suffix = "M"))

#_______4____________________________________________________________________
theme_set(theme_minimal())
ggplot() + 
  geom_line(data = data11, aes(x = year, y = I, group = yn, color = yn, linetype = yn),size = 1.2) + 
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("1" = "#036564", "0" = "#CE3C4F"), labels = c("Yes", "No"), name = "Help to buy") +
  scale_linetype_manual(values=c("1" = "solid", "0" = "twodash"), labels = c("Yes", "No"), name = "Help to buy",) +   
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparison of deflated capital investment means for two groups",
       x = "Year",
       y = "Average capital investment (Milion Pounds)") +
  scale_y_continuous(labels=scales::dollar_format(prefix="£", suffix = "M"))

#_______5____________________________________________________________________
theme_set(theme_minimal())
ggplot() + 
  geom_line(data = data11, aes(x = year, y = roce, group = yn, color = yn, linetype = yn),size = 1.2) + 
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("1" = "#036564", "0" = "#CE3C4F"), labels = c("Yes", "No"), name = "Help to buy") +
  scale_linetype_manual(values=c("1" = "solid", "0" = "twodash"), labels = c("Yes", "No"), name = "Help to buy",) +   
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparison of deflated ROCE rate means for two groups",
       x = "Year",
       y = "Average ROCE rate") 
#_______ 6 mk density ____________________________________________________________________
#因为markup不能算均值，加权平均才有意义，我们先看看总density 在13和16年
data_forols <- read_csv("data_forols.csv")

data_low <- data_forols %>% filter(year == 2013) %>% 
  select(mk) %>% na.omit() %>% 
  mutate(time = "low")

data_high <- data_forols %>% filter(year == 2016)  %>% 
  select(mk) %>% na.omit() %>% 
  mutate(time = "high")

data_lh <- rbind(data_low, data_high) %>% 
  mutate(time = as.factor(time),mk = as.numeric(mk)) %>% 
  filter(mk < quantile(mk, 0.95, na.rm = T))

theme_set(theme_minimal())
ggplot(data = data_lh, aes(mk, group = time, color = time, linetype = time)) + 
  geom_density(size = 0.8) +
  scale_color_manual(values = c("low" = "#CE3C4F", "high" = "#036564"), 
                     labels = c("low" = "2013","high" = "2016"), name = "Year of density") +
  scale_linetype_manual(values=c("high" = "solid", "low" = "twodash"), 
                        labels = c("low" = "2013","high" = "2016"), name = "Year of density") +  
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparison of kernel density (unweighted)",
       x = "Marup values",
       y = "Kernel density of the markups") 

#—————— htb分开看density 手动改yn==0
data_forols1 <- read_csv("data_forols.csv") 

data_forols <- data_forols1 %>% filter(yn == 0)

data_low <- data_forols %>% filter(year == 2013) %>% 
  select(mk) %>% na.omit() %>% 
  mutate(time = "low")

data_high <- data_forols %>% filter(year == 2016)  %>% 
  select(mk) %>% na.omit() %>% 
  mutate(time = "high")

data_lh <- rbind(data_low, data_high) %>% 
  mutate(time = as.factor(time)) %>% 
  filter(mk < quantile(mk, 0.96, na.rm = T))

theme_set(theme_minimal())
ggplot(data = data_lh, aes(mk, group = time, color = time, linetype = time)) + 
  geom_density(size = 0.8) +
  scale_color_manual(values = c("low" = "#CE3C4F", "high" = "#036564"), 
                     labels = c("low" = "2013","high" = "2016"), name = "Year of density") +
  scale_linetype_manual(values=c("high" = "solid", "low" = "twodash"), 
                        labels = c("low" = "2013","high" = "2016"), name = "Year of density") +  
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Comparison of kernel density (unweighted)",
       x = "Marup values",
       y = "Kernel density of the markups") 

#_________htb non-htb 用cost sales分别weighted
data_forols <- read_csv("data_forols.csv") 
data_2 <- data_forols %>% 
  filter(mk < quantile(mk, 0.97, na.rm = T)) %>% 
  mutate(yn = as.factor(yn)) 

#data_2$yn <- factor(data_2$yn, levels = rev(levels(data_2$yn)))

data_mk0 <- data_2 %>% filter(yn == 0) %>% 
  group_by(year) %>% 
  mutate(sumsale = sum(turnover, na.rm = T),
         #         totalcost = cost_of_sales + admin
         sumcost = sum(cost_of_sales, na.rm = T),
         saleportion = turnover/sumsale,
         costportion = cost_of_sales/sumcost,
         salemk = mk*saleportion,
         costmk = mk*costportion) %>%
  summarize(across(c(salemk, costmk), ~ sum(.x, na.rm = T)))
  
data_mk1 <- data_2 %>% filter(yn == 1) %>% 
  group_by(year) %>% 
  mutate(sumsale = sum(turnover, na.rm = T),
         #         totalcost = cost_of_sales + admin
         sumcost = sum(cost_of_sales, na.rm = T),
         saleportion = turnover/sumsale,
         costportion = cost_of_sales/sumcost,
         salemk = mk*saleportion,
         costmk = mk*costportion) %>%
  summarize(across(c(salemk, costmk), ~ sum(.x, na.rm = T)))

theme_set(theme_minimal())
ggplot() +
  geom_line(data = data_mk1, aes(x = as.factor(year), y = salemk, colour = "salemk1", group = 1), size = 1.2) +
  geom_line(data = data_mk0, aes(x = as.factor(year), y = salemk, colour = "salemk0", group = 1), size = 1.2) +
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("salemk1" = "#036564", "salemk0" = "#CE3C4F"),
                     labels = c("salemk1" = "Yes", "salemk0" = "No"), name = "Help-to-buy") +
  scale_linetype_manual(values=c("salemk1" = "solid", "salemk0" = "twodash"),
                        labels = c("salemk1" = "Yes", "salemk0" = "No"), name = "Help-to-buy") +
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Markups estimated in HtB and non-HtB groups (sales weighted)",
       x = "Year",
       y = "Aggragated markup")

theme_set(theme_minimal())
ggplot() +
  geom_line(data = data_mk0, aes(x = as.factor(year), y = salemk, colour = "sale", group = 1), size = 1.2) +
  geom_line(data = data_mk0, aes(x = as.factor(year), y = costmk, colour = "cost", group = 1), size = 1.2) +
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("sale" = "#036564", "cost" = "#CE3C4F"),
                     labels = c("sale" = "Sales weighted", "cost" = "Cost weighted"), name = "Weighting methods") +
  scale_linetype_manual(values=c("sale" = "solid", "cost" = "twodash"),
                        labels = c("sale" = "Sales weighted", "cost" = "Cost weighted"), name = "Weighting methods") +
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Non H-t-B group:markups estimated with different weighting methods",
       x = "Year",
       y = "Aggragated markup")

theme_set(theme_minimal())
ggplot() +
  geom_line(data = data_mk1, aes(x = as.factor(year), y = salemk, colour = "sale", group = 1), size = 1.2) +
  geom_line(data = data_mk1, aes(x = as.factor(year), y = costmk, colour = "cost", group = 1), size = 1.2) +
  geom_vline(aes(xintercept = '2014'), linetype="solid", color = "black") +
  scale_color_manual(values=c("sale" = "#036564", "cost" = "#CE3C4F"),
                     labels = c("sale" = "Sales weighted", "cost" = "Cost weighted"), name = "Weighting methods") +
  scale_linetype_manual(values=c("sale" = "solid", "cost" = "twodash"),
                        labels = c("sale" = "Sales weighted", "cost" = "Cost weighted"), name = "Weighting methods") +
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "H-t-B group: markups estimated with different weighting methods",
       x = "Year",
       y = "Aggragated markup")



#_____________________ 开始 大小问题 ________________________________________
data_forols <- read_csv("data_forols.csv") 
data_2 <- data_forols %>% 
  mutate(yn = as.factor(yn)) #%>% 
filter(mk < quantile(mk, 0.99, na.rm = T)) 

data_mk0 <- data_2 %>% filter(mk > quantile(mk, 0.80, na.rm = T)) 

Hmisc::describe(data_mk0$roce_denonminator)
Hmisc::describe(data_mk1$roce_denonminator)
hist(data_mk0$turnover)
hist(data_mk1$turnover)


