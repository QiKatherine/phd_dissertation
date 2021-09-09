setwd("D:/Google drive local/second paper/")
library(tidyverse)
library(stringr)
library(readxl)
library(purrr)
library(openxlsx)

#download and try ols to delete missing data________________________________________________________
data_ALL_fame <- read.xlsx("uk_whole.xlsx", sheet = "Results", na.strings = c(" ", "n.a."))

std_colnames <- function(data) {
  colnames(data) <- colnames(data) %>% 
    str_to_lower() %>% 
    str_remove_all("&") %>%
    str_replace_all(" ", "_") %>% 
    str_replace_all("\\.", "_") %>% 
    str_replace_all("\\n", "_") %>% 
    str_replace_all("_-_", "_") %>% 
    str_replace_all("__", "_") %>% 
    str_remove_all("\\(") %>%
    str_remove_all("\\)") %>%
    str_replace_all("last_avail_yr", "year_0") %>% 
    str_replace_all("accounts_date_year_", "accounts_date_th_gbp_year_")
  
  return(data)
}

data_ALL_fame <- std_colnames(data_ALL_fame)

names_all <- data_ALL_fame %>% 
  select(contains("2020")) %>% 
  {names(.)} %>% 
  stringr::str_remove("_th_gbp_2020") %>% 
  str_remove("_2020")

#只把要转的variable单独提出来，n格子
sub_data <- vector(mode = "list", length = length(names_all))

for (i in seq_along(names_all)){
  sub_data[[i]] <- data_ALL_fame %>% 
    select("registered_number", contains(names_all[i]))
}

names(sub_data) <- names_all

my_pivot_w2l <- function(data, pvt_var) {
  res <- data %>% 
    pivot_longer(cols = starts_with(pvt_var), 
                 names_to = "year", names_prefix = "^.*\\(?=[0-9]{4}\\)",
                 values_to = pvt_var) %>% 
    mutate(year = str_extract(year, "\\d{4}"))}

#分别转成long format
sub_data_long <- map2(.x = sub_data, .y = names_all, .f = my_pivot_w2l)

#aa <- my_pivot_w2l(sub_data[[11]] , "number_of_employees")

# account_dates没有GBP, 单独操作
data_ALL_fame_long <- sub_data_long[[1]]

#其它n列用function，转完立马合体
for(i in 2:length(sub_data_long)) {
  data_ALL_fame_long <- data_ALL_fame_long %>% 
    left_join(sub_data_long[[i]])
}

#把合并n块和原data合并
data_ALL_fame_long <- data_ALL_fame %>% 
  select(registered_number, company_name, date_of_incorporation) %>% 
  distinct(registered_number, company_name, date_of_incorporation) %>% 
  right_join(data_ALL_fame_long, by = "registered_number") %>% 
  rename(reg = registered_number) 

#去原data里找到yn那一列，合并进来
data_yn <- read_csv("D:/Google drive local/second paper/UK.csv")
token_col <- data_yn %>% select(reg, yn) %>% 
  mutate(reg = str_pad(data_yn$reg, 8, pad = "0",  side = c("left")))

data_ALL_fame_long_all <- right_join(token_col, data_ALL_fame_long) %>% 
  mutate(time_dummy = ifelse(year <= 2014, 0, 1))

write_csv(data_ALL_fame_long_all, "data_ALL_fame_long_all.csv")
