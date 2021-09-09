# https://www.zoopla.co.uk/new-homes/developers/england/?q=England&page=93
setwd("D:/Google drive local/second paper/")
library(tidyverse)
library(readxl)
library(pdftools)
library(tidyr)
library(plm)
#__layout > div > main > div.dev-search-results__wrapper > div > div:nth-child(2) > ul > li:nth-child(1) > article > a
 
zpla_newhomes_urls <- "https://www.zoopla.co.uk/new-homes/developers/england/?q=England&page=" %>% 
  str_c(., 1:93)

zpla_newhomes_list <- vector(mode = "list", length = length(zpla_newhomes_urls))
for (i in 1:93) {
  cat("\nPage:", i)
  zpla_newhomes_list[[i]] <- read_html(zpla_newhomes_urls[i]) %>%
    html_nodes(css = "main div.dev-search-results__wrapper div div ul li article a") %>%
    html_attr("href") %>%
    str_c("https://www.zoopla.co.uk", .)

  Sys.sleep(rexp(1, 2))
}

zpla_newhomes_list <- unlist(zpla_newhomes_list)

newhomes_df <- tibble::tibble(urls = zpla_newhomes_list) %>% 
  mutate(htb = rep(NA_character_, length(zpla_newhomes_list)), 
         developer = rep(NA_character_, length(zpla_newhomes_list)))

for (i in 89:length(zpla_newhomes_list)) {
  cat("\t:", i)
  r_tmp <- read_html(zpla_newhomes_list[i]) 
  
  newhomes_df$htb[i] <- r_tmp %>% 
    html_nodes(css = ".dev-ldp__htb-link") %>% 
    {length(.) == 1}
  
  newhomes_df$developer[i] <- r_tmp %>% 
    html_nodes(css = "li.breadcrumbs__crumb.breadcrumbs__crumb--active") %>% 
    html_text()
  
  Sys.sleep(rexp(1, 0.4))
}

newhomes_df$builders <- newhomes_df$developer %>% 
  str_remove("\\s\\-\\s.*$")

htb_builder_raw <- newhomes_df %>% select(builders, htb) %>% 
  distinct(builders, htb) %>% 
  nest_by(builders) %>% 
  mutate(htb_or_not_flag = sapply(data, length, simplify = T))
  
htb_builders <- htb_builder_raw %>% 
  tidyr::unnest() %>% 
  filter(htb_or_not_flag == 2 || htb == T) %>% 
  select(-c(htb_or_not_flag, htb)) %>% 
  distinct(builders)


nonhtb_builders <- htb_builder_raw %>% 
  tidyr::unnest() %>% 
  filter(htb_or_not_flag == 1 & htb == F) %>% 
  select(-c(htb_or_not_flag, htb)) %>% 
  distinct(builders)

htb_builder$data[1]
#__________________________________________________________________________________________
load("htb.RData")
pacman::p_load(dplyr, httr, rvest, stringr)

data_comfirm_zoopla <- htb_builders$builders

# Scotland htb builder from https://www.mygov.scot/help-to-buy/registered-builders/

data_comfirm_scot <- read_html("https://www.mygov.scot/help-to-buy/registered-builders/") %>% 
  html_nodes(".body-content li") %>% 
  html_text()


# Wales htb builder from https://www.mygov.scot/help-to-buy/registered-builders/

data_comfirm_wals <- read_html("https://gov.wales/help-buy-wales-participating-builders") %>% 
  html_nodes("#quick_guide--body li") %>% 
  html_text()

data_comfirm_ALL <- append(append(data_comfirm_scot, data_comfirm_wals), data_comfirm_zoopla) %>% 
  str_replace_all("LTD|Ltd", "Limited") %>% 
  str_replace_all("&", "and") %>% 
  str_remove_all("\\s?Limited\\s?") %>% 
  str_remove_all("\\s?\\(.*\\)") %>% 
  unique() %>% 
  sort()

#Scotland builders from https://www.homesforscotland.com/Members-directory/Home-Builder-Members
dev_scot_list <- read_html("https://www.homesforscotland.com/Members-directory/Home-Builder-Members") %>% 
  html_nodes("#dnn_ctr2170_HtmlModule_lblContent > div > div a") %>% 
  html_text()

data_wait_check_ALL <- append(append(append(dev_hbf_list, dev_nh_list), nonhtb_builders$builders), dev_scot_list) %>% 
  str_replace_all("LTD|Ltd", "Limited") %>% 
  str_replace_all("&", "and") %>% 
  str_remove_all("\\s?Limited\\s?") %>% 
  str_remove_all("\\s?\\(.*\\)") %>% 
  unique() %>% 
  sort()

da <- as.data.frame(data_wait_check_ALL)
write.csv(da, "da.csv")
#导出name去fame匹配成htb1real htb2real, 名字+regi, 下载到本地是htb1 htb2
#data_wait_check_ALL再来一遍，导出name去fame匹配成waitreal

#____________________________________________________________________________________________________________
#把确认htb的放进FAME找regi no, 加名字，htb =1成新数据
htb1 <- read_excel("htb1.xlsx", sheet = "Results")
htb2 <- read_excel("htb2.xlsx", sheet = "Results")
htb_with_regi <- rbind(htb1[2:3], htb2[2:3]) %>% 
  mutate(yn = rep(1)) %>% 
  rename(reg = `Registered number`) 

# #待查的放进FAME找regi no
# htbw <- read_excel("htb_w.xlsx", sheet = "Results")
# #根据regis num,把其中确认过htb删掉
# wait_with_regi <- as_tibble(htbw[2:3]) %>% 
#   rename(reg = `Registered number`) %>% 
#   mutate(True_htb = sapply(reg, function(x) ifelse(x %in% htb_with_regi$reg, T, F), simplify = T))
# #剩下wait_export是真的待查，存成wait_export.csv从此开始人工标注
# wait_export <- wait_with_regi %>% filter(True_htb == F) 

htbw_correction <- read_excel("waitreal.xlsx", sheet = "Results")
wait_with_regi_correction <- as_tibble(htbw_correction[2:3]) %>% 
  rename(reg = `Registered number`) %>% 
  mutate(True_htb = sapply(reg, function(x) ifelse(x %in% htb_with_regi$reg, T, F), simplify = T))
wait_export_correction <- wait_with_regi_correction %>% filter(True_htb == F) 

#
data_manual_check <- read_csv("wait_export.csv")
#wait_export加上01
check_finished <- wait_export_correction %>% 
  select(-True_htb) %>% 
  full_join(data_manual_check, by = "Company name") %>% 
mutate(True_htb = sapply(reg, function(x) ifelse(x %in% htb_with_regi$reg, T, F), simplify = T))
second_check <- check_finished %>% filter(True_htb == F) %>% select(-True_htb)
#存成"second_check.csv"
second_check <- read_csv("second_check.csv")
second_check <- second_check %>% select(-X1)

#和htb合并，终表832个公司，叫UK.csv, regi有点问题，手动补全8位
data_ALL <- rbind(second_check, htb_with_regi) 
#write_csv(data_ALL, "UK.csv")

data_yn <- read_csv("UK.csv")
uk_regi_token <- data_yn %>% select(reg) %>% mutate(reg = str_pad(data_yn$reg, 8, pad = "0",  side = c("left")))
write_csv(uk_regi_token, "uk_regi_token.csv") #用notepad打开这个去fame选变量

#去fame下载了发现很多turnover都没有，二次筛选turnover有数据的公司, 手动排列选298个公司,
uk_all_fivemore <- read_excel("uk_turnover.xlsx", sheet = "Results", na = c("", "n.a."))
uk_all_fivemore <- uk_all_fivemore %>% select(contains("Turn"), contains("Regi")) 
uk_all_fivemore$na_count <- apply(uk_all_fivemore, 1, function(x) sum(is.na(x)))

check_hand <- filter(uk_all_fivemore, na_count<=7) %>% select(contains("Regi"))
write_csv(check_hand, "realtotal.csv") #298公司

# 去fame在myview1变量的基础上增加
# "net_tangible_assets_liab_"
# "depreciation"              "amortisation"              "cost_of_sales"            
# "time_dummy"               

#ireland ____________________________________________________________________________________________
# htbird1 <- read_excel("htbird1.xlsx", sheet = "Results")
# htbird2 <- read_excel("htbird2.xlsx", sheet = "Results")
# htbird3 <- read_excel("htbird3.xlsx", sheet = "Results")
# htb_ird <- rbind(htbird1[2:3], htbird2[2:3], htbird3[2:3]) %>% 
#   mutate(yn = rep(1)) %>% 
#   rename(reg = `Registered number`)
# write_csv(htb_ird, "IR.csv")

#UK.csv 是英国全体company name, reg, yn
#IR.csv 是ireland全体company name, reg, yn=1
