library(tidyverse)
library(magrittr)
#library(crayon)
library(lubridate)
library(doSNOW)
library(parallel)
library(Hmisc)
library(psych)
library(summarytools)
library(anytime)
setwd("D:/Google drive local/Financial constraints/Data/")

raw <- read_csv("bank_add_ready.csv", 
                col_types = cols(accounts_date = col_character(), 
                                 date_of_incorporation = col_character())) %>% 
  mutate(date_of_incorporation = anydate(date_of_incorporation), 
         accounts_date = anydate(accounts_date))  

newsmall <- select(raw, -c("profit_loss_before_interest_paid", "profit_loss_before_taxation"))
names(newsmall)
# [1] "registered_number"         "date_of_incorporation"     "accounts_date"            
# [4] "depreciation"              "net_tangible_assets_liab_" "profit_loss_after_tax"    
# [7] "total_assets"              "turnover"                  "tangible_assets"   
skimr::skim(newsmall)

#_____________________________________________________________________________
data_long <- newsmall %>% 
  mutate(tangible_assets = ifelse(is.na(tangible_assets), net_tangible_assets_liab_ * 0.44, tangible_assets)) %>% 
  # filter(!is.na(tangible_assets)) %>%
  mutate(tangible_assets = ifelse(tangible_assets <= 0, 0.001, tangible_assets)) %>% 
  mutate(depreciation = ifelse(is.na(depreciation), tangible_assets * 0.1, depreciation)) %>% 
  group_by(registered_number)

#____________ retain only companies that have n > 6 years of dat __  logs -------------------------------
data_truncated <- data_long
n_company <- numeric(20)
for (i in 1:20) {
  n_company[i] <- n_distinct(filter(data_truncated, n() > i)$registered_number)
}

plot(x = 1:20, y = n_company)

data_6more_obs <- data_truncated %>%
  mutate(S.base = ifelse(turnover < 0.01, 0.01, turnover),
         log.s = log(S.base)) %>%
  filter(n() > 6) %>% # change the number to select 
  ungroup

distinct(data_6more_obs, registered_number) %>% nrow() 

#bank 33027

#____________ match with gdp ----------------------------------------------------
data_interpolated <- data_6more_obs
rm(data_6more_obs,i, n_company) #leaving working panel with useful things only

gdp <- read_csv("GBRGDPDEFAISMEI.csv") %>%
  mutate(DATE = ymd(DATE)) %>% 
  set_colnames(c("date","gdp")) %>% 
  mutate(match = as.character(date), 
         # month = str_extract(match, regex("-\\d{2}-")),
         # month = str_remove_all(month, "-"),
         match = str_extract(match, regex("\\d{4}"))) %>% 
  # filter(month == "01") %>% 
  select(-date)

dat_gdp <- data_interpolated %>% 
  mutate(match = as.character(accounts_date), 
         match = str_extract(match, regex("\\d{4}"))) %>% 
  left_join(gdp, by = "match")
#glimpse(dat_gdp)  

#____________ mutate deflator_i+1 I,delts.s,K -------------------------------------------
#delta.s:change of turnover 
#log.s:log of turnover
#log.k:log of K
## 测试一个公司
company_ID <- distinct(dat_gdp, registered_number) %>% unlist 

calculate_K <- function(dat, id) {
  dat_temp <- filter(dat, registered_number == id) %>% 
    arrange(accounts_date)
  
  n <- nrow(dat_temp)
  
  dat_temp <- dat_temp %>% 
    mutate(I = tangible_assets - c(NA_real_, tangible_assets[-n]), 
           delta.s = log.s - c(NA_real_, log.s[-n])) 
  
  
  K <- dat_temp$tangible_assets
  
  for (i in 3:(n - 1)) {
    
    # i = 3
    K[i] <- K[i - 1] * 0.95 * dat_temp$gdp[i] / dat_temp$gdp[i - 1] + dat_temp$I[i - 1]
    K[i] <- ifelse(K[i] <= 0.001, 0.001, K[i])
  }
  
  dat_temp <- dat_temp %>% 
    mutate(K = K) %>% 
    .[-c(1, n),]
}


cl <- parallel::makeCluster(4)
registerDoSNOW(cl)
n <- length(company_ID)


pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
system.time({
  dat_with_K <- foreach(i = 1:n, .combine = 'rbind',
                        .packages = c("dplyr", "purrr"),
                        .options.snow = opts) %dopar%
    calculate_K(dat_gdp, company_ID[i])
})
close(pb)
stopCluster(cl)
rm(n, pb, cl, opts, progress)

# dat_with_K <- filter(dat_with_K, (abs(K) > 1) & (abs(K) < 100000))
#____________ age待加-------------------------------------------
dat_ <- dat_with_K %>%
  filter(!is.na(K)) %>% 
  mutate(CF = profit_loss_after_tax + depreciation,
         cfratio = CF/K,
         log.K = log(K),
         age = floor(difftime(accounts_date, date_of_incorporation, units = c("weeks")) / 52) %>% 
           as.integer, 
         accounts_year = accounts_date %>% 
           as.character %>% 
           str_extract(regex("\\d{4}")) %>% 
           as.integer)

distinct(dat_, registered_number) %>% 
  nrow() 

#bank 32964

#____________ add Dummy___________############
add_dummy <- function(data, year) {
  
  data_yearly <- data %>%
    filter(accounts_year == year)
  
  
  rank1 = rank(data_yearly$total_assets)
  rank2 = rank(data_yearly$age)
  rank3 = rank(data_yearly$cfratio)
  
  index <- nrow(data_yearly)
  
  result <- data_yearly %>%
    mutate(
      
      size1 = ifelse(rank1 < 0.25 * index, 1, 0),
      size2 = ifelse(rank1 < 0.75 * index & rank1 >= 0.25 * index, 1, 0), 
      size3 = ifelse(rank1 >= 0.75 * index, 1, 0),
      
      age1 = ifelse(rank2 < 0.25 * index, 1, 0),
      age2 = ifelse(rank2 < 0.75 * index & rank2 >= 0.25 * index, 1, 0), 
      age3 = ifelse(rank2 >= 0.75 * index, 1, 0),
      
      CFratio1 = ifelse(cfratio < 0, 1, 0),
      CFratio3 = ifelse(rank3 >= 0.75 * index, 1, 0),
      CFratio2 = ifelse((CFratio1 + CFratio3) == 0, 1, 0))
}


years <- 1982:2019
data_w_dummy <- map_dfr(years, ~ add_dummy(dat_, .)) 

#____________ add lag -----------------------------------------------------
add_lagterm <- function(data, company) {
  data_company <- data %>%
    filter(registered_number == company) 
  
  len <- nrow(data_company)
  
  #attach(data_company$) 
  K1 <- c(NA_real_, data_company$K[-len])
  I1 <- c(NA_real_, data_company$I[-len])
  deltas1 <- c(NA_real_, data_company$delta.s[-len])
  K2 <- c(NA_real_,NA_real_, data_company$K[-c((len-1),len)])
  logk2 <- c(NA_real_,NA_real_, data_company$log.K[-c((len-1),len)])
  logs2 <- c(NA_real_,NA_real_, data_company$log.s[-c((len-1),len)])
  new1 <- cbind(data_company, K1, I1, deltas1, K2, logk2, logs2)
  #new2 <- new1[complete.cases(new1),] 
  new3 <- new1
  
  IK1 <- new3$I/new3$K1
  CFK1 <- new3$CF/new3$K1
  k2s2 <- new3$logk2 - new3$logs2
  I1K2 <- new3$I1/new3$K2
  new4 <- cbind(new3, IK1, CFK1, k2s2, I1K2) 
  #new5 <- new4[complete.cases(new4),]
}
company_ID <- data_w_dummy %>% distinct(registered_number)
# 

data_w_lag <- map_dfr(company_ID$registered_number, ~ add_lagterm(data_w_dummy, .)) 

# test <- data_w_lag %>% select(registered_number, accounts_year, starts_with("K"), 
#                               log.K, logk2, log.s, logs2)

write_csv(data_w_lag, "bank_w_lag.csv")
#在这一步再回FAME检查data_w_lag里delta.s算的对不对