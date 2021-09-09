setwd("D:/Google drive local/second paper/")
library(tidyverse)
library(stringr)
library(purrr)
library(plm)
library(magrittr) 
library(lubridate)
library(doSNOW)
library(parallel)
library(Hmisc) 
library(summarytools)
library(anytime)
library(psych)

#____________________________________________________________________ ____________________________
data_long <- read_csv("data_ALL_fame_long_all.csv") %>% 
  rename(registered_number = reg) 

data_long <- data_long %>% 
  mutate(fix_tangible_asset = ifelse(fix_tangible_asset <= 0, 0.001, fix_tangible_asset)) %>% 
  group_by(registered_number) %>%
  mutate(S.base = ifelse(turnover < 0.01, 0.01, turnover),
         log.s = log(S.base),
         amortisation = ifelse(is.na(amortisation), 0, amortisation),
         depreciation = ifelse(is.na(depreciation), 0, depreciation))
#________________________________________________________________________________________________
names(data_long)

gdp <- read_csv("D:/Google drive local/Financial constraints/Data/GBRGDPDEFAISMEI.csv") %>%
  mutate(DATE = ymd(DATE)) %>% 
  set_colnames(c("date","gdp")) %>% 
  mutate(match = as.character(date), 
         match = str_extract(match, regex("\\d{4}"))) %>% 
  select(-date) 

dat_gdp <- data_long %>% 
  mutate(match = as.character(accounts_date), 
         match = str_extract(match, regex("\\d{4}"))) %>% 
  left_join(gdp, by = "match") 

#____________ mutate deflator_i+1 I,delts.s,K -------------------------------------------
company_ID <- distinct(dat_gdp, registered_number) %>% unlist 

calculate_K <- function(dat, id) {
  dat_temp <- filter(dat, registered_number == id) %>% 
    arrange(accounts_date)
  
  n <- nrow(dat_temp)
  
  dat_temp <- dat_temp %>% 
    mutate(I = fix_tangible_asset - c(NA_real_, fix_tangible_asset[-n]), 
           delta.s = log.s - c(NA_real_, log.s[-n])) 
  
  
  K <- dat_temp$fix_tangible_asset
  
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

#____________ 加K ___________________________________ 
dat_ <- dat_with_K %>%
  filter(!is.na(K)) %>% 
  mutate(CF = profit_loss_after_tax + depreciation + amortisation,
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

#____________ add lag -----------------------------------------------------
add_lagterm <- function(data, company) {
  data_company <- data %>%
    filter(registered_number == company) 
  
  len <- nrow(data_company)
  
  #attach(data_company$) 
  K1 <- c(NA_real_, data_company$K[-len])
  I1 <- c(NA_real_, data_company$I[-len])
  deltas1 <- c(NA_real_, data_company$delta.s[-len])
  new3 <- mutate(data_company, 
           "K1" = K1, 
           "I1" = I1, 
           "deltas1" = deltas1)
  IK1 <- new3$I/new3$K1
  CFK1 <- new3$CF/new3$K1
  new4 <- mutate(new3, 
                 "IK1" = IK1, 
                 "CFK1" = CFK1) 
}
company_ID <- dat_ %>% distinct(registered_number)

data_w_lag <- map_dfr(company_ID$registered_number, ~ add_lagterm(dat_, .)) 

write_csv(data_w_lag, "data_w_lag.csv")
#data_w_lag是给GMM用的

