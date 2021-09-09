rm(list = names(.GlobalEnv))
setwd("D:/Google drive local/second paper/819newall")
library(tidyverse)
library(readxl)
library(purrr)
library(data.table)
library(BETS)
# library(Hmisc)
library(magrittr)
library(lubridate)
library(parallel)
library(prodest)

data_ALL_fame_long_all <- read_csv("23tfpfull.csv")
#names(data_ALL_fame_long_all)

##————————————————————————— markup n DID ——————————————————————————————————————
data_ALL_fame_long_all <- data_ALL_fame_long_all[, roce := profit_loss_before_tax/(total_assets-current_liabilities)] 

# model1 <- plm(log(turnover) ~ time_dummy + yn + time_dummy:yn, data = data_ALL_fame_long_all, 
#               index = c("registered_number", "year"), 
#               model = "within", effect = "twoways")
# summary(model1) 

model1 <- lm(log(turnover) ~ time_dummy*yn + factor(year) + 
               factor(registered_number), data = data_ALL_fame_long_all)

model2 <- lm(log(gross_profit) ~ time_dummy*yn + factor(year) + 
               factor(registered_number), data = data_ALL_fame_long_all)

model3 <- lm(roce ~ time_dummy*yn + factor(year) + 
               factor(registered_number), data = data_ALL_fame_long_all)

model4 <-  lm(log(profit_loss_before_tax) ~ time_dummy*yn + factor(year) + 
                factor(registered_number), data = data_ALL_fame_long_all)

model5 <- lm(log(stock_w_i_p) ~ time_dummy*yn + factor(year) + 
               factor(registered_number), data = data_ALL_fame_long_all)

model6 <- lm(mk ~ time_dummy*yn + factor(year) + 
               factor(registered_number), data = data_ALL_fame_long_all)

stargazer(model1 , model2, model3, model4, model5, model6,type = "text", omit = "factor")

##——————————————————————————— GMM  —————————————————————————————————————————————
data_small <- data_ALL_fame_long_all %>% 
                 mutate(
                     no = ifelse(yn == 1, 0, 1),
                     before = ifelse(time_dummy == 1, 0, 1),
                     CFCF1 = CFK1 * no * before, 
                     CFCF2 = CFK1 * no * yn, 
                     CFCF3 = CFK1 * yn * time_dummy,
                     CFCF4 = CFK1 * yn * before) 
data_small <- data_small %>% 
  select(registered_number, year, IK1, delta.s, CFCF1, CFCF2, CFCF3, CFCF4)


model6 <- plm(IK1 ~ lag(delta.s, 1) + CFCF1 + CFCF2 + CFCF3 + CFCF4, 
              data = data_small,
              index = c("registered_number", "year"), 
              model = "within", effect = "twoways")
summary(model6)

stargazer(model6, type="text")

data_GMM <- read_csv("data_w_lag.csv")

data_small_indexed

model_cons_small <- pgmm(IK1 ~ lag(IK1, 1) + lag(delta.s, 0:1) + 
                           CFK1|lag(IK1, 2 : 5) +
                           lag(delta.s, 2:99) + lag(CFK1, 2:99),  
                         transformation = c("ld"),
                         data = data_small_indexed,  
                         effect = c("individual"), model = "twosteps")

summary(model_cons_small, robust = F)
