library(Hmisc)
library(psych)
library(summarytools)
library(tidyverse)
library(skimr)
###______________   remove large outlier by skewness of summary stats___________________
###____ test area for trim threshold_________________
# skimr::skim(full_test_data)

trim <- function(data, test_variable, threshold){  
  trim_data <- data %>% 
    filter(abs(.[[test_variable]]) < threshold) 
  
  hist(trim_data[[test_variable]], breaks = 60)
  
  nrow(trim_data)  / nrow(data)
}

trim_2way <- function(data, test_variable, thresholds = c(-1e8, 1e8)){  
  trim_data <- data %>% 
    filter(.[[test_variable]] > thresholds[1]) %>% 
    filter(.[[test_variable]] < thresholds[2])
  
  hist(trim_data[[test_variable]], breaks = 60)
  
  nrow(trim_data)  / nrow(data)
}

#_____________________________   bank  ___________________________________________
raw_data <- read_csv("D:/Google drive local/Financial constraints/Data/bank_w_lag.csv") 

full_test_data <- select(raw_data, registered_number, accounts_year, tangible_assets, IK1, K, delta.s, k2s2,CFK1, I, age1, age2, age3,
                        size1, size2, size3, CFratio1, CFratio2, CFratio3)
skimr::skim(full_test_data)

full_test_data %>% trim("tangible_assets", 100000)
full_test_data %>% trim("IK1", 50)
full_test_data %>% trim_2way("K", c(-5000, 20000))
full_test_data %>% trim_2way("delta.s", c(-6,6))
full_test_data %>% trim_2way("k2s2", c(-100,10))
full_test_data %>% trim("CFK1", 100)

# threshrold put here 
trimmed_data <- full_test_data %>% 
  filter(abs(tangible_assets) < 100000,
         abs(IK1) < 10,
         abs(I) < 10000,
         k2s2 > -100,
         k2s2 < 10, 
         abs(CFK1) < 100,
         abs(delta.s) < 6)

skimr::skim(trimmed_data)
#两块一起用来调参，循环往复

data_reserve <- trimmed_data %>% distinct(registered_number, accounts_year, .keep_all = T)
skimr::skim(data_reserve)

write_csv(data_reserve, "D:/Google drive local/Financial constraints/Data/bank_trim.csv")
rm(list = ls())
#_____________________________   manufac  ___________________________________________
raw_data <- read_csv("D:/Google drive local/Financial constraints/Data/manu_w_lag.csv") 

full_test_data <- select(raw_data, registered_number, accounts_year, tangible_assets, IK1, K, delta.s, k2s2,CFK1, I, age1, age2, age3,
                         size1, size2, size3, CFratio1, CFratio2, CFratio3)
skimr::skim(full_test_data)

full_test_data %>% trim_2way("tangible_assets", c(0, 70000))
full_test_data %>% trim("IK1", 10)
full_test_data %>% trim_2way("K", c(-5000, 20000))
full_test_data %>% trim_2way("delta.s", c(-4,4))
full_test_data %>% trim_2way("k2s2", c(-100,10))
full_test_data %>% trim("CFK1", 10)
full_test_data %>% trim("K", 40000)

# threshrold put here 
trimmed_data <- full_test_data %>% 
  filter(abs(tangible_assets) < 70000,
         K < 40000,
         abs(CFK1) < 10,
         abs(IK1) < 50,
         k2s2 > -20,
         k2s2 < 10)


skimr::skim(trimmed_data)
#两块一起用来调参，循环往复

data_reserve <- trimmed_data %>% distinct(registered_number, accounts_year, .keep_all = T)
skimr::skim(data_reserve)

write_csv(data_reserve, "D:/Google drive local/Financial constraints/Data/manu_trim.csv")
rm(list = ls())
#_____________________________   constru  ___________________________________________
raw_data <- read_csv("D:/Google drive local/Financial constraints/Data/cons_w_lag.csv")

full_test_data <- select(raw_data, registered_number, accounts_year, tangible_assets, IK1, K, delta.s, k2s2,CFK1, I, age1, age2, age3,
                         size1, size2, size3, CFratio1, CFratio2, CFratio3)
skimr::skim(full_test_data)

full_test_data %>% trim_2way("tangible_assets", c(0, 70000))
full_test_data %>% trim("IK1", 10)
full_test_data %>% trim_2way("K", c(0, 20000))
full_test_data %>% trim_2way("delta.s", c(-4,4))
full_test_data %>% trim_2way("k2s2", c(-100,10))
full_test_data %>% trim("CFK1", 10000)
full_test_data %>% trim("K", 40000)

# threshrold put here 
trimmed_data <- full_test_data %>% 
  filter(abs(tangible_assets) < 70000,
         K < 20000,
         abs(CFK1) < 50,
         abs(IK1) < 50,
         abs(delta.s) < 10,
         k2s2 > -100,
         k2s2 < 10)


skimr::skim(trimmed_data)
#两块一起用来调参，循环往复

data_reserve <- trimmed_data %>% distinct(registered_number, accounts_year, .keep_all = T)
skimr::skim(data_reserve)

write_csv(data_reserve, "D:/Google drive local/Financial constraints/Data/cons_trim.csv")
rm(list = ls())

