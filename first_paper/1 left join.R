pacman::p_load(tidyverse)
setwd("D:/Google drive local/Financial constraints/Data/")
cons_raw <- readRDS("uk_40w_construction_raw_long.rds")
manu_raw <- readRDS("uk_6w_manufac_raw_long.rds")
bank_raw <- readRDS("uk_16w_control_raw_long.rds")

names(cons_raw)
# [1] "registered_number"                "date_of_incorporation"            "accounts_date"                    "depreciation"                    
# [5] "net_tangible_assets_liab_"        "profit_loss_after_tax"            "profit_loss_before_interest_paid" "profit_loss_before_taxation"     
# [9] "total_assets" "turnover"     
Hmisc::describe(cons_raw)

#______________ 以上是最原始的数据，以下用turnover扔掉90% na _______

cons_long <- cons_raw %>% 
  filter(!is.na(turnover)) 


manu_long <- manu_raw %>% 
  filter(!is.na(turnover))
 
bank_long <- bank_raw %>% 
  filter(!is.na(turnover)) 

rm(cons_raw, manu_raw, bank_raw)

cons_fix <- read_csv("cons_fix_long.csv") %>% filter(!is.na(accounts_date))
manu_fix <- read_csv("manu_fix_long.csv") %>% filter(!is.na(accounts_date)) 
bank_fix <- read_csv("bank_fix_long.csv") %>% filter(!is.na(accounts_date))
############## full = long <- fix ######################################

cons_full <- cons_long %>% 
  left_join(cons_fix, by = c("registered_number", "accounts_date")) 

manu_full <- manu_long %>% 
  left_join(manu_fix, by = c("registered_number", "accounts_date")) 

bank_full <- bank_long %>% 
  left_join(bank_fix, by = c("registered_number", "accounts_date")) 

write_csv(cons_full, "cons_add_ready.csv")
write_csv(manu_full, "manu_add_ready.csv")
write_csv(bank_full, "bank_add_ready.csv")
# 挨个检查一下full和FAME数据对的上不