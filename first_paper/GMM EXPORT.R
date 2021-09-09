# add_sig_level <- function(x) {
#   sig_level <- function(dat) {
#     
#     if (is.na(dat)) return("")
#     y <- as.numeric(dat)
#     if (y <= 0.001) return("***")
#     else if (y <= 0.01) return("**")
#     else if (y <= 0.05) return("*")
#     else return("")
#   }
#   map_chr(x, sig_level)
# }
# 

# library(tidyverse)
# library(broom)
library(gmm)
library(plm)
setwd("D:/Google drive local/Financial constraints/Data/")
model_manu_small <- readRDS("modell_manu_small.rds" )
model_manu_CF <- readRDS("model_manu_CF.rds")
model_manu_size <- readRDS("model_manu_size.rds")
model_manu_age <- readRDS("model_manu_age.rds")

summary(model_manu_CF)

model_bank_small <- readRDS("model_bank_small.rds")
model_bank_CF <- readRDS("model_bank_CF.rds")
model_bank_size <- readRDS("model_bank_size.rds")
model_bank_age <- readRDS("model_bank_age.rds")


model_cons_small <- readRDS("model_cons_small.rds")
model_cons_CF <- readRDS("model_cons_CF.rds")
model_cons_size <- readRDS("model_cons_size.rds")
model_cons_age <- readRDS("model_cons_age.rds")

# manu_srmy_small <- summary(model_manu_small)$coefficients[,1:2]

tidy_gmm <- function(md) {
  md_srmy <- summary(md)$coefficients[,1:2] %>% 
    as.data.frame() 
  
  vars <- rownames(md_srmy)
  
  md_srmy %>% 
    mutate(term = vars) %>%
    mutate_if(is.numeric, ~ as.character(round(.x, digits = 3))) %>% 
    select(term, everything())
}
 

manu_md_res <- tidy_gmm(model_manu_size) %>% 
  full_join(tidy_gmm(model_manu_CF), by = "term") %>% 
  full_join(tidy_gmm(model_manu_age), by = "term")
write.csv(manu_md_res, "gmm_manu.csv")

bank_md_res <- tidy_gmm(model_bank_size) %>% 
  full_join(tidy_gmm(model_bank_CF), by = "term") %>% 
  full_join(tidy_gmm(model_bank_age), by = "term")
write.csv(bank_md_res, "gmm_bank.csv")

cons_md_res <- tidy_gmm(model_cons_size) %>% 
  full_join(tidy_gmm(model_cons_CF), by = "term") %>% 
  full_join(tidy_gmm(model_cons_age), by = "term")
write.csv(cons_md_res, "gmm_cons.csv")


