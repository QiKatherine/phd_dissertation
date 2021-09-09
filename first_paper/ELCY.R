
pacman::p_load(tidyverse, skimr)

setwd("D:/Google drive local/Financial constraints/Data/")

# customized summary function --------------------------------------------------
smry_cons <- read_csv("smry_stats_cons.csv")
smry_manu <- read_csv("smry_stats_manu.csv")
smry_bank <- read_csv("smry_stats_bank.csv")

new_data <- smry_bank %>% filter(X1 %in% c("IK1_mean" , "CFK1_mean"))

 # a:cofficient_CF, b:mu_I, c:mu_CF
elaciticty <- function(a,b,c){
  (a/b)/(1/c)
}


#small
small_elac <- elaciticty(0.058, 0.19, 2.65)
small_elac





