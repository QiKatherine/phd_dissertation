library(tidyverse)
library(plm)
library(gmm)
lag <- plm::lag
#  ________________ data extrac  ______________________________________________ ###  ####
constru_full <- read_csv("D:/Google drive local/Financial constraints/Data/cons_trim.csv") %>% 
  mutate(registered_number = as.factor(registered_number))
# [1] "registered_number"        "accounts_year"            "net_tangible_assets_liab" "IK1"                      "delta.s"                 
# [6] "k2s2"                     "CFK1"                     "I"                        "age1"                     "age2"                    
# [11] "age3"                     "size1"                    "size2"                    "size3"                    "CFratio1"                
# [16] "CFratio2"                 "CFratio3"  

constru_CF <- mutate(constru_full, 
                     CFCF1 = CFK1 * CFratio1, 
                     CFCF2 = CFK1 * CFratio2, 
                     CFCF3 = CFK1 * CFratio3) %>% 
  select(registered_number, accounts_year, IK1, delta.s, k2s2, CFCF1, CFCF2, CFCF3)        

constru_size <- mutate(constru_full, 
                       CFSIZE1 = CFK1 * size1, 
                       CFSIZE2 = CFK1 * size2, 
                       CFSIZE3 = CFK1 * size3)%>% 
  select(registered_number, accounts_year, IK1, delta.s, k2s2, CFSIZE1, CFSIZE2, CFSIZE3)

constru_age <- mutate(constru_full, 
                      CFAGE1 = CFK1 * age1, 
                      CFAGE2 = CFK1 * age2,
                      CFAGE3 = CFK1 * age3)%>% 
  select(registered_number, accounts_year, IK1, delta.s, k2s2, CFAGE1, CFAGE2, CFAGE3)

#####----------------------     GMM  ------------------------ ###  
#### ______________________   overall  ____________________________________________##  
data_small <- select(constru_full, registered_number, accounts_year, IK1, delta.s, k2s2, CFK1) 

data_small_indexed <- pdata.frame(data_small, c("registered_number", "accounts_year"))

model_cons_small <- pgmm(IK1 ~ lag(IK1, 1) + lag(delta.s, 0:1) + k2s2 + CFK1|lag(IK1, 2 : 99) + 
               lag(k2s2, 0:99) + lag(delta.s, 2:99) + lag(CFK1, 2:99), 
               data = data_small_indexed,
               transformation = "ld", collapse = TRUE, effect = c("individual"))

summary(model_cons_small, robust = F)
# sargan(model1)
# rv <- vcovHC(model1)
# mtest(model1, order = 2, vcov = rv)
#______________________   one dummy  ____________________________________________________
constru_CF_indexed <- pdata.frame(constru_CF, c("registered_number", "accounts_year"))
model_cons_CF <- pgmm(IK1 ~ lag(IK1, 1) + lag(delta.s, 0:1) + k2s2 + CFCF1 + CFCF2 + CFCF3|lag(IK1, 2 : 99) + 
                 lag(k2s2, 0:99) + lag(delta.s, 2:99) + lag(CFCF1, 2:99) + lag(CFCF2, 2:99) + lag(CFCF3, 2:99), 
               data = constru_CF_indexed,
               transformation = "ld", collapse = TRUE, effect = c("individual"))
summary(model_cons_CF, robust = F)


constru_size_indexed <- pdata.frame(constru_size, c("registered_number", "accounts_year"))
model_cons_size <- pgmm(IK1 ~ 1 + lag(IK1, 1) + lag(delta.s, 0:1) + k2s2 + CFSIZE1 + CFSIZE2 + CFSIZE3|lag(IK1, 2 : 99) + 
                  lag(k2s2, 2:99) + lag(delta.s, 2:99) + lag(CFSIZE1, 2:99) + lag(CFSIZE2, 2:99) +lag(CFSIZE3, 2:99), 
                data = constru_size_indexed,
                transformation = "ld", collapse = TRUE, effect = c("individual"))
summary(model_cons_size, robust = F)


constru_age_indexed <- pdata.frame(constru_age, c("registered_number", "accounts_year"))
model_cons_age <- pgmm(IK1 ~ 1 + lag(IK1, 1) + lag(delta.s, 0:1) + k2s2 + CFAGE1 + CFAGE2 + CFAGE3|lag(IK1, 2 : 99) + 
                  lag(k2s2, 0:99) + lag(delta.s, 2:99) + lag(CFAGE1, 2:99) + lag(CFAGE2, 2:99) +lag(CFAGE3, 2:99), 
                data = constru_age_indexed,
                transformation = "ld", collapse = TRUE, effect = c("individual"))
summary(model_cons_age, robust = F)

#________________________________________________________________________________
saveRDS(model_cons_small, "model_cons_small.rds")
saveRDS(model_cons_CF, "model_cons_CF.rds")
saveRDS(model_cons_size, "model_cons_size.rds")
saveRDS(model_cons_age, "model_cons_age.rds")
