library(tidyverse)
library(plm)
library(gmm)

#  ________________ data extrac  ______________________________________________ ###  ####
manu_full <- read_csv("D:/Google drive local/Financial constraints/Data/manu_trim.csv") %>% 
  mutate(registered_number = as.factor(registered_number))
# [1] "registered_number"        "accounts_year"            "net_tangible_assets_liab" "IK1"                      "delta.s"                 
# [6] "k2s2"                     "CFK1"                     "I"                        "age1"                     "age2"                    
# [11] "age3"                     "size1"                    "size2"                    "size3"                    "CFratio1"                
# [16] "CFratio2"                 "CFratio3"  

lag <- plm::lag
manu_CF <- mutate(manu_full, 
                     CFCF1 = CFK1 * CFratio1, 
                     CFCF2 = CFK1 * CFratio2, 
                     CFCF3 = CFK1 * CFratio3) %>% 
  select(registered_number, accounts_year, IK1, delta.s, k2s2, CFCF1, CFCF2, CFCF3)        

manu_size <- mutate(manu_full, 
                       CFSIZE1 = CFK1 * size1, 
                       CFSIZE2 = CFK1 * size2, 
                       CFSIZE3 = CFK1 * size3)%>% 
  select(registered_number, accounts_year, IK1, delta.s, k2s2, CFSIZE1, CFSIZE2, CFSIZE3)

manu_age <- mutate(manu_full, 
                      CFAGE1 = CFK1 * age1, 
                      CFAGE2 = CFK1 * age2,
                      CFAGE3 = CFK1 * age3)%>% 
  select(registered_number, accounts_year, IK1, delta.s, k2s2, CFAGE1, CFAGE2, CFAGE3)

#####----------------------     GMM  ------------------------ ###  
#### ______________________   overall  ____________________________________________##  
data_small <- select(manu_full, registered_number, accounts_year, IK1, delta.s, k2s2, CFK1) 

data_small_indexed <- pdata.frame(data_small, c("registered_number", "accounts_year"))

modell_manu_small <- pgmm(IK1 ~ lag(IK1, 1) + lag(delta.s, 0:1) + k2s2 + CFK1|lag(IK1, 2 : 99) + 
                            lag(k2s2, 0:99) + lag(delta.s, 2:99) + lag(CFK1, 2:99), 
                    data = data_small_indexed,
                    transformation = "ld", collapse = TRUE, effect = c("individual"))

summary(modell_manu_small, robust = F)

# sargan(model1)
# rv <- vcovHC(model1)
# mtest(model1, order = 2, vcov = rv)
#______________________   one dummy  ____________________________________________________
manu_CF_indexed <- pdata.frame(manu_CF, c("registered_number", "accounts_year"))
model_manu_CF <- pgmm(IK1 ~ lag(IK1, 1) + lag(delta.s, 0:1) + k2s2 + CFCF1 + CFCF2 + CFCF3|lag(IK1, 2 : 99) + 
                           lag(k2s2, 0:99) + lag(delta.s, 2:99) + lag(CFCF1, 2:99) + lag(CFCF2, 2:99) + lag(CFCF3, 2:99), 
                         data = manu_CF_indexed,
                         transformation = "ld", collapse = TRUE, effect = c("individual"))
summary(model_manu_CF, robust = F)


manu_size_indexed <- pdata.frame(manu_size, c("registered_number", "accounts_year"))
model_manu_size <- pgmm(IK1 ~ 1 + lag(IK1, 1) + lag(delta.s, 0:1) + k2s2 + CFSIZE1 + CFSIZE2 + CFSIZE3|lag(IK1, 2 : 99) + 
                             lag(k2s2, 0:99) + lag(delta.s, 2:99) + lag(CFSIZE1, 2:99) + lag(CFSIZE2, 2:99) +lag(CFSIZE3, 2:99), 
                           data = manu_size_indexed,
                           transformation = "ld", collapse = TRUE, effect = c("individual"))
summary(model_manu_size, robust = F)


manu_age_indexed <- pdata.frame(manu_age, c("registered_number", "accounts_year"))
model_manu_age <- pgmm(IK1 ~ 1 + lag(IK1, 1) + lag(delta.s, 0:1) + k2s2 + CFAGE1 + CFAGE2 + CFAGE3|lag(IK1, 2 : 99) + 
                            lag(k2s2, 0:99) + lag(delta.s, 2:99) + lag(CFAGE1, 2:99) + lag(CFAGE2, 2:99) +lag(CFAGE3, 2:99), 
                          data = manu_age_indexed,
                          transformation = "ld", collapse = TRUE, effect = c("individual"))
summary(model_manu_age, robust = F)

#_________________________ two dummy __________________________________________
saveRDS(modell_manu_small, "modell_manu_small.rds")
saveRDS(model_manu_CF, "model_manu_CF.rds")
saveRDS(model_manu_size, "model_manu_size.rds")
saveRDS(model_manu_age, "model_manu_age.rds")
