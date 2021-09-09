pacman::p_load(tidyverse, skimr)

setwd("D:/Google drive local/Financial constraints/Data/")

# customized summary function --------------------------------------------------

smry_stats <- function(data, var_to_smry = NULL) {

  # data <- constru
  
  if (is.null(var_to_smry)) 
    var_to_smry <- c("tangible_assets", "IK1", "delta.s", "k2s2", "CFK1")
  
  smry <- function(data, var_to_smry) {
    res <- data %>% 
      select(var_to_smry) %>% 
      # View() %>% 
      {data.frame(Mean = map_dbl(., ~ mean(.x, na.rm = T)), 
                 SD = map_dbl(., ~ sd(.x, na.rm = T)))} %>% 
      mutate_if(is.numeric, ~ round(.x, digits = 4) %>% as.character()) %>%
      mutate(space = "") %>% 
      mutate(SD = str_c("alibaba(", SD, ")")) %>% 
      as.matrix() %>% 
      t() %>% 
      map(~ rbind(.x)) %>% 
      unlist() %>% 
      c(., as.character(nrow(data))) %>% 
      as.data.frame()
      
    
    hangdemingzi <- str_c(rep(var_to_smry, each = 3), rep(c("_mean", "_sd", "__NA"), 5)) %>% 
      c(., c("Total N"))
    
    rownames(res) <- hangdemingzi
    
    # colnames(res) <- var_to_smry
    res
  }
      #mutate_if(is.numeric, as.character) %>% 
      # mutate(res = str_c(Mean, "\n(", SD, ")")) %>% 
      # select(res) %>% 
      # unlist %>% 
      # append(nrow(data)) %>% 
      # unname()
  
  `All firm-years` <- smry(data, var_to_smry)
  
  `Firm-years when SMALLSIZE_it=1` <- filter(data, size1 == 1) %>% 
    smry(var_to_smry)
  `Firm-years when MEDIUMSIZE_it=1` <- filter(data, size2 == 1) %>% 
    smry(var_to_smry)
  `Firm-years when LARGESIZE_it=1` <- filter(data, size3 == 1) %>% 
    smry(var_to_smry)
  
  `Firm-years when NEGCF_it= 1` <- filter(data, CFratio1 == 1) %>% 
    smry(var_to_smry)
  `Firm-years when MEDIUMCF_it= 1` <- filter(data, CFratio2 == 1) %>% 
    smry(var_to_smry)
  `Firm-years when LARGECF_it= 1` <- filter(data, CFratio3 == 1) %>% 
    smry(var_to_smry)
  
  `Firm-years when YOUNG_it=1` <- filter(data, age1 == 1) %>% 
    smry(var_to_smry)
  `Firm-years when MIDAGE_it=1` <- filter(data, age2 == 1) %>% 
    smry(var_to_smry)
  `Firm-years when OLD_it=1` <- filter(data, age3 == 1) %>% 
    smry(var_to_smry)
  
  smry_res <- cbind(`All firm-years`, 
                    `Firm-years when SMALLSIZE_it=1`, 
                    `Firm-years when MEDIUMSIZE_it=1`, 
                    `Firm-years when LARGESIZE_it=1`, 
                    `Firm-years when NEGCF_it= 1`, 
                    `Firm-years when MEDIUMCF_it= 1`, 
                    `Firm-years when LARGECF_it= 1`, 
                    `Firm-years when YOUNG_it=1`, 
                    `Firm-years when MIDAGE_it=1`, 
                    `Firm-years when OLD_it=1`) %>% 
    as.data.frame
  
  # rownames(smry_res) <- c(var_to_smry, "Total Obs.")
  
  return(smry_res)
}


# read data --------------------------------------------------------------------
constru <- read_csv("cons_trim.csv")
manufac <- read_csv("manu_trim.csv")
bank <- read_csv("bank_trim.csv")

smry_cons <- smry_stats(constru)
smry_manu <- smry_stats(manufac)
smry_bank <- smry_stats(bank)

write.csv(smry_stats(constru), "smry_stats_cons.csv")
write.csv(smry_stats(manufac), "smry_stats_manu.csv")
write.csv(smry_stats(bank), "smry_stats_bank.csv")

