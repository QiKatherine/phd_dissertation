# ==============================================================================

# 此代码，编写时是用来合并英国 6w+ 制造业公司的FAME数据，并转换为 long format

# ==============================================================================

pacman::p_load(tidyverse, readxl, doSNOW, foreach)
setwd("D:/Google drive local/Financial constraints/Data/")

# 获取所有分块文件名

files <- list.files("D:/Google drive local/Financial constraints/Data/merge/")
# var_types <- rep("text", 164)


# ------------------------------------------------------------------------------
# 测试读取单个文件
# readxl包

test <- read_xlsx(path = str_c("merge/", files)[1],
                  sheet = "Results", 
                  
                  range = cell_cols("B:AQ"),
                  col_types = c(rep("text", 2), rep("date", 20),
                                rep("numeric", 20)),
                  na = c("", "n.a."))


# ------------------------------------------------------------------------------

# 合并为一个文件

system.time({
  bank <- map_dfr(str_c("merge/", files)[1:4], 
                  ~ read_xlsx(path = .x,
                              sheet = "Results", 
                              range = cell_cols("B:AQ"),
                              col_types = c(rep("text", 2), rep("date", 20),
                                            rep("numeric", 20)),
                              na = c("", "n.a.")))
})

system.time({
  cons <- map_dfr(str_c("merge/", files)[5:7], 
                  ~ read_xlsx(path = .x,
                              sheet = "Results", 
                              range = cell_cols("B:AQ"),
                              col_types = c(rep("text", 2), rep("date", 20),
                                            rep("numeric", 20)),
                              na = c("", "n.a.")))
})

system.time({
  manu <- map_dfr(str_c("merge/", files)[8:9], 
                  ~ read_xlsx(path = .x,
                              sheet = "Results", 
                              range = cell_cols("B:AQ"),
                              col_types = c(rep("text", 2), rep("date", 20),
                                            rep("numeric", 20)),
                              na = c("", "n.a.")))
})



# ------------------------------------------------------------------------------

# all <- readxl::read_xlsx(path = "D:/Google drive local/Financial constraints/Data/bank_ fixed_tangible.xlsx", 
#                      range = "Results!B1:AQ21239", 
#                      col_types = c(rep("text", 1), rep("date", 20),
#                                    rep("numeric", 20), "text"),
#                      na = c("", "n.a."))


# 标准化变量名
std_colnames <- function(data) {
  colnames(data) <- colnames(data) %>% 
    str_to_lower() %>% 
    str_replace_all(" ", "_") %>% 
    str_replace_all("\\.", "_") %>% 
    str_replace_all("\\n", "_") %>% 
    str_replace_all("_-_", "_") %>% 
    str_replace_all("__", "_") %>% 
    str_remove_all("\\(") %>%
    str_remove_all("\\)") %>%
    str_replace_all("last_avail_yr", "year_0") %>% 
    str_replace_all("accounts_date_year_", "accounts_date_th_gbp_year_")
  
  return(data)
}

bank <- std_colnames(bank)
cons <- std_colnames(cons)
manu <- std_colnames(manu)

names(bank)
names(cons)
names(manu)


# ------------------------------------------------------------------------------

# 把相应的日期变量改为date类变量
# library(lubridate)
# all[,c(2:21)] <- map_dfc(all[,c(2:21)], ~ as_date(.x, origin = "1899/12/30"))
# test <- all[,163] %>% as.integer %>% as_date(origin = "1899/12/30")

# filter(all, !str_detect(date_of_incorporation, "/")) %>% dim
# # 416324    163
# filter(all, str_detect(date_of_incorporation, "/")) %>% dim
# # 37 163
# filter(all, is.na(date_of_incorporation)) %>% dim
# # 1 163

# all <-
#   bind_rows(filter(all, str_detect(date_of_incorporation, "/")) %>%
#               mutate(date_of_incorporation = dmy(date_of_incorporation)),
#             filter(all, !str_detect(date_of_incorporation, "/")) %>%
#               mutate(date_of_incorporation =
#                        as_date(as.integer(date_of_incorporation),
#                                origin = "1899/12/30")))

# all_unique <- distinct(all, .keep_all = T)
# rm(all)



# ------------------------------------------------------------------------------

long_format <- function(id, data = all_unique) {
  
  # data <- bank
  
  # id = "00617987"
  # 选出一个公司
  uk_cpm_1 <- data %>%
    filter(registered_number == id)
    
  n <- nrow(uk_cpm_1)
    
  uk_cpm_1 <- uk_cpm_1[n,] %>% 
    mutate(i = row_number()) %>% 
    gather(key, value, -1, -2, -43)
  
  uk_cpm_1_date <- uk_cpm_1 %>% 
    filter(str_detect(key, "accounts_date_")) %>% 
    mutate(year = str_remove(key, "accounts_date_")) %>% 
    select(-key) %>% 
    select(accounts_date = value, everything()) %>% 
    mutate(accounts_date = anytime::anydate(accounts_date))
  
  uk_cpm_long_1 <- uk_cpm_1 %>% 
    filter(!str_detect(key, "accounts_date_")) %>% 
    tidyr::extract(key, into = c("variables", "year"), 
                   regex = "(.*)_(th_gbp_year_\\d{1,2}$)") %>%
    tidyr::spread(variables, value) %>% 
    left_join(uk_cpm_1_date, by = c("registered_number", "year", 
                                    "company_name", "i")) %>% 
    select(-year, -i)
}

# bank fix to long format ------------------------------
bank_id <- distinct(bank, registered_number, .keep_all = F) %>% unlist
select(bank, registered_number) %>% unlist %>% duplicated %>% sum %>% 
  {. + length(id)}


# 测试function 转long format
# test <- long_format("00617987")
# filter(all_unique, registered_number == "00617987") %>% glimpse

cl <- makeCluster(6)
registerDoSNOW(cl)
n <- length(bank_id)

pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

Sys.time()
system.time({
  bank_fix_long <- foreach(i = 1:n, .combine = rbind,
                           .packages = c("dplyr", "stringr", "tidyr"), 
                           .errorhandling = 'remove', 
                           .inorder = F, 
                           # .multicombine = T, 
                           # .maxcombine = 500000, 
                           .options.snow = opts) %dopar%
    long_format(bank_id[i], bank)
})
Sys.time()


close(pb)
stopCluster(cl)
rm(n, pb, cl, opts, progress)


# cons fix to long format ------------------------------
cons_id <- distinct(cons, registered_number, .keep_all = F) %>% unlist
select(cons, registered_number) %>% unlist %>% duplicated %>% sum %>% 
  {. + length(id)}


cl <- makeCluster(6)
registerDoSNOW(cl)
n <- length(cons_id)

pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

Sys.time()
system.time({
  cons_fix_long <- foreach(i = 1:n, .combine = rbind,
                           .packages = c("dplyr", "stringr", "tidyr"), 
                           .errorhandling = 'remove', 
                           .inorder = F, 
                           # .multicombine = T, 
                           # .maxcombine = 500000, 
                           .options.snow = opts) %dopar%
    long_format(cons_id[i], cons)
})
Sys.time()


close(pb)
stopCluster(cl)
rm(n, pb, cl, opts, progress)

# manu fix to long format ------------------------------
manu_id <- distinct(manu, registered_number, .keep_all = F) %>% unlist
select(manu, registered_number) %>% unlist %>% duplicated %>% sum %>% 
  {. + length(id)}


cl <- makeCluster(6)
registerDoSNOW(cl)
n <- length(manu_id)

pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

Sys.time()
system.time({
  manu_fix_long <- foreach(i = 1:n, .combine = rbind,
                           .packages = c("dplyr", "stringr", "tidyr"), 
                           .errorhandling = 'remove', 
                           .inorder = F, 
                           # .multicombine = T, 
                           # .maxcombine = 500000, 
                           .options.snow = opts) %dopar%
    long_format(manu_id[i], manu)
})
Sys.time()


close(pb)
stopCluster(cl)
rm(n, pb, cl, opts, progress)

# end --------------------------------------------


# 批量测试
# system.time({
#   cpm_long_test_2 <- map_dfr(id[1:10000], ~ {tryCatch(long_format(., all_unique),
#                                                       error = function(e) cat(., "\n"))})
# })
# identical(arrange(cpm_long_test, company_name),
#           arrange(cpm_long_test_2, company_name))


# 
# bank_fix_long$accounts_date <- as_date(cpm_long$accounts_date)
# 
# UK_construction_long <- arrange(cpm_long, 
#                                 registered_number, desc(accounts_date)) %>% 
 

bank_fix_long %>% arrange(registered_number, accounts_date) %>% 
  select(-company_name) %>% write_csv("bank_fix_long.csv")

manu_fix_long %>% arrange(registered_number, accounts_date) %>% 
  select(-company_name) %>% write_csv("manu_fix_long.csv")

cons_fix_long %>% arrange(registered_number, accounts_date) %>% 
  select(-company_name) %>% write_csv("cons_fix_long.csv")
