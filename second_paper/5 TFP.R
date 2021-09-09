setwd("D:/Google drive local/second paper/42120")
library(tidyverse)
library(stringr)
library(openxlsx)
library(purrr)
library(data.table)
library(BETS)
library(Hmisc)
library(magrittr)
library(lubridate)
library(parallel)
library(doSNOW)

#__________________________________________________________________________________________________
# data_ALL_fame1 <- read.xlsx("filter1.xlsx", sheet = "Results", na.strings = c(" ", "n.a."))
# data_ALL_fame2 <- read.xlsx("filter2.xlsx", sheet = "Results", na.strings = c(" ", "n.a."))
# 
# data_ALL_fame <- rbind(data_ALL_fame1, 
#                        data_ALL_fame2) %>% 
#   dplyr::filter(!is.na(Registered.number))
# 
# rm(data_ALL_fame1, data_ALL_fame2)
# 
# data_ALL_fame1 <- data_ALL_fame %>% select(Registered.number, contains("turnover"),-X1)
# data_ALL_fame1$na_c <- apply(data_ALL_fame1, 1, function(x) sum(is.na(x)))
# 
# aa <- data_ALL_fame1 %>% filter(na_c<5) %>% select(Registered.number)
# #write_csv(aa, "filterregi.csv") 
# #notepad找filterregi去fame找到公司下载十年

####___ download and try ols to delete missing data________________________________________________________
data_ALL_fame <- read.xlsx("export.xlsx", sheet = "Results", na.strings = c(" ","n.a.")) 

std_colnames <- function(data) {
  colnames(data) <- colnames(data) %>% 
    str_to_lower() %>% 
    str_remove_all("&") %>%
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

data_ALL_fame <- std_colnames(data_ALL_fame)

names_all <- data_ALL_fame %>% 
  select(contains("2020")) %>% 
  {names(.)} %>% 
  stringr::str_remove("_th_gbp_2020") %>% 
  str_remove("_2020")

#只把要转的variable单独提出来，n格子
sub_data <- vector(mode = "list", length = length(names_all))

for (i in seq_along(names_all)){
  sub_data[[i]] <- data_ALL_fame %>% 
    select("registered_number", contains(names_all[i]))
}

names(sub_data) <- names_all

my_pivot_w2l <- function(data, pvt_var) {
  res <- data %>% 
    pivot_longer(cols = starts_with(pvt_var), 
                 names_to = "year", names_prefix = "^.*\\(?=[0-9]{4}\\)",
                 values_to = pvt_var) %>% 
    mutate(year = str_extract(year, "\\d{4}")) 
}

#分别转成long format
sub_data_long <- map2(.x = sub_data, .y = names_all, .f = my_pivot_w2l)

data_ALL_fame_long <- sub_data_long[[1]]

#其它n列用function，转完立马合体

for(i in 2:length(sub_data_long)) {
  data_ALL_fame_long <- data_ALL_fame_long %>%
    left_join(sub_data_long[[i]])
  gc()
}

#把合并n块和原data合并
data_ALL_fame_long <- data_ALL_fame %>% 
  select(registered_number, company_name) %>% 
  distinct(registered_number, company_name) %>% 
  right_join(data_ALL_fame_long, by = "registered_number") %>% 
  rename(reg = registered_number) 
#——————————————————————————————————————————————————————————————————————————————————————————————————
data_long <- data_ALL_fame_long %>% 
  rename(registered_number = reg) %>%  
  filter(!is.na(registered_number)) %>%
  arrange(registered_number, year)

company_ID <- data_long %>% distinct(registered_number)

add_I <- function(data, company) {
  data_company <- data %>%
    filter(registered_number == company) 
  len <- nrow(data_company)
  data_company <- data_company %>% 
    mutate(fixed_assets1000 = fixed_assets*1000) %>% 
    mutate(I = (fixed_assets1000 - c(NA_real_, fixed_assets1000[-len])))}
  
data_w_I <- map_dfr(company_ID$registered_number, ~ add_I(data_long, .)) 

#___ 转完 清洗 *1000 deflate ________

#数指定列的列missing value
missing_count <- function(data, col1){
  
  data_list <- data$registered_number %>% unique()
  new <- c(NA_real_)
  data_t <- as.data.frame(cbind(data_list, new))
  
  for (i in 1:length(data_list)){
    
    data_t$new[i] <- data %>% 
      filter(registered_number == data_list[i]) %>% .[[col1]] %>% 
      {sum(is.na(.))}
  }
  return(data_t)
}

miss_fixed <- missing_count(data_w_I, "fixed_assets")
data_remove <- miss_fixed %>% filter(as.numeric(new) > 5) %>% .$data_list 

miss_fixed1 <- missing_count(data_w_I, "cost_of_sales")
data_remove1 <- miss_fixed1 %>% filter(as.numeric(new) > 6) %>% .$data_list 

miss_fixed2 <- missing_count(data_w_I, "number_of_employees")
data_remove2 <- miss_fixed2 %>% filter(as.numeric(new) > 6) %>% .$data_list 

data_trim <- data_w_I %>% 
  filter(!(registered_number %in% data_remove)) %>% 
  filter(!(registered_number %in% data_remove1)) %>% 
  filter(!(registered_number %in% data_remove2)) 
  
length(unique(data_trim$registered_number))  #3032

deflator <- read_csv("deflator.csv")
deflator <- deflator %>% 
  mutate(year = stringr::str_extract(date, "\\d{4}")) %>% 
  group_by(year) %>% 
  summarise(input_mean = mean(input), output_mean = (output)) %>% 
  distinct(year, .keep_all = T) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 2010)

gdp <- read_csv("D:/Google drive local/Financial constraints/Data/GBRGDPDEFQISMEI.csv") %>%
  mutate(year = stringr::str_extract(as.character(ymd(DATE)), "\\d{4}")) %>% 
  group_by(year) %>% 
  summarise(input_mean = mean(GBRGDPDEFQISMEI)) %>% 
  distinct(year, .keep_all = T) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 2010)

deflator1 <- ts(deflator$input_mean, start = 2010, end = 2020, frequency = 1)
deflator2 <- ts(deflator$output_mean, start = 2010, end = 2020, frequency = 1)
deflator3 <- ts(gdp$input_mean, start = 2010, end = 2020, frequency = 1)

transform_ts <- function(data, regi){
  
  my_ts <- purrr::partial(.f = ts, start = 2010, end = 2020, frequency = 1)
  
  data_try <- data.table::setDT(data) %>% 
    .[registered_number == regi] %>% 
    .[, c("fixed_assets", "turnover", "cost_of_sales", "stock_w_i_p") := lapply(.SD, "*", 1000), 
      .SDcols = c("fixed_assets", "turnover", "cost_of_sales", "stock_w_i_p")] %>% 
    .[, c("turnover", "cost_of_sales", "stock_w_i_p", "fixed_assets", "I") := lapply(.SD, my_ts), 
      .SDcols = c("turnover", "cost_of_sales", "stock_w_i_p", "fixed_assets", "I")] %>% 
    
    .[, turnover := deflate(turnover, deflator2, type = "index")] %>% 
    .[, cost_of_sales := deflate(cost_of_sales, deflator1, type = "index")] %>% 
    .[, stock_w_i_p := deflate(stock_w_i_p, deflator1, type = "index")] %>% 
    .[, fixed_assets := deflate(fixed_assets, deflator3, type = "index")] %>%
    .[, I := deflate(I, deflator3, type = "index")] %>%
    select(registered_number, year, fixed_assets, number_of_employees,
           turnover, cost_of_sales, stock_w_i_p, I)
}

cl <- makeCluster(4)
parallel::clusterExport(cl, varlist = c("transform_ts", "data_trim", "deflator2", "deflator1", "deflator3"))
clusterEvalQ(cl, expr = {library(dplyr)
  library(BETS)})

system.time(
  outputlist2 <- parLapplyLB(cl, unique(data_trim$registered_number), 
                             function(x) transform_ts(data = data_trim, regi = x))
)

stopCluster(cl)

final_dat <- rbindlist(outputlist2)

unique(final_dat$registered_number) %>% length() #3428
write_csv(final_dat, "final_dat.csv")

##--------------add log K--------------------------------------------------------
new_data <- read_csv("final_dat.csv")

remedy <- function(dat, col, id){
  test_data <- filter(dat, registered_number == id) %>% 
    arrange(year)
  
  predictions <- stats::predict(lm(test_data[[col]] ~ year, test_data), newdata = test_data)
  test_data[[col]][is.na(test_data[[col]])] <- predictions[is.na(test_data[[col]])]
  return(test_data)
}

company_ID <- distinct(new_data, registered_number) %>% unlist 

cl <- parallel::makeCluster(4)
registerDoSNOW(cl)
n <- length(company_ID)

pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
system.time({
  data_w_interpolate <- foreach(i = 1:n, .combine = 'rbind',
                        .packages = c("dplyr", "purrr"),
                        .options.snow = opts) %dopar%
    remedy(new_data,"fixed_assets" ,company_ID[i])
})

system.time({
  data_w_interpolate <- foreach(i = 1:n, .combine = 'rbind',
                        .packages = c("dplyr", "purrr"),
                        .options.snow = opts) %dopar%
    remedy(data_w_interpolate,"turnover" ,company_ID[i])
})

system.time({
  data_w_interpolate <- foreach(i = 1:n, .combine = 'rbind',
                         .packages = c("dplyr", "purrr"),
                         .options.snow = opts) %dopar%
    remedy(data_w_interpolate,"number_of_employees" ,company_ID[i])
})

system.time({
  data_w_interpolate <- foreach(i = 1:n, .combine = 'rbind',
                         .packages = c("dplyr", "purrr"),
                         .options.snow = opts) %dopar%
    remedy(data_w_interpolate,"cost_of_sales" ,company_ID[i])
})

close(pb)
stopCluster(cl)
rm(n, pb, cl, opts, progress)

Hmisc::describe(new_data)
Hmisc::describe(data_w_interpolate)

data_for_ml <- data_w_interpolate %>% 
  filter(turnover > 1,
         cost_of_sales < 0,
         fixed_assets > 1) %>% 
  mutate(stock_w_i_p = ifelse(is.na(stock_w_i_p), 0, stock_w_i_p),
         y = turnover - cost_of_sales - stock_w_i_p) %>% 
  mutate(turnover = log(turnover,),
         cost_of_sales = log(-cost_of_sales),
         number_of_employees = log(number_of_employees),
         lk = log(fixed_assets),
         ly = log(y)) %>% 
  select(registered_number, year, turnover, 
         number_of_employees, lk, stock_w_i_p, cost_of_sales, ly, I) 

write_csv(data_for_ml, "data_for_ml.csv")
Hmisc::describe(data_for_ml)

#__________________________________________________________________
setwd("D:/Google drive local/second paper/42120")
library(tidyverse)
library(prodest)
data_markup <- read_csv("data_for_ml.csv") %>% 
  select(ly, number_of_employees, lk, cost_of_sales, registered_number, year, I) 

data_small <- data_markup[complete.cases(data_markup), ]

df <- data_small %>% rename(output = ly,
                            firm = registered_number , 
                            labor = number_of_employees,
                            Materials = cost_of_sales,
                            capital = lk,
                            invest = I)

notmissing <- !is.na(df$ capital ) & !is.na(df$ invest )
df.nm <- subset (df , notmissing )
df.nm <- df.nm[order(df.nm$firm, df.nm$year),]

op1 <- lm(output ~ labor +  Materials + poly(cbind(capital, invest), degree = 4) * factor(year),
          data = df.nm)
# calculate fhat
b1 <- op1$coefficients[c("labor", "Materials")]
xb1 <- as.matrix(df.nm[ ,c("labor", "Materials")]) %*% b1
fhat <- stats::predict(op1, df.nm) - xb1

# function to lag fhat and capital
lag <- function (x, i = df.nm$firm, t = df.nm$year) {
  if (length (i) != length (x) || length (i) != length (t) ) {
    stop (" Inputs not same length ")
  }
  x.lag <- x [1:( length (x) -1)]
  x.lag[ i[1:( length (i) -1)] != i[2: length (i)] ] <- NA
  x.lag[ t[1:( length (i) -1)]+1 != t[2: length (i)] ] <- NA
  return (c(NA ,x.lag))
} 

#create data frame for step 2 regression
df.step2 <- data.frame(lhs = df.nm$output - xb1,
                       k = df.nm$capital,
                       fhat = fhat,
                       k.lag = lag(df.nm$capital),
                       f.lag =lag(fhat))
# drop missing observations because they mess up poly ()
df.step2 <- subset(df.step2, !apply(df.step2 , 1, function (x)
  any(is.na(x))))
# objective function = sum of residuals ^2
objective <- function (betaK, degree = 4) {
  op2 <- lm(I(lhs - betaK *k) ~ poly (I(f.lag - betaK * k.lag), degree),
            data = df.step2)
  return(sum(residuals(op2)^2))
}

opt.out <- optim(op1$coefficients ["capital"],
                 fn = objective , method ="Brent",lower =-1, upper =1)
b1
#  0.2261960+0.5778142 = 0.8040102
#____分年______________________________________________________________





