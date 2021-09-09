library(tidyverse)

#总结在aggrafate
initial_data <- read_csv("D:/Google drive local/Financial constraints/Data/aggragate/aggragateraw.csv")


summary_data <- initial_data %>% 
  group_by(`Bvd sectors`) %>% 
  summarise(turnover_mean = mean(turnover),
            profit_mean = mean(profit),
            assets_mean = mean(assets),
            turnover_per_assets_mean = mean(turnover_per_assets),
            profit_per_assets_mean = mean(profit_per_assets)
            ) %>% 
  arrange(desc(turnover_mean))

export_data <- summary_data[1:10, ]
write.csv(export_data, "export_data.csv")
