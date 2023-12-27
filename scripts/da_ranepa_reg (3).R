library(tidyverse)

bottle <- read_csv("~/Downloads/archive/bottle.csv")
cast <- read_csv("~/Downloads/archive/cast.csv")
str(bottle)

nrow(bottle)

bottle |> 
    full_join(cast |> select(Cast_ID, Quarter, Cst_Cnt), by = "Cst_Cnt") |> #nrow()
    mutate(century = str_extract(Depth_ID, "^\\d{2}-\\d{4}")) |> # select(century)
    separate(century, into = c("century", "date"), sep = "-") |> # select(century, date) |> head()
    filter(century == 20) |> 
    mutate(quarter = as.character(Quarter)) |> 
    group_by(Cst_Cnt, quarter, date) |> 
    summarise(salinity = mean(Salnty),
              depth = mean(Depthm),
              temp = mean(T_degC)) -> btl_agg

# sum(is.na(btl_agg$Quarter))

btl_agg |> write_csv("CalCOFI.csv")

fit1 <- lm(temp ~ salinity, btl_agg)
summary(fit1)

fit2 <- lm(temp ~ salinity + depth, btl_agg)
summary(fit2)
car::vif(fit2)

fit3 <- lm(temp ~ salinity * depth, btl_agg)
summary(fit3)
car::vif(fit3)

fit4 <- lm(temp ~ salinity * quarter, btl_agg)
summary(fit4)
car::vif(fit4)



