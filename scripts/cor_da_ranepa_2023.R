# install.packages("tidyverse")
# install.packages("Hmisc")
# install.packages("corrplot")

library(tidyverse)

taia <- read_csv("https://raw.githubusercontent.com/angelgardt/taia/master/data/taia.csv")

str(taia)

taia |> # cmd + shift + M
  select(id, matches("[^gt]\\d{2}$")) |> 
  pivot_longer(cols = -id, names_to = "item", values_to = "score") |> 
  mutate(scale = str_remove_all(item, "\\d")) |> 
  group_by(id, scale) |> 
  summarise(scale_score = sum(score)) -> taia_scale_score

taia_scale_score |> 
  pivot_wider(names_from = scale, values_from = scale_score) |> 
  ungroup() |> 
  select(-id) |> 
  cor() |> 
  corrplot::corrplot.mixed()

taia_scale_score |> 
  summarise(total_score = sum(scale_score)) |> 
  full_join(#data
    taia |> select(id, gt_score)
    ) -> taia_gt

cor.test(taia_gt$total_score, taia_gt$gt_score)
cor.test(taia_gt$total_score, taia_gt$gt_score, method = "spearman")

# plot(taia_gt$total_score, taia_gt$gt_score)

taia_gt |> 
  ggplot(aes(total_score, gt_score)) +
  geom_point() +
  geom_smooth(method = "lm")




