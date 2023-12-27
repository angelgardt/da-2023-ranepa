# install.packages("tidyverse")
# install.packages("Hmisc")

library(tidyverse)

taia <- read_csv("https://raw.githubusercontent.com/angelgardt/taia/master/data/taia.csv")

taia |> 
  select(id, matches("[^gt]\\d{2}$")) |> 
  pivot_longer(cols = -id, names_to = "item", values_to = "score") |> 
  mutate(scale = str_remove_all(item, "\\d") |> toupper()) |> 
  group_by(id, scale) |> 
  summarise(scale_score = sum(score)) |> 
  pivot_wider(names_from = scale, values_from = scale_score) -> taia_scale_score

corrplot::corrplot(cor(taia_scale_score |> ungroup() |> select(-id)))

taia_scale_score |> 
  pivot_longer(cols = -id, names_to = "scale", values_to = "score") |> 
  group_by(id) |> 
  summarise(total_score = sum(score)) -> taia_total_score

taia_total_score |> 
  full_join(taia |> select(id, gt_score),
            by = "id") -> taia_gt

taia_gt |> 
  ggplot(aes(total_score, gt_score)) +
  geom_point() +
  geom_smooth(method = "lm")

cor.test(taia_gt$total_score, taia_gt$gt_score)

