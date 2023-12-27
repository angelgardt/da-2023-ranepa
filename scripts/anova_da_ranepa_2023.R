library(tidyverse)
library(ez)

share <- read_csv("https://raw.githubusercontent.com/angelgardt/mk_ggplot2/master/sharexp_data.csv")
str(share)

share$trialtype |> unique()

share |> 
  filter(trialtype != "both") |> 
  group_by(id, trialtype, setsize, platform) |> 
  summarise(rt = mean(time1)) |> 
  mutate(setsize = as_factor(setsize)) -> share

share$setsize

share

model1 <- aov(rt ~ platform, share)
summary(model1)

model1.1 <- lm(rt ~ platform, share)
summary(model1.1)
anova(model1.1)

ezANOVA(share, dv = rt, wid = id, within = trialtype, between = platform)

theme_set(theme_bw())

pd <- position_dodge(.5)

share |> 
  ggplot(aes(trialtype, rt, color = platform)) +
  stat_summary(fun = mean, geom = "point", 
               position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", 
               width = .3, position = pd)



vowels <- read_csv("vowels.csv")

vowels$reduction <- as_factor(vowels$reduction)

vowels |> 
  ggplot(aes(reduction, duration)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange")


vowels |> mutate(reduction = as_factor(reduction)) -> vowels

levels(vowels$reduction)

contrasts(vowels$reduction) <- c(1, 0, 0)

model2 <- aov(duration ~ reduction, data = vowels)
summary(model2)
summary(model2, split = list(reduction = list("first vs other" = 1)))


model3 <- aov(rt ~ setsize, share)
summary(model3)
TukeyHSD(model3)

pairwise.t.test(x = share$rt,
                g = share$setsize,
                p.adjust.method = "bonf")

pairwise.t.test(x = share$rt,
                g = share$setsize,
                p.adjust.method = "holm")

