library(tidyverse)
theme_set(theme_bw())
library(lme4)
library(lmerTest)

share <- read_csv("https://raw.githubusercontent.com/angelgardt/mk_ggplot2/master/sharexp_data.csv")
str(share)

is_outlier <- function(x) {!(x < mean(x) + 2.5 * sd(x) & x > mean(x) - 2.5 * sd(x))}

share %>% 
  group_by(id, trialtype, platform, setsize) %>% 
  mutate(trialtype = as_factor(trialtype),
         id = as_factor(id),
         platform = as_factor(platform),
         is_outlier = is_outlier(time1)) %>% 
  filter(trialtype != 'both' & !is_outlier) -> share

nrow(share)

fit1 <- glm(time1 ~ setsize, family = Gamma, data = share)
summary(fit1)

share$fitted1 <- fit1$fitted.values

share %>% 
  ggplot() +
  geom_point(aes(setsize, time1)) +
  geom_smooth(aes(setsize, fitted1), 
              method = "lm", 
              formula = y ~ x)

unique(share$id)

fit2 <- glm(time1 ~ setsize + id,
            family = Gamma,
            data = share)
summary(fit2)

share$fitted2 <- fit2$fitted.values

share %>% 
  ggplot() +
  geom_point(aes(setsize, time1, color = id)) +
  geom_smooth(aes(setsize, fitted2, color = id), method = 'lm')


mix1 <- lmer(time1 ~ setsize + (1|id), data = share, REML = FALSE)
summary(mix1)

mix2 <- lmer(time1 ~ setsize + (1 + setsize|id), data = share)
summary(mix2)

mix0 <- lmer(time1 ~ 1 + (1|id), data = share, REML = FALSE)

anova(mix0, mix1)

MuMIn::r.squaredGLMM(mix1)

mix3 <- lmer(time1 ~ setsize * trialtype + (1|id), data = share, REML = FALSE)
summary(mix3)

MuMIn::r.squaredGLMM(mix3)

mix3.1 <- lmer(time1 ~ setsize + trialtype + (1|id), data = share, REML = FALSE)
anova(mix3, mix3.1)

mix3.2 <- lmer(time1 ~ setsize + (1|id), data = share, REML = FALSE)
anova(mix3.1, mix3.2)


res <- tibble(fitted = fitted(mix1),
              resid = resid(mix3, type = "pearson"),
              sresid = resid(mix3, type = "pearson", scaled = TRUE))


ggplot(res, aes(fitted, sresid)) +
  geom_point() +
  geom_hline(yintercept = 0)

res %>% 
  ggplot(aes(x = share$id, y = sresid)) +
  geom_boxplot()

mix4 <- lmer(time1 ~ trialtype + (1|id), data = share)
summary(mix4)
anova(mix4)

