library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")
library(mgcv)

avocado <- read_csv("https://raw.githubusercontent.com/agricolamz/2019.08.01_ANDAN_GAM/master/data/avocado.csv")
str(avocado)

avocado$Date
avocado$Date2
avocado$AveragePrice
avocado$a_type %>% unique()

avocado %>% 
  filter(region == "Sacramento" & a_type == "organic") -> av_s_org

av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  # geom_smooth(method = "lm")
  geom_smooth()


poly1 <- lm(AveragePrice ~ poly(Date2, 2), av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = poly1$fitted.values), 
            color = "blue")

poly2 <- lm(AveragePrice ~ poly(Date2, 3), av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = poly2$fitted.values), 
            color = "blue")

poly3 <- lm(AveragePrice ~ poly(Date2, 7), av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = poly3$fitted.values), 
            color = "blue")

poly4 <- lm(AveragePrice ~ poly(Date2, 20), av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = poly4$fitted.values), 
            color = "blue")

poly2_fitted <- poly2$fitted.values
poly3_fitted <- poly3$fitted.values
poly4_fitted <- poly4$fitted.values

tibble(x = av_s_org$Date2,
       poly2 = poly1$fitted.values,
       poly3 = poly2_fitted,
       poly7 = poly3_fitted,
       poly20 = poly4_fitted) %>% 
  pivot_longer(cols = -x) %>% 
  ggplot() +
  geom_point(data = av_s_org, 
             aes(Date2, AveragePrice)) +
  geom_line(aes(x = x,
                y = value, 
                color = name))

summary(poly3)


gam1 <- gam(AveragePrice ~ s(Date2), data = av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = gam1$fitted.values), 
            color = "blue") +
  labs(caption = gam1$call)


gam2 <- gam(AveragePrice ~ s(Date2, sp = .1), 
            data = av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = gam2$fitted.values), 
            color = "blue") +
  labs(caption = gam2$call)

gam3 <- gam(AveragePrice ~ s(Date2, sp = 1), 
            data = av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = gam3$fitted.values), 
            color = "blue") +
  labs(caption = gam3$call)

gam4 <- gam(AveragePrice ~ s(Date2, k = 20), 
            data = av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = gam4$fitted.values), 
            color = "blue") +
  labs(caption = gam4$call)

gam5 <- gam(AveragePrice ~ s(Date2, k = 4), 
            data = av_s_org)
av_s_org %>% 
  ggplot(aes(Date2, AveragePrice)) +
  geom_point() +
  geom_line(aes(y = gam5$fitted.values), 
            color = "blue") +
  labs(caption = gam5$call)


avocado %>% 
  filter(region == "Sacramento") %>% 
  mutate(a_type = factor(a_type)) -> avocado_sacr

avocado_sacr %>% 
  ggplot(aes(Date2, AveragePrice, color = a_type)) +
  geom_point()

gam6 <- gam(AveragePrice ~ s(Date2) + a_type, 
            data = avocado_sacr)
summary(gam6)

tibble(x = avocado_sacr$Date2,
       a_type = avocado_sacr$a_type,
       y = gam6$fitted.values) %>% 
  ggplot() +
  geom_point(data = avocado_sacr,
             aes(Date2, AveragePrice, color = a_type)) +
  geom_line(aes(x = x, y = y, color = a_type))

gam7 <- gam(AveragePrice ~ s(Date2, by = a_type),
            data = avocado_sacr)
summary(gam7)

tibble(x = avocado_sacr$Date2,
       a_type = avocado_sacr$a_type,
       y = gam7$fitted.values) %>% 
  ggplot() +
  geom_point(data = avocado_sacr,
             aes(Date2, AveragePrice, color = a_type)) +
  geom_line(aes(x = x, y = y, color = a_type))


gam8 <- gam(AveragePrice ~ s(Date2, by = a_type) + a_type,
            data = avocado_sacr)
summary(gam8)

tibble(x = avocado_sacr$Date2,
       a_type = avocado_sacr$a_type,
       y = gam8$fitted.values) %>% 
  ggplot() +
  geom_point(data = avocado_sacr,
             aes(Date2, AveragePrice, color = a_type)) +
  geom_line(aes(x = x, y = y, color = a_type))


AIC(gam8)
AIC(gam7)

par(mfrow = c(2,2))
gam.check(gam5)

gam.check(gam8)

concurvity(gam8)



