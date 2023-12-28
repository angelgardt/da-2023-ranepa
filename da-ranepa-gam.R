library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")
library(mgcv)

avocado <- read_csv("https://raw.githubusercontent.com/agricolamz/2019.08.01_ANDAN_GAM/master/data/avocado.csv")
str(avocado)

avocado$Date %>% year() %>% unique()
avocado$Date2 %>% unique()

avocado %>% 
    filter(region == "Sacramento" & a_type == "organic") -> av_s_org

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(legend.position = "bottom")

poly2 <- lm(AveragePrice ~ poly(Date2, degree = 2), av_s_org)
poly2_fitted <- poly2$fitted.values

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = poly2_fitted), 
              color = "blue")

poly3 <- lm(AveragePrice ~ poly(Date2, degree = 2), av_s_org)
poly3_fitted <- poly3$fitted.values

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = poly3_fitted), 
              color = "blue")

poly4 <- lm(AveragePrice ~ poly(Date2, degree = 4), av_s_org)
poly4_fitted <- poly4$fitted.values

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = poly4_fitted), 
              color = "blue")

poly5 <- lm(AveragePrice ~ poly(Date2, degree = 5), av_s_org)
poly5_fitted <- poly5$fitted.values

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = poly5_fitted), 
              color = "blue")

poly10 <- lm(AveragePrice ~ poly(Date2, degree = 10), av_s_org)
poly10_fitted <- poly10$fitted.values

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = poly10_fitted), 
              color = "blue")

poly20 <- lm(AveragePrice ~ poly(Date2, degree = 20), av_s_org)
poly20_fitted <- poly20$fitted.values

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = poly20_fitted), 
              color = "blue")

summary(poly10)

tibble(
    x = av_s_org$Date2,
    poly2 = poly2$fitted.values,
    poly3 = poly3$fitted.values,
    poly4 = poly4$fitted.values,
    poly5 = poly5$fitted.values,
    poly10 = poly10$fitted.values,
    poly20 = poly20$fitted.values
) %>%
    pivot_longer(-x) -> fitted_poly
    

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(data = fitted_poly, aes(x, value, color = name))


gam1 <- gam(AveragePrice ~ s(Date2), data = av_s_org)

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = gam1$fitted.values), color = "blue")

summary(gam1)


gam2 <- gam(AveragePrice ~ s(Date2, sp = .1), data = av_s_org)

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = gam2$fitted.values), color = "blue")

summary(gam2)

gam3 <- gam(AveragePrice ~ s(Date2, sp = 1), data = av_s_org)

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = gam3$fitted.values), color = "blue")

summary(gam3)



gam4 <- gam(AveragePrice ~ s(Date2, k = 20), data = av_s_org)

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = gam4$fitted.values), color = "blue")

summary(gam4)

gam5 <- gam(AveragePrice ~ s(Date2, k = 4), data = av_s_org)

av_s_org %>% 
    ggplot(aes(Date2, AveragePrice)) +
    geom_point() +
    geom_line(aes(y = gam5$fitted.values), color = "blue")

summary(gam5)


avocado %>% 
    filter(region == "Sacramento") %>% 
    mutate(a_type = factor(a_type)) %>% 
    mutate(Date3 = Date2 + rnorm(nrow(avocado_sacramento), 3, 20)) -> avocado_sacramento

gam6.1 <- gam(AveragePrice ~ a_type, data = avocado_sacramento, method = "REML")
gam6.2 <- gam(AveragePrice ~ s(Date2) + s(Date3) + a_type, data = avocado_sacramento, method = "REML")
summary(gam6.1)
summary(gam6.2)

gam6 <- gam(AveragePrice ~ s(Date2)+a_type, data = avocado_sacramento, method = "REML")
summary(gam6)

anova(gam6, gam6.1, test = "F")
anova(gam6.1, gam6.2, test = "F")


tibble(x = avocado_sacramento$Date2,
       a_type = avocado_sacramento$a_type,
       fit = gam6$fitted.values) %>% 
    ggplot(aes(x, fit, color = a_type))+
    geom_line()+
    geom_point(data = avocado_sacramento, aes(Date2, AveragePrice, color = a_type))+
    labs(caption = gam6$call)

gam7 <- gam(AveragePrice ~ s(Date2, by = a_type), data = avocado_sacramento, method = "REML")
summary(gam7)



tibble(x = avocado_sacramento$Date2,
       a_type = avocado_sacramento$a_type,
       fit = gam7$fitted.values) %>% 
    ggplot(aes(x, fit, color = a_type))+
    geom_line()+
    geom_point(data = avocado_sacramento, aes(Date2, AveragePrice, color = a_type))+
    labs(caption = gam7$call)


gam8 <- gam(AveragePrice ~ s(Date2, by = a_type) + a_type, data = avocado_sacramento, method = "REML")
summary(gam8)

tibble(x = avocado_sacramento$Date2,
       a_type = avocado_sacramento$a_type,
       fit = gam8$fitted.values) %>% 
    ggplot(aes(x, fit, color = a_type))+
    geom_line()+
    geom_point(data = avocado_sacramento, aes(Date2, AveragePrice, color = a_type))+
    labs(caption = gam8$call)

gam.check(gam8)

concurvity(gam8)
           