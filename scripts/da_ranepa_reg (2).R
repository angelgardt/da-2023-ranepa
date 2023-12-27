library(tidyverse)

cofi <- read_csv("~/Downloads/Telegram Desktop/CalCOFI.csv")
str(cofi)
unique(cofi$quarter)
cofi |> mutate(quarter = as.character(quarter)) -> cofi
str(cofi)

nrow(cofi)


fit1 <- lm(temp ~ salinity, cofi)
summary(fit1)

par(mfrow = c(2,2))
plot(fit1)

cofi |> 
  ggplot(aes(salinity, temp)) +
  geom_point() +
  geom_smooth(method = "lm")





fit2 <- lm(temp ~ salinity + depth, cofi)
summary(fit2)
car::vif(fit2)
cor.test(cofi$salinity, cofi$depth)

anova(fit1, fit2, fit3)



fit3 <- lm(temp ~ salinity * depth, cofi)
summary(fit3)
car::vif(fit3, type = 'predictor')



fit4 <- lm(temp ~ salinity + depth + quarter, cofi)
summary(fit4)

cofi |> 
  ggplot(aes(salinity, temp, color = depth)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ quarter)



fit5 <- lm(temp ~ salinity * quarter, cofi)
summary(fit5)
car::vif(fit5)

# MSE

sum((cofi$temp - fit1$fitted.values)^2, na.rm = TRUE) / nrow(cofi)

# RMSE

sqrt(sum((cofi$temp - fit1$fitted.values)^2, na.rm = TRUE) / nrow(cofi))

