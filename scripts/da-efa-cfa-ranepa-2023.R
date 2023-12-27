library(tidyverse)

pizza <- read_csv("https://raw.githubusercontent.com/angelgardt/hseuxlab-andan/master/Pizza.csv")
str(pizza)

pizza2 <- pizza %>% select(-brand, -id)

factanal(pizza2, factors = 3)
factanal(pizza2, factors = 2)

factanal(pizza2, factors = 2, rotation = "promax")

fa <- factanal(pizza2, factors = 2, scores = "regression")

fa$converged
fa$loadings
fa$uniquenesses["prot"]

fa$correlation
fa$scores
fa$call

fa$scores %>% 
  as_tibble() %>% 
  mutate(brand = pizza$brand,
         id = pizza$id) %>% 
ggplot(aes(Factor1, Factor2, color = brand)) +
  geom_point() +
  theme_bw()

library(lavaan)

tolunc <- read_csv2("https://raw.githubusercontent.com/angelgardt/hseuxlab-wlm2021/master/data/tolerance_uncertainty.csv")
str(tolunc)
tolunc

model1 <- "TU =~ tu1 + tu2 + tu3 + tu4 + tu5 + tu6 + tu7 + tu8 + tu9 + tu10 + tu11 + tu12 + tu13 + tu14 + tu15 + tu16 + tu17 + tu18 + tu19 + tu20 + tu21 + tu22"

cfa1 <- cfa(model1, data = tolunc)
summary(cfa1)

scfa1 <- standardizedsolution(cfa1)
scfa1

scfa1[scfa1$op == "=~", ]
scfa1[scfa1$op == "~~", ]

fitmeasures(cfa1, c("cfi", "tli", "srmr", "rmsea"))


model2 <- "TU =~ tu1 + tu2 + tu3 + tu4 + tu5 + tu6 + tu7 + tu8 + tu9 + tu10 + tu11 + tu12 + tu13 + tu14 + tu15 + tu16 + tu17 + tu18 + tu19 + tu20 + tu22"

cfa2 <- cfa(model2, data = tolunc)
fitmeasures(cfa2, c("cfi", "tli", "srmr", "rmsea"))

modificationindices(cfa2)


model3 <- "
TU =~ tu1 + tu2 + tu3 + tu4 + tu5 + tu6 + tu7 + tu8 + tu9 + tu10 + tu11 + tu12 + tu13 + tu14 + tu15 + tu16 + tu17 + tu18 + tu19 + tu20 + tu22
tu1 ~~ tu2
"
cfa3 <- cfa(model3, tolunc)
fitmeasures(cfa3, c("cfi", "tli", "srmr", "rmsea"))


anova(cfa2, cfa3)
