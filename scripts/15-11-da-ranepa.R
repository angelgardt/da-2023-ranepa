library(tidyverse)
manag <- read_csv("https://raw.githubusercontent.com/angelgardt/hseuxlab-wlm2021/master/book/wlm2021-book/data/managers.csv")
str(manag)

unique(manag$lvl)

manag |> 
  select(-...1, - id) |> 
  # mutate(level = recode(lvl, "Менеджер" = 1, "Сотрудник" = 2))
  mutate(lvl = ifelse(lvl == "Менеджер", 1, 0)) -> manag


model <- glm(lvl ~ ., manag, family = binomial)
summary(model)

model0 <- glm(lvl ~ 1, manag, family = binomial)

anova(model0, model, test = 'Chi')

drop1(model, test = "Chi")

model1 <- update(model, .~. -error_cost)
drop1(model1, test = "Chi")

AIC(model, model1)


satis <- read_csv('https://raw.githubusercontent.com/angelgardt/hseuxlab-wlm2021/master/book/wlm2021-book/data/trust_data.csv')
str(satis)

unique(satis$feedback)
unique(satis$explanation)

unique(satis$sus_mark)

satis |> 
  mutate(sus_grade = recode(sus_mark,
                            "A" = 5,
                            "B" = 4, 
                            "C" = 3,
                            "D" = 2,
                            "E" = 1,
                            "F" = 0)) -> satis

model3 <- glm(sus_grade ~ feedback * explanation, satis, family = poisson)
summary(model3)


model_0 <- glm(sus_grade ~ 1, satis, family = poisson)

anova(model3, model_0, test = "Chi")

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  if (inherits(model, 'negbin'))
    rdf <- rdf - 1
  rp <- residuals(model, type = 'pearson')
  Pearson.chisq <- sum(rp ^ 2)
  prat <- Pearson.chisq / rdf
  pval <-
    pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(
    chisq = Pearson.chisq,
    ratio = prat,
    rdf = rdf,
    p = pval
  )
}

overdisp_fun(model3)



model4 <- glm(sus_grade ~ feedback * explanation, satis, family = quasipoisson)
summary(model4)
model00 <- glm(sus_grade ~ 1, satis, family = quasipoisson)

anova(model00, model4, test = "F")


