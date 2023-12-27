library(MASS)
library(klaR)
library(tidyverse)
theme_set(theme_bw())
# install.packages("MASS")
# install.packages("devtools")
# devtools::install_github("fawda123/ggord")
# install.packages("klaR")

## LDA

cpi <- readxl::read_xlsx("data/cpi.xlsx")
str(cpi)
summary(cpi)

cpi %>% 
    summarise(mean_prod = mean(prodazhi),
              sd_prod = sd(prodazhi),
              .by = group) %>% 
    arrange(desc(mean_prod))

cpi %>% 
    mutate(group = recode(group,
                          "0" = "low",
                          "1" = "medium",
                          "2" = "high") %>% as_factor()) -> cpi

GGally::ggpairs(cpi, columns = 1:10, ggplot2::aes(color = group))

model_lda <- lda(group ~ Cs + Sp + Do + Sy, cpi)
model_lda

lda_pred <- predict(model_lda, cpi)
lda_pred
ldahist(lda_pred$x[, 1], g = cpi$group)
ldahist(lda_pred$x[, 2], g = cpi$group)

# ggord::ggord(model_lda, cpi$group)

partimat(group ~ Cs + Sp + Do + Sy, data = cpi, method = "lda")

confmat <- table(pred = lda_pred$class, real = cpi$group)
confmat
sum(diag(confmat)) / sum(confmat)


## CLUSTER



