# install.packages("MASS")
library(MASS)
# install.packages("klaR")
library(klaR)
library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = 'bottom')


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


partimat(group ~ Cs + Sp + Do + Sy, data = cpi, method = "lda")

confmat <- table(pred = lda_pred$class, real = cpi$group)
confmat
sum(diag(confmat)) / sum(confmat)


## CLUSTER

sapply(cpi %>% select(-group), range)

d <- dist(cpi %>% select(-group, -prodazhi), method = 'euclid')
hc_complete <- hclust(d, method = 'complete')
hc_average <- hclust(d, method = 'average')
hc_single <- hclust(d, method = 'single')

plot(hc_complete)
plot(hc_average)
plot(hc_single)

cutree(hc_complete, 6)

cstats.table <- function(dist, tree, k) {
    library(fpc)
    clust.assess <-
        c(
            "cluster.number",
            "n",
            "within.cluster.ss",
            "average.within",
            "average.between",
            "average.distance",
            "wb.ratio",
            "dunn2",
            "avg.silwidth"
        )
    clust.size <- c("cluster.size")
    stats.names <- c()
    row.clust <- c()
    output.stats <- matrix(ncol = k, nrow = length(clust.assess))
    cluster.sizes <- matrix(ncol = k, nrow = k)
    for (i in c(1:k)) {
        row.clust[i] <- paste("Cluster-", i, " size")
    }
    for (i in c(2:k)) {
        stats.names[i] <- paste("Test", i - 1)
        
        for (j in seq_along(clust.assess)) {
            output.stats[j, i] <-
                unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
            
        }
        
        for (d in 1:k) {
            cluster.sizes[d, i] <-
                unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
            dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
            cluster.sizes[d, i]
            
        }
    }
    output.stats.df <- data.frame(output.stats)
    cluster.sizes <- data.frame(cluster.sizes)
    cluster.sizes[is.na(cluster.sizes)] <- 0
    rows.all <- c(clust.assess, row.clust)
    # rownames(output.stats.df) <- clust.assess
    output <- rbind(output.stats.df, cluster.sizes)[, -1]
    colnames(output) <- stats.names[2:k]
    rownames(output) <- rows.all
    is.num <- sapply(output, is.numeric)
    output[is.num] <- lapply(output[is.num], round, 2)
    output
}


clust.stats <- cstats.table(d, hc_complete, 15)


ggplot(data = data.frame(t(clust.stats)),
       aes(x = cluster.number, y = within.cluster.ss)) +
    geom_point() +
    geom_line() +
    labs(x = "Num. of clusters", y = "Within clusters sum of squares (WSS)") +
    theme(plot.title = element_text(hjust = 0.5))


cpi %>% 
    ggplot(aes(Sp, Em, color = as_factor(cutree(hc_complete, 6)))) +
    geom_point()

cpi %>% 
    ggplot(aes(Fx, Re, color = as_factor(cutree(hc_complete, 6)))) +
    geom_point()

cpi %>% 
    ggplot(aes(Sy, Cs, color = as_factor(cutree(hc_complete, 6)))) +
    geom_point()

cpi %>% 
    ggplot(aes(prodazhi, as_factor(cutree(hc.complete, 6)),
               color = as_factor(group))) +
    geom_point()



km2 <- kmeans(cpi %>% select(-group, -prodazhi), centers = 2)
str(km2)

wss <- function(x, k) {
    wss <- numeric(k)
    names(wss) <- 1:k
    for (i in 2:k) {
        wss[i] <- kmeans(x, i)$tot.withinss
    }
    return(wss[-1])
}

km_wss <- wss(cpi %>% select(-group, -prodazhi), 15)

ggplot(NULL, aes(2:15, km_wss)) +
    geom_line() +
    geom_point() +
    labs(x = 'Number of Clusters',
         y = 'Within group sum of squares')





pizza <- read_csv("https://raw.githubusercontent.com/angelgardt/da-2023-ranepa/master/data/pizza_factor_scores.csv")


pizza %>% 
    ggplot(aes(Factor1, Factor2, color = brand)) +
    geom_point()

d_pizza <- dist(pizza[c("Factor1", "Factor2")], method = "euclid")

hc_ave_pizza <- hclust(d_pizza, method = 'average')
plot(hc_ave_pizza)

clust.stats_pizza <- cstats.table(d_pizza, hc_ave_pizza, 10)

ggplot(data = data.frame(t(clust.stats_pizza)),
       aes(x = cluster.number, y = within.cluster.ss)) +
    geom_point() +
    geom_line() +
    labs(x = "Num. of clusters", y = "Within clusters sum of squares (WSS)") +
    theme(plot.title = element_text(hjust = 0.5))


km_wss_pizza <- wss(pizza[c("Factor1", "Factor2")], 10)

ggplot(NULL, aes(2:10, km_wss_pizza)) +
    geom_line() +
    geom_point() +
    labs(x = 'Number of Clusters',
         y = 'Within group sum of squares')

km3_pizza <- kmeans(pizza[c("Factor1", "Factor2")], centers = 3)

pizza %>% 
    mutate(cluster_hc = cutree(hc_ave_pizza, 3),
           cluster_km = km3_pizza$cluster) -> pizza

pizza %>% 
    ggplot(aes(Factor1, Factor2, 
               color = brand, 
               shape = as_factor(cluster_hc))) +
    geom_point(size = 2)

pizza %>% 
    ggplot(aes(Factor1, Factor2, 
               color = brand, 
               shape = as_factor(cluster_km))) +
    geom_point(size = 2)

