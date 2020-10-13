
## With SAV file
library(foreign)
grp4 <- as.data.frame(read.spss(file.choose()))


## Looking at the data
with(grp4, hist(a_meat))
with(grp4, hist(a_fish))
with(grp4, hist(a_dairy))
with(grp4, hist(a_eggs))

summary(grp4[, c("a_meat", "a_fish", 
                 "a_eggs", "a_dairy")])


## Cluster analysis on 4 variables
set.seed(20190926)
grp4.cluster4 <- kmeans(grp4[, c("a_meat", "a_fish", 
                                 "a_eggs", "a_dairy")],
                        4,
                        nstart = 100)

grp4.cluster4[["size"]]

grp4.cluster4[["centers"]]
# segment 1: high all
# segment 2: high meat
# segment 3: high dairy
# segment 4: low all


grp4.cluster4[["cluster"]]

## Adding a column with segment membership to my da(taset
grp4[["cluster"]] <- grp4.cluster4[["cluster"]]

grp4[["cluster1"]] <- with(grp4, ifelse(cluster == 1, 1, 0))
grp4[["cluster2"]] <- with(grp4, ifelse(cluster == 2, 1, 0))
grp4[["cluster3"]] <- with(grp4, ifelse(cluster == 3, 1, 0))
grp4[["cluster4"]] <- with(grp4, ifelse(cluster == 4, 1, 0))

head(grp4[, c("cluster", "cluster1", "cluster2", 
              "cluster3", "cluster4")])

## Example of profiling

## Unadjusted (univariate) logistic regression
cluster1.cook.unadj <- glm(cluster1 ~ o_prepar, data = grp4,
                           family = binomial)
coef(summary(cluster1.cook.unadj))               

exp(coef(summary(cluster1.cook.unadj))[2, 1])                           
# OR = 1.33

exp(confint(cluster1.cook.unadj)[2, ])
# 95% CI: [1.03, 1.70]

## Adjusted (univariate) logistic regression
cluster1.cook.adj <- glm(cluster1 ~ o_prepar+age+education+gender, data = grp4,
                         family = binomial)
coef(summary(cluster1.cook.adj))               

exp(coef(summary(cluster1.cook.adj))[2, 1])                           
# OR = 1.27

exp(confint(cluster1.cook.adj)[2, ])
# 95% CI: [0.98, 1.64]
# exactly the same result as in the article
