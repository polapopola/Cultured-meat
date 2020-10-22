##getting rid of the NAs
new<-na.omit(DatasetCM_Age_cleaned)
## Dividing the data into two groups, group1 is non-meat eaters
group1<- new[1:72, ]
group1[["cluster1"]] <- NA  # added by Christian Ritz, Oct. 22 2020
group1[["cluster2"]] <- NA
group1[["cluster3"]] <- NA
group1[["cluster4"]] <- 1

group2 <- new[-c(1:72), ]
group2[["cluster4"]] <- 0  # added by Christian Ritz, Oct. 22 2020 (see also line 54 below)

##clustering group2
library(cluster)
gapStatistic<-clusGap(group2[,c("feqsum","Processed")], 
                      kmeans,5)
plot(gapStatistic)
gapStatistic

set.seed(20190926)

clus3<-kmeans(group2[,c("feqsum","Processed")],
              centers = 3,
              nstart = 25)
clus3[["centers"]]
clus3[["size"]]
print(clus3)

plot(group2[,c("feqsum","Processed")],
     col=clus3[["cluster"]]+1)

## Cluster 1
## Cluster 2
## Cluster 3 

## Telling R that we have 3 clusters

group2[["cluster"]]<- clus3[["cluster"]]

## CLuster 1, low 

group2[["cluster1"]] <- with(group2, ifelse(cluster == 1, 1, 0))
## Cluster2, low
group2[["cluster2"]] <- with(group2, ifelse(cluster == 2, 1, 0))
## CLuster 3, high 
group2[["cluster3"]] <- with(group2, ifelse(cluster == 3, 1, 0))

head(group2[, c("cluster", "cluster1", "cluster2", 
              "cluster3")])

head(group2[,c("Sex","cluster1")],20)
#Female 3 #Male 2

JointGroup <- rbind(group1, group2)  # added by Christian Ritz, Oct. 22 2020


## We are loking the genders inside cluster1
group2[["Sex"]]<-as.factor(group2[["Sex"]])
logreg.1.gender<-glm(cluster1 ~ Sex, data=group2,
                     family=binomial)
summary(logreg.1.gender)
with(group2, table(Sex, cluster1))
##Gives the OR
exp(coef(summary(logreg.1.gender)))
## Looking for confidence interval 
exp(confint(logreg.1.gender))
##Pecentages of M and F
with(group2, prop.table(table(Sex, cluster1), margin=1)*100)
exp(coef(summary(logreg.1.gender))[2, 1]) 
exp(confint(logreg.1.gender)[2, ])

##Genders for cluster 2 - Medium
logreg.2.gender<-glm(cluster2 ~ Sex, data=group2,
                     family=binomial)
exp(coef(summary(logreg.2.gender)))
summary(logreg.2.gender)
exp(coef(summary(logreg.2.gender))[2, 1]) 
exp(confint(logreg.2.gender)[2, ])
with(group2, table(Sex, cluster2))
with(group2, (table(Sex, cluster2), margin=1)*100)

## Gender for cluster 3 - High 

logreg.3.gender<-glm(cluster3 ~ Sex, data=group2,
                     family=binomial)
exp(coef(summary(logreg.3.gender)))
summary(logreg.3.gender)
exp(coef(summary(logreg.3.gender))[2, 1]) 
exp(confint(logreg.3.gender)[2, ])
with(group2, table(Sex, cluster3))
with(group2(table(Sex, cluster3), margin=1)*100)

## Gender for cluster4
group1[["Sex"]]<-as.factor(group1[["Sex"]])
logreg.4.gender<-glm(cluster4 ~ Sex, data=group1,
                     family=binomial)
exp(coef(summary(logreg.4.gender)))
summary(logreg.4.gender)
exp(coef(summary(logreg.4.gender))[2, 1]) 
exp(confint(logreg.4.gender))
with(group1, table(Sex, cluster4))
with(group1(table(Sex, cluster4), margin=1)*100)

## Countries cluster 1
group2[["Country"]]<-as.factor(group2[["Country"]])

logreg.1.Country<-glm(cluster1 ~ Country, data=group2,
                     family=binomial)
exp(coef(summary(logreg.1.Country)))
summary(logreg.1.Country)
exp(coef(summary(logreg.1.Country))[2, 1]) 
exp(confint(logreg.1.Country))
with(group2, table(Country, cluster1))
with(group2(table(Country, cluster1), margin=1)*100)

## Counries cluster 2

logreg.2.Country<-glm(cluster2 ~ Country, data=group2,
                      family=binomial)
exp(coef(summary(logreg.2.Country)))
summary(logreg.2.Country)
exp(coef(summary(logreg.2.Country)))
exp(confint(logreg.2.Country))
with(group2, table(Country, cluster2))
with(group2(table(Country, cluster2), margin=1)*100)

## Countries cluster 3

logreg.3.Country<-glm(cluster3 ~ Country, data=group2,
                      family=binomial)
exp(coef(summary(logreg.3.Country)))
summary(logreg.3.Country)
exp(coef(summary(logreg.3.Country))[2, 1]) 
exp(confint(logreg.3.Country)
with(group2, table(Country, cluster3))
with(group2(table(Country, cluster3), margin=1)*100)

## Countries cluster 4

logreg.4.Country<-glm(cluster4 ~ Country, data=group1,
                      family=binomial)
exp(coef(summary(logreg.4.Country)))
summary(logreg.4.Country)
exp(coef(summary(logreg.4.Country))[2, 1]) 
exp(confint(logreg.4.Country)
with(group1, table(Country, cluster4))
    with(group1(table(Country, cluster4), margin=1)*100)

## Cluster 1 Localty
group2[["Residence"]]<-as.factor(group2[["Residence"]])
group1[["Residence"]]<-as.factor(group1[["Residence"]])

logreg.1.loc<-glm(cluster1 ~ Residence, data=group2,
                      family=binomial)
exp(coef(summary(logreg.1.loc)))
summary(logreg.1.loc)
exp(coef(summary(logreg.1.loc))[2, 1]) 
exp(confint(logreg.1.loc)[2, ])
with(group2, table(Residence, cluster1))
with(group2(table(Residence, cluster1), margin=1)*100)

## Cluster 2 Local

logreg.2.loc<-glm(cluster2 ~ Residence, data=group2,
                  family=binomial)
exp(coef(summary(logreg.2.loc)))
summary(logreg.2.loc)
exp(coef(summary(logreg.2.loc))[2, 1]) 
exp(confint(logreg.2.loc)[2, ])
with(group2, table(Residence, cluster2))
with(group2(table(Residence, cluster2), margin=1)*100)

## Cluster 3 Local

logreg.3.loc<-glm(cluster3 ~ Residence, data=group2,
                  family=binomial)
exp(coef(summary(logreg.3.loc)))
summary(logreg.3.loc)
exp(coef(summary(logreg.3.loc))[2, 1]) 
exp(confint(logreg.3.loc)[2, ])
with(group2, table(Residence, cluster3))
with(group2(table(Residence, cluster3), margin=1)*100)

## Cluster 4 Local

logreg.4.loc<-glm(cluster4 ~ Residence, data=group1,
                  family=binomial)
exp(coef(summary(logreg.4.loc)))
summary(logreg.4.loc)
exp(coef(summary(logreg.4.loc))[2, 1]) 
exp(confint(logreg.4.loc)[2, ])
with(group1, table(Residence, cluster4))
with(group1(table(Residence, cluster4), margin=1)*100)

## Cluster 1 education 
group2[["Education"]]<-as.factor(group2[["Education"]])
group1[["Education"]]<-as.factor(group1[["Education"]])
logreg.1.edu<-glm(cluster1 ~ Education, data=group2,
                  family=binomial)
exp(coef(summary(logreg.1.edu)))
summary(logreg.1.edu)
exp(coef(summary(logreg.1.edu))
exp(confint(logreg.1.edu))
with(group2, table(Education, cluster1))
with(group2(table(Education, cluster1), margin=1)*100)

## Cluster 2 education

logreg.2.edu<-glm(cluster2 ~ Education, data=group2,
                  family=binomial)
exp(coef(summary(logreg.2.edu)))
summary(logreg.2.edu)
exp(coef(summary(logreg.2.edu))
exp(confint(logreg.2.edu)
with(group2, table(Education, cluster2))
with(group2(table(Education, cluster2), margin=1)*100)

## Cluster 3 education 

logreg.3.edu<-glm(cluster3 ~ Education, data=group2,
                  family=binomial)
exp(coef(summary(logreg.3.edu)))
summary(logreg.3.edu)
exp(coef(summary(logreg.3.edu)))
exp(confint(logreg.3.edu))
with(group2, table(Education, cluster3))
with(group2(table(Education, cluster3), margin=1)*100)

## Cluster 1 Age

logreg.1.age<-glm(cluster1 ~ Age, data=group2,
                  family=binomial)
exp(coef(summary(logreg.1.age)))
summary(logreg.1.age)
exp(coef(summary(logreg.1.age))[2, 1]) 
exp(confint(logreg.1.age)[2, ])
with(group2, table(Age, cluster1))
with(group2(table(Age, cluster1), margin=1)*100)

## CLuster 2 AGe

logreg.2.age<-glm(cluster2 ~ Age, data=group2,
                  family=binomial)
exp(coef(summary(logreg.2.age)))
summary(logreg.2.age)
exp(coef(summary(logreg.2.age))[2, 1]) 
exp(confint(logreg.2.age)[2, ])
with(group2, table(Age, cluster2))
with(group2(table(Age, cluster2), margin=1)*100)

##CLuster 3 Age

logreg.3.age<-glm(cluster3 ~ Age, data=group2,
                  family=binomial)
exp(coef(summary(logreg.3.age)))
summary(logreg.3.age)
exp(coef(summary(logreg.3.age))[2, 1]) 
exp(confint(logreg.3.age)[2, ])
with(group2, table(Age, cluster3))
with(group2(table(Age, cluster3), margin=1)*100)

## MC cluster 1

group2[["MC1"]]<-as.factor(group2[["MC1"]])
group2[["MC2"]]<-as.factor(group2[["MC2"]])
group2[["MC3"]]<-as.factor(group2[["MC3"]])


logreg.1.MC<-glm(cluster1 ~ MC1, data=group2,
                  family=binomial)
exp(coef(summary(logreg.1.MC)))
summary(logreg.1.MC)
exp(coef(summary(logreg.1.MC))[2, 1])
exp(confint(logreg.1.MC)[2, ])
with(group2, table(MC1, cluster1))
with(group2(table(MC1, cluster1), margin=1)*100)

## Mc1 cluster 2

logreg.2.MC1<-glm(cluster2 ~ MC1, data=group2,
                 family=binomial)
exp(coef(summary(logreg.2.MC1)))
summary(logreg.2.MC1)
exp(coef(summary(logreg.2.MC1))[2, 1])
exp(confint(logreg.2.MC1)[2, ])
with(group2, table(MC1, cluster2))
with(group2(table(MC1, cluster2), margin=1)*100)

## MC1 cluster 3

logreg.3.MC1<-glm(cluster3 ~ MC1, data=group2,
                  family=binomial)
exp(coef(summary(logreg.3.MC1)))
summary(logreg.3.MC1)
exp(coef(summary(logreg.3.MC1))[2, 1])
exp(confint(logreg.3.MC1)[2, ])
with(group2, table(MC1, cluster3))
with(group2(table(MC1, cluster3), margin=1)*100)

## MC1 cluster 4

logreg.4.MC1<-glm(cluster4 ~ MC1, data=group1,
                  family=binomial)
exp(coef(summary(logreg.4.MC1)))
summary(logreg.4.MC1)
exp(coef(summary(logreg.4.MC1))[2, 1])
exp(confint(logreg.4.MC1)[2, ])
with(group1, table(MC1, cluster4))
with(group1(table(MC1, cluster4), margin=1)*100)

## MC2 cluster 1

logreg.1.MC2<-glm(cluster1 ~ MC2, data=group2,
                 family=binomial)
exp(coef(summary(logreg.1.MC2)))
summary(logreg.1.MC2)
exp(coef(summary(logreg.1.MC2))[2, 1])
exp(confint(logreg.1.MC2)[2, ])
with(group2, table(MC2, cluster1))
with(group2(table(MC2, cluster1), margin=1)*100)

## MC2 cluster 2

logreg.2.MC2<-glm(cluster2~ MC2, data=group2,
                  family=binomial)
exp(coef(summary(logreg.2.MC2)))
summary(logreg.2.MC2)
exp(coef(summary(logreg.2.MC2))[2, 1])
exp(confint(logreg.2.MC2)[2, ])
with(group2, table(MC2, cluster2))
with(group2(table(MC2, cluster2), margin=1)*100)

## MC2 cluster 3

logreg.3.MC2<-glm(cluster3~ MC2, data=group2,
                  family=binomial)
exp(coef(summary(logreg.3.MC2)))
summary(logreg.3.MC2)
exp(coef(summary(logreg.3.MC2))[2, 1])
exp(confint(logreg.3.MC2)[2, ])
with(group2, table(MC2, cluster3))
with(group2(table(MC2, cluster3), margin=1)*100)

## MC2 cluster 4

logreg.4.MC2<-glm(cluster4~ MC2, data=group1,
                  family=binomial)
exp(coef(summary(logreg.4.MC2)))
summary(logreg.4.MC2)
exp(coef(summary(logreg.4.MC2))[2, 1])
exp(confint(logreg.4.MC2)[2, ])
with(group1, table(MC2, cluster4))

## MC3 cluster 1

logreg.1.MC3<-glm(cluster1~ MC3, data=group2,
                  family=binomial)
exp(coef(summary(logreg.1.MC3)))
summary(logreg.1.MC3)
exp(coef(summary(logreg.1.MC3))[2, 1])
exp(confint(logreg.1.MC3)[2, ])
with(group2, table(MC3, cluster1))

## MC3 cluster2

logreg.2.MC3<-glm(cluster2~ MC3, data=group2,
                  family=binomial)
exp(coef(summary(logreg.2.MC3)))
summary(logreg.2.MC3)
exp(coef(summary(logreg.2.MC3))[2, 1])
exp(confint(logreg.2.MC3)[2, ])
with(group2, table(MC3, cluster2))

## MC3 cluster3

logreg.3.MC3<-glm(cluster3~ MC3, data=group2,
                  family=binomial)
exp(coef(summary(logreg.3.MC3)))
summary(logreg.3.MC3)
exp(coef(summary(logreg.3.MC3))[2, 1])
exp(confint(logreg.3.MC3)[2, ])
with(group2, table(MC3, cluster3))

## MC3 cluster 4

logreg.4.MC3<-glm(cluster4~ MC3, data=group1,
                  family=binomial)
exp(coef(summary(logreg.4.MC3)))
summary(logreg.4.MC3)
exp(coef(summary(logreg.4.MC3))[2, 1])
exp(confint(logreg.4.MC3)[2, ])
with(group1, table(MC3, cluster4))

## Familiartiy cluster1
group2[["Familiarity"]]<-as.factor(group2[["Familiarity"]])
group1[["Familiarity"]]<-as.factor(group1[["Familiarity"]])

logreg.1.Fami<-glm(cluster1~ Familiarity, data=group2,
                  family=binomial)
exp(coef(summary(logreg.1.Fami)))
summary(logreg.1.Fami)
exp(coef(summary(logreg.1.Fami)))
exp(confint(logreg.1.Fami))
with(group2, table(Familiarity, cluster1))

## Familiarity cluster 2

logreg.2.Fami<-glm(cluster2~ Familiarity, data=group2,
                   family=binomial)
exp(coef(summary(logreg.2.Fami)))
summary(logreg.2.Fami)
exp(confint(logreg.2.Fami))
with(group2, table(Familiarity, cluster2))

## Familiartiy cluster 3

logreg.3.Fami<-glm(cluster3~ Familiarity, data=group2,
                   family=binomial)
exp(coef(summary(logreg.3.Fami)))
summary(logreg.3.Fami)
exp(confint(logreg.3.Fami))
with(group2, table(Familiarity, cluster3))

## Familiarity cluster 4

logreg.4.Fami<-glm(cluster4~ Familiarity, data=group1,
                   family=binomial)
exp(coef(summary(logreg.4.Fami)))
summary(logreg.4.Fami)
exp(confint(logreg.4.Fami))
with(group1, table(Familiarity, cluster4))

## Adjusted (univariate) logistic regression
cluster1.adj <- glm(cluster1 ~ Age+Education+Sex, data = group2,
                         family = binomial)
coef(summary(cluster1.adj)) 
exp(coef(summary(cluster1.adj))[2, 1])                           
exp(confint(cluster1.adj)[2, ])
summary(cluster1.adj)

## Agre dis cluster 1

group2[["Healhty"]]<-as.factor(group2[["Healthy"]])
group1[["Healthy"]]<-as.factor(group1[["Healthy"]])
group2[["EF"]]<-as.factor(group2[["EF"]])
group1[["EF"]]<-as.factor(group1[["EF"]])
group2[["Tasty"]]<-as.factor(group2[["Tasty"]])
group1[["Tasty"]]<-as.factor(group1[["Tasty"]])
group2[["Kind"]]<-as.factor(group2[["Kind"]])
group1[["Kind"]]<-as.factor(group1[["Kind"]])
group2[["Unnatural"]]<-as.factor(group2[["Unnatural"]])
group1[["Unnatural"]]<-as.factor(group1[["Unnatural"]])
group2[["Disgusting"]]<-as.factor(group2[["Disgusting"]])
group1[["Disgusting"]]<-as.factor(group1[["Disgusting"]])

cluster1.adj <- glm(cluster1 ~ Healthy+EF+Tasty+Kind+Unnatural+Disgusting, data = group2,
                    family = binomial)
coef(summary(cluster1.adj)) 
exp(coef(summary(cluster1.adj)))                          
exp(confint(cluster1.adj))
summary(cluster1.adj)

with(group2, table(Healthy, cluster1))
with(group2, table(Healthy, cluster2))
with(group2, table(Healthy, cluster3))
with(group1, table(Healthy, cluster4))

with(group2, table(EF, cluster1))
with(group2, table(EF, cluster2))
with(group2, table(EF, cluster3))
with(group1, table(EF, cluster4))

with(group2, table(Tasty, cluster1))
with(group2, table(Tasty, cluster2))
with(group2, table(Tasty, cluster3))
with(group1, table(Tasty, cluster4))

with(group2, table(Kind, cluster1))
with(group2, table(Kind, cluster2))
with(group2, table(Kind, cluster3))
with(group1, table(Kind, cluster4))

with(group2, table(Unnatural, cluster1))
with(group2, table(Unnatural, cluster2))
with(group2, table(Unnatural, cluster3))
with(group1, table(Unnatural, cluster4))

with(group2, table(Disgusting, cluster1))
with(group2, table(Disgusting, cluster2))
with(group2, table(Disgusting, cluster3))
with(group1, table(Disgusting, cluster4))
