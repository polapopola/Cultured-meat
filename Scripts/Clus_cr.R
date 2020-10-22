##getting rid of the NAs
new<-na.omit(DatasetCM_Copy)
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
with(group2(table(Sex, cluster2), margin=1)*100)

## Gender for cluster 3 - High 

logreg.3.gender<-glm(cluster3 ~ Sex, data=group2,
                     family=binomial)
exp(coef(summary(logreg.3.gender)))
summary(logreg.3.gender)
exp(coef(summary(logreg.3.gender))[2, 1]) 
exp(confint(logreg.3.gender)[2, ])
with(group2, table(Sex, cluster3))
with(group2(table(Sex, cluster3), margin=1)*100)

## Countries cluster 1

logreg.1.Country<-glm(cluster1 ~ Country, data=group2,
                     family=binomial)
exp(coef(summary(logreg.1.Country)))
summary(logreg.1.Country)
exp(coef(summary(logreg.1.Country))[2, 1]) 
exp(confint(logreg.1.Country)[2, ])
with(group2, table(Country, cluster1))
with(group2(table(Country, cluster1), margin=1)*100)

## Counries cluster 2

logreg.2.Country<-glm(cluster2 ~ Country, data=group2,
                      family=binomial)
exp(coef(summary(logreg.2.Country)))
summary(logreg.2.Country)
exp(coef(summary(logreg.2.Country))[2, 1]) 
exp(confint(logreg.2.Country)[2, ])
with(group2, table(Country, cluster2))
with(group2(table(Country, cluster2), margin=1)*100)

## Countries cluster 3

logreg.3.Country<-glm(cluster3 ~ Country, data=group2,
                      family=binomial)
exp(coef(summary(logreg.3.Country)))
summary(logreg.3.Country)
exp(coef(summary(logreg.3.Country))[2, 1]) 
exp(confint(logreg.3.Country)[2, ])
with(group2, table(Country, cluster3))
with(group2(table(Country, cluster3), margin=1)*100)

## Cluster 1 Localty

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

## Cluster 1 education 

logreg.1.edu<-glm(cluster1 ~ Education, data=group2,
                  family=binomial)
exp(coef(summary(logreg.1.edu)))
summary(logreg.1.edu)
exp(coef(summary(logreg.1.edu))[2, 1]) 
exp(confint(logreg.1.edu)[2, ])
with(group2, table(Education, cluster1))
with(group2(table(Education, cluster1), margin=1)*100)

## Cluster 2 education

logreg.2.edu<-glm(cluster2 ~ Education, data=group2,
                  family=binomial)
exp(coef(summary(logreg.2.edu)))
summary(logreg.2.edu)
exp(coef(summary(logreg.2.edu))[2, 1]) 
exp(confint(logreg.2.edu)[2, ])
with(group2, table(Education, cluster2))
with(group2(table(Education, cluster2), margin=1)*100)

## Cluster 3 education 

logreg.3.edu<-glm(cluster3 ~ Education, data=group2,
                  family=binomial)
exp(coef(summary(logreg.3.edu)))
summary(logreg.3.edu)
exp(coef(summary(logreg.3.edu))[2, 1]) 
exp(confint(logreg.3.edu)[2, ])
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


## MC cluster 1

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

## M2 cluster 1

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


