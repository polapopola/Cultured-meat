##getting rid of the NAs
new<-na.omit(DatasetCM_Copy)
## Dividing the data into two groups, group1 is non-meat eaters
group1<- new[1:72, ]
group2<- new[-c(1:72), ]

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

