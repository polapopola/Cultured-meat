## M7 - consumer segmentation

## Data import
file.choose()

eu.qpc.not0 <- 
  read.csv2("/home/christian/stat/teaching/FOOD/2020-FCR/eu.qpc.not0.csv")

names(eu.qpc.not0)

## Looking at the data
with(eu.qpc.not0, hist(VarietyTotal))
# a left-skewed distribution
with(eu.qpc.not0, hist(TotalPorkWeek))
# a right-skewed distribution, as expected


## Running a first K-means approach
sessionInfo()  # look for R version

set.seed(20200922)
kmeans.3 <- kmeans(eu.qpc.not0[, c("VarietyTotal", "TotalPorkWeek")],
                   centers = 3,
                   nstart = 25)

## Asking for specific results

## Segment membership
kmeans.3[["cluster"]]
# a column with numbers 1,2,3 indicating membership of segment 1,2,3, 
#  respectively

## How does the segments look?
## Looking at the segments means
kmeans.3[["centers"]]

## Segment sizes
kmeans.3[["size"]]
# large and useful segments

## Looking at within-segment similarity
kmeans.3[["tot.withinss"]]

## Looking at the betweem-segment proximity
kmeans.3[["betweenss"]]
# differences between segments are larger than differences within
#  segments

## Plotting the behavioural data
plot(eu.qpc.not0[, c("VarietyTotal", "TotalPorkWeek")])

## Plotting the Voronoi tesselation, visualization of the segments
plot(eu.qpc.not0[, c("VarietyTotal", "TotalPorkWeek")],
     col = kmeans.3[["cluster"]] + 1)
# col encoding: 1 = black, 2 = red, 3 = green, 4 = blue

## Pedestrian way

## K-means assuming 2 segments

# K = 2
kmeans.2 <- kmeans(eu.qpc.not0[, c("VarietyTotal", "TotalPorkWeek")],
                   centers = 2,
                   nstart = 25)
kmeans.2[["tot.withinss"]]

# K = 4
kmeans.4 <- kmeans(eu.qpc.not0[, c("VarietyTotal", "TotalPorkWeek")],
                   centers = 4,
                   nstart = 25)
kmeans.4[["tot.withinss"]]

# K = 5
kmeans.5 <- kmeans(eu.qpc.not0[, c("VarietyTotal", "TotalPorkWeek")],
                   centers = 5,
                   nstart = 25)
kmeans.5[["tot.withinss"]]
