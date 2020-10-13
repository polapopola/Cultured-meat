### Importing data (from a CSV file)
FCR.2020 <- read.csv2(file.choose())

### Looking at the data
FCR.2020
head(FCR.2020)  # first 6 lines
str(FCR.2020)
summary(FCR.2020)

### Descriptive statistics

### Gender - a categorical/qualitative variable
with(FCR.2020, table(Gender))  # giving the counts

with(FCR.2020, prop.table(table(Gender)) * 100)

# 29 (85%) females


### Age - a quantitative variabe

### Looking at the histogram first
with(FCR.2020, hist(Age))
# A right-skewed distribution
# Summary by means of the median and IQR

with(FCR.2020, summary(Age))
# Median: 25, IQR: [23, 27]

### Height - a qunatitative

### Looking at the histogram first
with(FCR.2020, hist(Hieght))
# Another right-skewed distribution

with(FCR.2020, summary(Hieght))
## Median: 167, IQR: [162, 174]

## Calculating mean and SD in case the distribution was treated
##  as a symmetric one
with(FCR.2020, mean(Hieght, na.rm = TRUE))  # removing missing values
# Mean: 170

with(FCR.2020, sd(Hieght, na.rm = TRUE))
# SD = 10

## In summary: 170 +/- 10


### Weight- a quantitative variable

### Looking at the histogram first
with(FCR.2020, hist(Weight))
# A right-skewed distribution

with(FCR.2020, summary(Weight))
# Median: 62, IQR: [56, 70] (note the rounding to integers)

### Looking at a Likert score

with(FCR.2020, table(I.will.only.buy.products.at.a.reduced.price))

### Conversion to a quantitative variable
FCR.2020[["I.will.only.buy.products.at.a.reduced.price"]]  
# accessing the column

## Coverting a string variable to a categorical variable (factor in R terminology)
FCR.2020[["I.will.only.buy.products.at.a.reduced.price"]] <- 
  as.factor(FCR.2020[["I.will.only.buy.products.at.a.reduced.price"]]) 
 
with(FCR.2020, levels(I.will.only.buy.products.at.a.reduced.price))
# Note the order is alphabetic

### Changing the order
levels(FCR.2020[["I.will.only.buy.products.at.a.reduced.price"]]) <-
  c(6, 5, 3, 2, 1, 4)

with(FCR.2020, table(I.will.only.buy.products.at.a.reduced.price))

### Summary as a quantitative variable
with(FCR.2020, hist(as.numeric(I.will.only.buy.products.at.a.reduced.price)))
# An asymmetric distribution, but not right- or left-skewed

with(FCR.2020, 
     summary(as.numeric(I.will.only.buy.products.at.a.reduced.price)))
# Median: 4, IQR: [2.3, 5.8]