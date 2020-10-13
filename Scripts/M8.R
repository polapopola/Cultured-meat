## M8

## Data import
file.choose()

eu.qpc.not0 <- 
  read.csv2("/home/christian/stat/teaching/FOOD/2020-FCR/eu.qpc.not0.csv")
# use read.csv2() for CSV files saved from a continental European or 
#  South American Excel

## Looking at the relevant data: 1 independent variable, 1 dependent variable
head(eu.qpc.not0[, c("Gender", "cluster1")], 10)
# respondents 1,2,3,6 don't belong to segment 1
# respondents 4,5 do belon to segment 1

## Formatting variables 
##  (converting numeric encoding to categorical encoding)
eu.qpc.not0[["Gender"]] <- as.factor(eu.qpc.not0[["Gender"]])
# 1: male, 2: female

## Fitting a logistic regression
logreg.1.gender <- glm(cluster1 ~ Gender, data = eu.qpc.not0,
                       family = binomial)
# family = binomial ensures fitting a logistic regression

## Looking at the estimates
summary(logreg.1.gender)
# log OR for Gender the change in likelihood when
#  comparing females and males

# log OR for the change from males to female = -0.5

## Obtaining the OR:
exp(-0.50422)
# OR = 0.60
# females 40% less likely to be in segment 1 compared to males
# so there are less female respondents than male respondents in segment 1

with(eu.qpc.not0, table (Gender, cluster1))  # counts
with(eu.qpc.not0, prop.table(table (Gender, cluster1), margin = 1)*100)
# 38% males, 27% females

## Looking at the summary output
summary(logreg.1.gender)
# p-value for testing no difference between male and female = 0.000003
# (<0.05)

## Finding the confidence interval
confint(logreg.1.gender)
# providing confidence intervals on the log odds scale
# need to back transform

exp(confint(logreg.1.gender))
# 95% CI: [0.49, 0.74]

## Changing the reference group (need to fit the model again)
logreg.1.gender.new <- glm(cluster1 ~ relevel(Gender, "2"),
                           data = eu.qpc.not0, family = binomial)
coef(summary(logreg.1.gender.new))  # condensed version of a summary

## Resulting OR (comparing now males to the females (reference))
exp(0.5042177)
# OR = 1.66
# Males are 66% more likely to be in segment 1 compared to females

## Logistic regression with a quantitative independent variable (=age)

## Visualization 
plot(cluster1 ~ Age, data = eu.qpc.not0)
# not informative perhaps: membership for all ages, no membership for all ages
# seemingly no effect of age

## Fitting the logistic regression model
logreg.1.age <- glm(cluster1 ~ Age, data = eu.qpc.not0,
                    family = binomial)
coef(summary(logreg.1.age))
# Any effect of age? No significant effect: p=0.49

## Finding the OR
exp(0.002828027)
## OR = 1.002832
## Each an increase in age by 1 year leads to an increase in likelihood
##  of segment membership by 0.3%

## Getting th confidence interval
exp(confint(logreg.1.age)[2, ])

100 * (exp(confint(logreg.1.age)[2, ]) - 1)
## Each an increase in age by 1 year leads to an increase in likelihood
##  of segment membership by 0.3% (95% CI: [-0.5, 1.1]) %

## Automated back transformation of the log odds estimate into a percentage:
100 * (exp(coef(summary(logreg.1.age))[2, 1]) - 1)


## Multivariate logistic regression analysis

## We want to adjust for confounding by including Age and Gender
eu.qpc.not0[["education"]] <- as.factor(eu.qpc.not0[["education"]])
logreg.1.educ <- glm(cluster1 ~ education + Age + Gender,
                     data = eu.qpc.not0, family = binomial)
coef(summary(logreg.1.educ))

## Getting OR's
exp(coef(summary(logreg.1.educ))[,1])

## Getting percentages
100 * (exp(coef(summary(logreg.1.educ))[,1]) - 1)

## Getting confidence intervals
exp(confint(logreg.1.educ))
# education is not a determinant of segment membership

## Getting confidence intervals in terms of percentages
(exp(confint(logreg.1.educ)) - 1) * 100
