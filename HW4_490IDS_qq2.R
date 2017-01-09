# HW 4 Due Tuesday Sept 27, 2016. Upload R file to Moodle with name: HW2_490IDS_YOUR_CLASSID.R
# Notice we are using the new system with your unique class ID. You should have received an email with
# your unique class ID. Please make sure that ID is the only information on your hw that identifies you. 
# Do not remove any of the comments. These are marked by #

### Part 1: Linear Regression Concepts
## These questions do not require coding but will explore some important concepts
## from lecture 5.

## "Regression" refers to the simple linear regression equation:
##    y = B0 + B1*x
## This homework will not discuss any multivariate regression.

## 1. (1 pt)
## What is the interpretation of the coefficient B1? 
## (What meaning does it represent?)

## Your answer
#The coefficient B1 is the slope of this regression model. 
#The slope describes the estimated difference in the y variable if the explanatory variable x for a case happened to be one unit larger.

## 2. (1 pt)
## If the residual sum of squares (RSS) of my regression is exactly 0, what does
## that mean about my model?

## Your answer
#RSS is 0 means that this linear model perfectly fit the data without error.

## 3. (2 pt)
## Outliers are problems for many statistical methods, but are particularly problematic
## for linear regression. Why is that? It may help to define what outlier means in this case.
## (Hint: Think of how residuals are calculated)

## Your answer
#In statistics, an outlier is an observation point that is distant from other observations.
#Linear regression is particularly vulnerable to outliers because the distance to the best-fit 
#line(this distance is called residual) is so far that influence the whole trend of the linear model line.


### Part 2: Sampling and Point Estimation

## The following problems will use the ggplot2movies data set and explore
## the average movie length of films in the year 2000.

## Load the data by running the following code
install.packages("ggplot2movies")
library(ggplot2movies)
data(movies)

## 4. (2 pts)
## Subset the data frame to ONLY include movies released in 2000.
movies[movies$year==2000, ]
head(movies[movies$year==2000, ])

## Use the sample function to generate a vector of 1s and 2s that is the same
## length as the subsetted data frame. Use this vector to split
## the 'length' variable into two vectors, length1 and length2.
dim(movies[movies$year==2000, ])

## IMPORTANT: Make sure to run the following seed function before you run your sample
## function. Run them back to back each time you want to run the sample function.


## Check: If you did this properly, you will have 1035 elements in length1 and 1013 elements
## in length2.

set.seed(1848)
movie_sample=sample(x=1:2,2048,replace=T)
length12=split(movies$length[movies$year==2000],movie_sample)
length1=length12[[1]]
length2=length12[[2]]
length(length1)
length(length2)

## 5. (3 pts)
## Calculate the mean and the standard deviation for each of the two
## vectors, length1 and length2. Use this information to create a 95% 
## confidence interval for your sample means. Compare the confidence 
## intervals -- do they seem to agree or disagree?

## Your answer here
mean(length1)
mean(length2)
sd(length1)
sd(length2)
CI1min=mean(length1)-1.96*(sd(length1)/sqrt(length(length1)))
CI1min
CI1max=mean(length1)+1.96*(sd(length1)/sqrt(length(length1)))
CI1max

CI2max=mean(length2)+1.96*(sd(length2)/sqrt(length(length2)))
CI2max
CI2min=mean(length2)-1.96*(sd(length2)/sqrt(length(length2)))
CI2min
#The confidence interval of length1 is (75.89484,80.77762).
#The confidence interval of length2 is(77.59924,82.44222).
#They seem to agree.


## 6. (4 pts)
## Draw 100 observations from a standard normal distribution. Calculate the sample mean.
## Repeat this 100 times, storing each sample mean in a vector called mean_dist.
## Plot a histogram of mean_dist to display the sampling distribution.
## How closely does your histogram resemble the standard normal? Explain why it does or does not.

## Your answer here
set.seed(1848)
rnorm(100)
mean(rnorm(100))
mean_dist=replicate(100,mean(rnorm(100)))
hist(mean_dist)
#This histogram resembles the standard normal with the same mean but different sd. 
#Because the sample of 100 observations are from the standard normal distribution.
#So the occurrence probability of each observation accords with the standard normal distribution.
#The mean of each sample also accords with the standard normal distribution.
#Thus, the plot of 100 times of sample mean resembles the standard normal plot.

## 7. (3 pts)
## Write a function that implements Q6.

## Your answer here

HW.Bootstrap=function(distn,n,reps){
  set.seed(1848)
  if(distn == "rnorm"){
    mean_dist=replicate(reps,mean(rnorm(n)))
    hist(mean_dist)
         }
    }
HW.Bootstrap("norm",50,50)
  



### Part 3: Linear Regression
## This problem will use the Boston Housing data set.
## Before starting this problem, we will declare a null hypthosesis that the
## crime rate has no effect on the housing value for Boston suburbs.
## That is: H0: B1 = 0
##          HA: B1 =/= 0
## We will attempt to reject this hypothesis by using a linear regression


# Load the data
housing <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"),sep="")
names(housing) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")

## 7. (2 pt)
## Fit a linear regression using the housing data using CRIM (crime rate) to predict
## MEDV (median home value). Examine the model diagnostics using plot(). Would you consider this a good
## model or not? Explain.
lmfit=lm(housing$MEDV~housing$CRIM)
lmfit
plot(lmfit)
plot(housing$CRIM,housing$MEDV,main="scatterplot",xlab = "CRIM",ylab = 'MEDV')
abline(lmfit,col="red")
#I don't think it is a good model. Because this linear model can't fit the data well.
#According to the scatterplot, many observations are not around the linear line.
#According to the Residuals vs Fitted plot, residuals of many observations is so big.
#According to the Normal Q-Q plot, the points are not approximately lie on a straight line, which shows the distributions are not strictly linearly related.
#According to the Residuals vs Leverage plot, there are some influential cases.

## 8. (2 pts)
## Using the information from summary() on your model, create a 95% confidence interval 
## for the CRIM coefficient 
summary(lmfit)
CI_CRIM_MIN=summary(lmfit)$coefficients[2,1] - 2 * summary(lmfit)$coefficients[2,2]
CI_CRIM_MIN
CI_CRIM_MAX=summary(lmfit)$coefficients[2,1] + 2 * summary(lmfit)$coefficients[2,2]
CI_CRIM_MAX
#The confidence interval is point estimate¡À1.96¡ÁSE.
#So confidence interval for the CRIM coefficient is -0.41519¡À1.96¡Á0.04389, which is (-0.502971,-0.3274095).

## 9. (2 pts)
## Based on the result from question 8, would you reject the null hypothesis or not?
## (Assume a significance level of 0.05). Explain.

## Your answer
lmfit
#For the hypothesis, the null value for the slope is 0.
#So the estimate is 0, which not within the confidence interval from Q8.
#In addition, p-value: < 2.2e-16, which is far more less than 0.05.
#So we would reject the null hypothesis.

## 10. (1 pt)
## Pretend that the null hypothesis is true. Based on your decision in the previous
## question, would you be committing a decision error? If so, which one?

## Your answer
#I commit the decision about rejection of the null hypothesis is a error.
#We reject the null hypothesis in the previous question, because the likelihood of H0 happening  
#is so small. But we have to admit that small probability event can still have chance to occur.
#So this kind of error is type-1 error, which means we reject the null hypothesis which is true.

## 11. (1 pt)
## Use the variable definitions from this site:
## https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names
## Discuss what your regression results mean in the context of the data (using appropriate units)
## (Hint: Think back to Question 1)

## Your answer
#We can use linear aggression to reject the hypothesis about "crime rate has no effect on the housing value for Boston suburbs."
#The linear aggression result is: median home value=-0.4152*(crime rate)+24.0331.
#The coefficient B1(slope) is quite small, which means that crime rate has little impact on median home value.


## 12. (2 pt)
## Describe the LifeCycle of Data for Part 3 of this homework.
#Housing data is acquired from some source(measured,observed).
#These data can be used to make some analysis, to establish model or to be calculated.
#These analysis and models can lead to insight, decision or hypothesis. 
#Some hypotheses can be put forward based on the observation of data, and can be tested by data models.
#These typical data can be preserved so that others can revisit and reuse these data now or in the future.
