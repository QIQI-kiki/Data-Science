# HW 6 - Due Tuesday October 18, 2016 in moodle and hardcopy in class. 
# Upload R file to Moodle with name: HW5_490IDS_YourClassID.R
# Do not remove any of the comments. These are marked by #

# Please ensure that no identifying information (other than your class ID) 
# is on your paper copy, including your name


# We will use the bootstrap technique to generate confidence intervals

# 1. Suppose we have a sample of data from an exponential distribution 
# with parameter lambda. In this case use lambda.hat = 1/mean(X). 

# As the number of observations increases, does the estimate for lambda 
# become roughly normally distributed? We will answer this question in
# the following parts.

# 1a. (1) Generate 100 observations of test data, with lambda=3. Remember
# to set your seed before carrying out any computations.
set.seed(0)
lambda=3
n=100
exponentialsample=rexp(n=n,rate=lambda)

# 1b. (1) What is the mean of your test data? (give the code and the value)
mean(exponentialsample)

# 1c. (1) What is your estimate lambda.hat? (give the code and the value)
lambda.hat=1/mean(exponentialsample)
lambda.hat

# 2. Now use the bootstrap to estimate the distribution of 
# lambda.hat and create bootstrap confidence intervals for lambda, 
# rather than the approach in 1).

# 2a. (1) Form a set of bootstrap estimates of our parameter by generating B
# random samples as you did once in 1a but use lambda.hat since we do not
# know the true lambda in this case (keep n=100). Set B=1000, and again set
# your seed.
set.seed(0)
B=1000
bootstrap=replicate(B,rexp(100,rate=lambda.hat))

# 2b. (1) Get a new estimate for lambda.hat from each of the bootstrap samples
# in 2a. You'll want to create a matrix to receive each value. You should 
# have 1000 estimates for lambda.hat now.
bs.matrix=matrix(1/(apply(bootstrap,2,mean)))


# 2c. (2) Now look at the sampling distribution for lambda.hat, using the hist
# function. Remember the graphing techniques discussed in class and use them 
# to make the plot look professional. Does the distribution look normal?
hist(bs.matrix,breaks = 20,xlim=c(2,4),xlab = 'Bootstrap Samples',ylab = 'Count',main = 'Sampling distribution for Lambda.hat')
#The sampling distribution of Lambda.hat looks normal.

# 2d. (1) Calculate an estimate of the standard error of lambda.hat using your
# collection of bootstrap estimated parameters. What is your 95% confidence interval?
bs.se=sd(bs.matrix)
bs.se
bsCI.high=lambda.hat+1.96*bs.se
bsCI.high
bsCI.low=lambda.hat-1.96*bs.se
bsCI.low

# 3a. (5) We made some decisions when we used the bootstrap above that we can now question. 
# Repeat the above creation of a confidence interval for a range of values of data
# (we had our sample size fixed at 100) and a range of bootstrap values (we had B 
# fixed at 1000). Suppose the sample size varies (100, 200, 300, .... , 1000) and 
# B varies (1000, 2000, ... , 10000). You will likely find it useful to write
# functions to carry out these calculations. Your final output should be 
# upper and lower pairs for the confidence intervals produced using the bootstrap
# method for each value of sample size and B.

# generalize 2b into a function, and vary inputs of sample size and B as we did above.

boot.sample <- function(sample.size, B){
set.seed(0)
bootstrap=replicate(B,rexp(sample.size,rate=lambda.hat))
bs.matrix=matrix(1/(apply(bootstrap,2,mean)))
bs.se=sd(bs.matrix)
bsCI.high=lambda.hat+1.96*bs.se
bsCI.low=lambda.hat-1.96*bs.se
return(c(bsCI.low,bsCI.high))
}
B=seq(from=1000,to=10000,by=1000)
n=seq(from=100,to=1000,by=100)
ci.high=matrix(nrow=length(n),ncol=length(B))
ci.low=matrix(nrow=length(n),ncol=length(B))
for(i in 1:length(n)){
  for(j in 1:length(B)){
    CI=boot.sample(n[i],B[j])
    ci.high[i,j]=CI[2]
    ci.low[i,j]=CI[1]
  }
 }
rownames(ci.low)=n
colnames(ci.low)=B
rownames(ci.high)=n
colnames(ci.high)=B
ci.low
ci.high

# 3b. (2) Plot your CI limits to show the effect of changing the sample size and 
# changing the number of bootstrap replications. What do you conclude?
library(Hmisc)
errbar(n,rep(lambda.hat,length(n)),ci.low[,1],ci.high[,1],xlab = "Sample size",ylab = "Confidence Limits",main = 'Confidence Limits for B=1000 as sample size varies' )
errbar(B,rep(lambda.hat,length(B)),ci.low[1,],ci.high[1,],xlab = 'B',ylab = 'Confidence Limits',main="Confidence Limits for n=100 as B varies")

# As we increase the sample size, the confidence intervals decrease.
# The increase in Bootstrap replications does not have that much effect on the size of the confidence intervals.

# 4a. (5) In 1961 John Tukey wrote an article called The Future of Data Analysis 
# (it is uploaded in moodle). Some people say it is prophetic regarding the 
# field of Data Science today. Do you agree or disagee? Why or why not? (Please 
# keep your answer less than 500 words).
#I agree with the idea about that it is prophetic regarding the field of Data Science 
#today. Now, seeking to know the future has been man¡¯s eternal quest--from ancient 
#mythology to the 21st century. Although is is true that future is really hard to imagine,
#and the future is full of uncertainty, we can still see some part of the future according 
#to the data science today. There are tons of different kinds of data within different 
#industries. We can easily collect the data which we are interested in according to 
#measuring, observing or even generating and creating by ourselves. We can acquire data 
#from lab, fieldwork, surveys, devices, simulations and so on. Then, we can organize and 
#clean these data properly, to make it suitable for subsequent analysis. After that, we 
#can get many conclusion according to analysis of data. Within this step, we can establish 
#models to predict the possible future. Although it is true that future is always hard to
#predict exactly, we can still get some possible trend or possible expectations. Thus, in
#some way, we can say that it is prophetic regarding the field of Data Science. Since we 
#can find out some conclusion via data analyzing, which we will not notice without data.


# 4b. (5) Relate the article to the Life Cycle of Data discussion from class. 
# You may wish to choose an example or idea from the article and clearly explore how it 
# relates to the Life Cycle of Data. (Please keep your answer less than 500 words).

#There are many challenges within data life circle. For the first phase: data acquisition. 
#Data always has a source, the foremost challenge in this aspect is to define the criteria
#for filters, so as to not lose out any valuable information. Since there are tons of raw
#data every day, this enormity in data is not of much use unless it is filtered on the 
#basis of several criteria. For the second phase: data structuring. But the fact is that
#most data is unstructured and therefore, heterogeneous is nature. Analytic tools 
#therefore need to be smart enough decipher all the diverse natures of data, assimilate 
#them with advanced algorithm development. Besides, increase in mobility and access to 
#information has led to massive discussions around data protections and security. 
#Industries such as banking, health-care are under strict compliance that make it a 
#tough job to create a proper data protection framework. This kind of risk can be really 
#huge sometimes. Therefore, data science not only brings a lot of opportunities to find 
#out many potential valuable discoveries, but also full of challenges. We have to face 
#these challenges and try to overcome them during the data life cycle.









	