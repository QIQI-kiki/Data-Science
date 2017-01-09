# HW 1 Due Tuesday Sept 6, 2016. Upload R file to Moodle with name: HW1_490IDS_YOURUNI.R
# Do Not remove any of the comments. These are marked by #

###Name: Qi Qi

# Load the data for this assignment into your R session 
# with the following command:

load(url("http://courseweb.lis.illinois.edu/~jguo24/SFTemps.rda"))

# Check to see that the data were loaded by running:
objects()
# This should show five variables: dates, dayOfMonth, month, temp, and year

# Use the length() function to find out how many observations there are.
length(dates)
length(dayOfMonth)
length(month)
length(temp)
length(year)
# For the following questions, use one of: head(), summary(),
# class(), min(), max(), hist(), quantile() to answer the questions.

# 1. (1) What was the coldest temperature recorded in this time period?
  min(temp,na.rm=TRUE)
   #answer is: 38.3
   
# 2. (1) What was the average temperature recorded in this time period?
   mean(temp,na.rm=TRUE)
  #answer is: 56.95646
   
# 3. (2) What does the distribution of temperatures look like, i.e.
# are there roughly as many warm as cold days, are the temps
# clustered around one value or spread evenly across the range
# of observed temperatures, etc.?
   hist(temp)
#We can observe that majority of data is clustered around 55-60 degrees, followed by 50-55, then 60-65.
   summary(temp)
# these results are corresponded with the histogram.
# the mean(56.96) and median(57) are in the range of 55-60.
   quantile(temp, na.rm=TRUE)
# 50% of the temperatures are between the first quantile(53.0) and third quantile(60.8)
# Thus, this distribution is like a normal distribution.
   
# 4. (1) Examine the first few values of dates. These are a special
# type of data. Confirm this with class().
head(dates)
class(dates)

# 5. (1) We would like to convert the temperature from Farenheit to Celsius.
# Below are several attempts to do so that each fail.  
# Try running each expression in R. 
# Record the error message in a comment
# Explain what it means. 
# Be sure to directly relate the wording of the error message with the problem you find in the expression.

(temp -32)
### Error message here  
#no error message here

### Explanation here 
#In this command, we simply subtract 32 from every value(desides NA)
#of the temp variable. But this is not the correct formula to obtain
# Celsius degrees.

(temp - 32)5/9
### Error message here  
#Error: unexpected numeric constant in "(temp - 32)5"

### Explanation here   
# There is no numeric operator before "5".
# We can fix it by introducing an operator between the (temp-32) and 5:(temp-32)*5/9.

5/9(temp - 32)
### Error message here
# Error: attempt to apply non-function

### Explanation here
# 5/9 has not been encased in parenthesis and defined as a function.
# there is no operator between 5/9 and(temp-32).
#we can fix it by introducing an operator: (5/9)*(temp-32).


[temp - 32]5/9
### Error message here
#Error: unexpected '[' in "["

### Explanation here
# Use wrong type of parenthesis. [] are used to access specific elements of a vector.
# we can fix it by changing parentheses [] to ().
#However, even with the correct (),this command failed again.
#we should also introduce an operation:(temp-32)*5/9.

# 6. (1) Provide a well-formed expression that correctly performs the 
# calculation that we want. Assign the converted values to tempC.
tempC=(temp-32)*5/9
tempC

# 7. Run the following code to make a plot.
# (don't worry right now about what this code is doing)

plot(temp~dates, col = rainbow(12)[month], type="p", pch=19, cex = 0.3)

# (1) Use the Zoom button in the Plots window to enlarge the plot.
# Resize the plot so that it is long and short, so it is easier to read.
# Include this plot in the homework your turn in.

# (1) Make an interesting observation about temp in the Bay Area
# based on this plot (something that you couldn't see with
# the calculations so far.)

### Your answer goes here
#Tempreture plot in the Bay Area shows a cyclical process as time goes by, which follows a consistent pattern of increase and decrease.
#We can observe that for the first half year, the temperature is below 60 degrees, indicating that this plot starts in winter.
#For the next half year, the temperature increases to above 60 degrees.
#This type of cyclical process is not obvious just with the raw data.

# (1) What interesting question about the weather in the SF Bay Area
# would you like to answer with these data, but don't yet know 
# how to do it? 

### Your answer goes here
## I am interested in the variation trend of average annual temperature as time passes.


# For the remainder of this assignment we will work with 
# one of the random number generators in R.


# 8. (5). Use the following information about you to generate
# some random values:  
#a. Use the day of the month you were born for the mean of the normal.
#b.	Use your year of birth for the standard deviation (sd) of the normal curve.
#c.	Generate 5 random values using the parameters from a and b.
#d.	Assign the values to a variable named with your first name.
#e.	Provide the values generated.
qi=rnorm(n=1:5,mean=9,sd=1993)
qi

# 9. (1). Generate a vector called "normsamps" containing
# 100 random samples from a normal distribution with
# mean 2 and SD 1.
normsamps=rnorm(100,2,1)

# 10. (1). Calculate the mean and sd of the 100 values.

### The return values from your computation go here
mean(normsamps)
sd(normsamps)

# 11. (1). Use implicit coercion of logical to numeric to calculate
# the fraction of the values in normsamps that are more than 3.
sum(as.numeric(normsamps>3))/length(normsamps)

# 12. (1). Look up the help for rnorm.
# You will see a few other functions listed.  
# Use one of them to figure out about what answer you 
# should expect for the previous problem.  
# That is, find the area under the normal(2, 1) curve
# to the right of 3.  This should be the chance of getting
# a random value more than 3. What value do you expect? 
# What value did you get? Why might they be different?
?rnorm
pnorm(q=3,mean=2,sd=1,lower.tail = FALSE)
#This value should be identical with the result from the exercise above (0.18).
#But I get the value 0.1586553.
#They are different because the sample from previous problem contains only 100 values, which is not enough to be accurate.
#If we use 10000 values, its result will be 0.1582, which is almost identical with 0,1586553.
#Thus, we always need a big sample volume to achieve enough accuracy.

