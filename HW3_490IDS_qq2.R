# HW 3 - Due Tuesday Sept 20, 2016. Upload R file to Moodle with name: HW3_490IDS_YOURNETID.R
# Do Not remove any of the comments. These are marked by #
# The .R file will contain your code and answers to questions.

#Name:QI QI

# Main topic: Using the "apply" family function

#Q1 (5 pts)
# Given a function below,
myfunc <- function(z) return(c(z,z^2, z^3%/%2))
#(1) Examine the following code, and briefly explain what it is doing.
y = 2:8
myfunc(y)
matrix(myfunc(y),ncol=3)
### Your explanation
#Create a vector named "y" that values 2 through 8. 
#Running the function created above (myfunc) on the values of the "y" vector,
#and returns a new vector with 21 values, which included the original values of the "y" vector, the squared values of the "y" vector, 
#and y^3%/%2.
#The matrix() creates a matrix with 3 columns with the 21 values of the vector obtained from running the function "myfunc()".

#(2) Simplify the code in (1) using one of the "apply" functions and save the result as m.
###code & result
m=matrix(data=c(2:8,sapply(2:8,function(y) y^2),sapply(2:8,function(y) y^3%/%2)),ncol=3)
m

#(3) Find the row product of m.
###code & result
apply(m,1,prod)

#(4) Find the column sum of m in two ways.
###code & result
apply(m,2,sum)
apply(m,2,function(m) sum(m))
colSums(m)

#(5) Could you divide all the values by 2 in two ways?
### code & result
m/2
apply(m,c(1,2),function(m) m/2)


#Q2 (8 pts)
#Create a list with 2 elements as follows:
l <- list(a = 1:10, b = 11:20)
#(1) What is the product of the values in each element?
lapply(l,prod)

#(2) What is the (sample) variance of the values in each element?
lapply(l,var)

#(3) What type of object is returned if you use lapply? sapply? Show your R code that finds these answers.
lapply(l,sum)
class(lapply(l,sum))
#lapply always return a list.
sapply(l,sum)
class(sapply(l,sum))
#sapply always return a vector.

# Now create the following list:
l.2 <- list(c = c(21:30), d = c(31:40))
#(4) What is the sum of the corresponding elements of l and l.2, using one function call?
mapply("+",l,l.2)

#(5) Take the log of each element in the list l:
lapply(l,log)
mapply(log,l)

#(6) First change l and l.2 into matrixes, make each element in the list as column,
### your code here
l=as.matrix(as.data.frame(l))
l

l.2=as.matrix(as.data.frame(l.2))
l.2

#Then, form a list named mylist using l,l.2 and m (from Q1) (in this order).
### your code here
mylist=list(l,l.2,m)

#Then, select the first column of each elements in mylist in one function call (hint '[' is the select operator).
### your code here
lapply(mylist, `[`, ,1)

#Q3 (3 pts)
# Let's load our friend family data again.
load(url("http://courseweb.lis.illinois.edu/~jguo24/family.rda"))
#(1) Find the mean bmi by gender in one function call.
tapply(family$height,family$gender,mean)

#(2) Could you get a vector of what the type of variables the dataset is made ofï¼?
sapply(family,class)

#(3) Could you sort the firstName in height descending order?
family$firstName[order(fheight,decreasing = TRUE)]

#Q4 (2 pts)
# There is a famous dataset in R called "iris." It should already be loaded
# in R for you. If you type in ?iris you can see some documentation. Familiarize 
# yourself with this dataset.
#(1) Find the mean petal length by species.
### code & result
lapply(list(iris$Petal.Length[iris$Species=='setosa'],iris$Petal.Length[iris$Species=='versicolor'],iris$Petal.Length[iris$Species=='virginica']),mean)

#(2) Now obtain the sum of the first 4 variables, by species, but using only one function call.
### code & result
aggregate(iris[ ,1:4],by=list(Species=iris$Species),sum)

#Q5 (2 pts)
#Below are two statements, their results have different structure, 
lapply(1:4, function(x) x^3)
sapply(1:4, function(x) x^3)
# Could you change one of them to make the two statements return the same results (type of object)?
as.list(sapply(1:4,function(x) x^3))

#Q6. (5 pts) Using the family data, fit a linear regression model to predict 
# weight from height. Place your code and output (the model) below. 
lmfit=lm(family$height~family$weight)
lmfit
# How do you interpret this model?
summary(lmfit)
plot(lmfit)
#This predict model means that there is linear relationship between weight and weight:Height = 54.39428 + 0.08009 * weight.The higher the height, the heavier the weight.
#Generally, this linear model fitted well.From the plot of Residuals vs Fitted, we can see that 
#there are only two samples which is accidental. According to the Normal Q-Q plot,
#the points fall approximately on a straight line, so the data basically followed the linear distribution.
#Based on the residuals and coefficients form summary(), the error of this model is relatively small.


# Create a scatterplot of height vs weight. Add the linear regression line you found above.
# Provide an interpretation for your plot.
plot(family$weight,family$height,main="scatterplot",xlab = "weight",ylab = 'height')
abline(lm(family$height~family$weight),col="red")
#The scatterplot shows the distribution of weight and height, and the red line shows linear model.Generally, there is linear relationship between the people's height and weight.
#The real data of height and weight almost fit well with the linear model, which means that this hypothesis prediction is correct. 

