# HW 5 - Due Tuesday October 4, 2016 in moodle and hardcopy in class. 
# Upload R file to Moodle with name: HW5_490IDS_YourClassID.R
# Do Not remove any of the comments. These are marked by #

# Please ensure that no identifying information (other than yur class ID) 
# is on your paper copy, including your name

#For this problem we will start with a simulation in order to find out how large n needs
#to be for the binomial distribution to be approximated by the normal
#distribution. 

#We will take m samples from the binomial distribution for some n and p.
#1.(4pts.) Let's let p=1/2, use the rbinom function to generate the sample of size m. 
#Add normal curves to all of the plots. 
#Use 3 values for n, 10, 30, and 50. Display the histograms as well as your
#code below. 
rbinom(1000,10,0.5)
rbinom(1000,30,0.5)
rbinom(1000,50,0.5)
hist(rbinom(1000,10,0.5),freq=FALSE)
curve(dnorm(x, 5, sqrt(2.5)), from = 0, to = 100,add=TRUE)
hist(rbinom(1000,30,0.5),freq=FALSE)
curve(dnorm(x, 15, sqrt(7.5)), from = 0, to = 100,add=TRUE)
hist(rbinom(1000,50,0.5),freq=FALSE)
curve(dnorm(x, 25, sqrt(12.5)), from = 0, to = 100,add=TRUE)



#1b.)(3pts.) Now use the techniques described in class to improve graphs. 
# Explain each step you choose including why you are making the change. You
# might consider creating density plots, changing color, axes, labeling, legend, and others for example.
seq=rbinom(1000,50,0.5)
hist(seq,freq=FALSE,xlab='successful trials',main='normal curve over histogram',col='lightgray')
curve(dnorm(x,25,sqrt(12.5)),from=0,to=100,add=TRUE,col='red',lwd=2)
#(1)Use the ¡°xlab¡± parameter to explain the meaning of x-axis, use the main parameter to explain the meaning of this plot.
#(2)Use the ¡°col¡± parameter to make the histogram and curve more clear.
#(3)Use the ¡°lwd¡± parameter to make the curve thicker and obviously.


#Q2.) (2pts.)
#Why do you think the Data Life Cycle is crucial to understanding the opportunities
#and challenges of making the most of digital data? Give two examples.
#In order to build a capability that can achieve beneficial data targets, we need to understand the data
#lifecycle and challenges at different stages. Here are two examples:
#(1) Most data is unstructured. It is heterogeneous, variable and comes in multiple formats, such as text, document, image, video
#and so on. Analytic tools therefore need to be smart enough to decipher all the diverse natures of data, assimilate them with advanced
#algorithm development, optimization and automation to bring it on a uniform, consumable format.
#(2)Increase in mobility and access to information has led to massive discussions around data governance, protection
#and security. Industries such as banking, healthcare, pharma, and defense are under strict compliance
#and regulatory mandates that make it a tough job to create a proper data protection framework.



###Part 2###
#3.)  San Francisco Housing Data
#
# Load the data into R.
load(url("http://www.stanford.edu/~vcs/StatData/SFHousing.rda"))

# (2 pts.)
# What is the name and class of each object you have loaded into your workspace?
### Your code below
objects()
class(cities)
class(housing)

### Your answer here
#I have loaded two objects: cities and housing, they are data frames.

# What are the names of the vectors in housing?
### Your code below
names(housing)

### Your answer here
#The housing data frame has 15 vectors and their names are listed above.

# How many observations are in housing?
### Your code below
dim(housing)

### Your answer here
#There are 281506 observations in housing.

# Explore the data using the summary function. 
summary(housing)
# Describe in words two problems that you see with the data.
#### Write your response here
#(1)Some of the variables have some problems with missing data.
#There is a large amount of missing data in the housing data frame.
#(2)There is some inconsistencies within the year vector. 
#The range of the year vector is from 0 to 3894, this vector has something wrong clearly.


# Q5. (2 pts.)
# We will work the houses in Albany, Berkeley, Piedmont, and Emeryville only.
# Subset the data frame so that we have only houses in these cities
# and keep only the variables city, zip, price, br, bsqft, and year
# Call this new data frame BerkArea. This data frame should have 4059 observations
# and 6 variables.
somecities=c('Albany','Berkeley','Piedmont','Emeryville')
BerkArea=housing[housing$city %in% somecities,c('city','zip','price','br','bsqft','year')]
dim(BerkArea)

# Q6. (2 pts.)
# We are interested in making plots of price and size of house, but before we do this
# we will further subset the data frame to remove the unusually large values.
# Use the quantile function to determine the 99th percentile of price and bsqft
# and eliminate all of those houses that are above either of these 99th percentiles
# Call this new data frame BerkArea, as well. It should have 3999 observations.
QuantPrice=quantile(x=BerkArea$price,probs = 0.99,na.rm = TRUE)
QuantBsqft=quantile(x=BerkArea$bsqft,probs = 0.99,na.rm = TRUE)
BerkArea=BerkArea[BerkArea$price<=QuantPrice & BerkArea$bsqft<=QuantBsqft, ]
dim(BerkArea)

# Q7 (2 pts.)
# Create a new vector that is called pricepsqft by dividing the sale price by the square footage
# Add this new variable to the data frame.
BerkArea$pricepsqft=BerkArea$price/BerkArea$bsqft


#  Q8 (2 pts.)
# Create a vector called br5 that is the number of bedrooms in the house, except
# if this number is greater than 5, it is set to 5.  That is, if a house has 5 or more
# bedrooms then br5 will be 5. Otherwise it will be the number of bedrooms.
br5=pmin(BerkArea$br,5)

# Q9 (4 pts. 2 + 2 - see below)
# Use the rainbow function to create a vector of 5 colors, call this vector rCols.
# When you call this function, set the alpha argument to 0.25 (we will describe what this does later)
# Create a vector called brCols of 4059 colors where each element's
# color corresponds to the number of bedrooms in the br5.
# For example, if the element in br5 is 3 then the color will be the third color in rCols.

# (2 pts.)
rCols=rainbow(5,alpha = 0.25)
brCols=rCols[br5]


######
# We are now ready to make a plot.
# Try out the following code
plot(pricepsqft ~ bsqft, data = BerkArea,
     main = "Housing prices in the Berkeley Area",
     xlab = "Size of house (square ft)",
     ylab = "Price per square foot",
     col = brCols, pch = 19, cex = 0.5)
legend(legend = 1:5, fill = rCols, "topright")

# (2 pts.)
### What interesting features do you see that you didn't know before making this plot? 
#(1)As the number of square feet increases, the number of bedrooms increases, although with some exception.
#(2)As the size of the house increases, prices of per square foot decreases.
#(3)The majority of houses are between 500 to 2000 square foot in the Berkeley Area.

# (2 pts.)
# Replicate the boxplots presented in class, with the boxplots sorted by median housing price (slide 45 of the lecture notes)
BerkArea$city=factor(BerkArea$city)
BerkArea$city=with(BerkArea, reorder(city,price, mean))
boxplot(BerkArea$price~BerkArea$city,data=BerkArea,las=2)
