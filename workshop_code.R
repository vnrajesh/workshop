## Code: Install some R packages that would be used today
#####################################################################

#Install R Packages

install.packages('jsonlite')
install.packages('car')
install.packages('dplyr')
install.packages('XML')
install.packages('rjson')
install.packages('plyr')
install.packages('data.table')
install.packages('rio')
install.packages('ggplot2')
install.packages('devtools')
install.packages('rtools')
install.packages('rattle')
devtools::install_github("kassambara/datarium")

####################################################################
## For loading a package in R we use the command library('packageName')

# Load the package rjson 

library('rjson')

# Reading a json file into R

folder<- "C:/workshop/data"

file_name <- "sampleInput.json"


rawData <- paste(folder, file_name, sep="/")

#Read the json file

data <- fromJSON(file = rawData)
print(data)

# This gives a list
# Convert the list to a data frame

data <- as.data.frame(data)

print(data)

#write the data to a csv file
write.csv(data, paste(folder, "EmployeeInfo.csv", sep="/"), row.names = FALSE)

################################################################################

##Importing an XML file and converting it to a data frame.,

# Load the packages required to read XML files.
library("XML")
library("methods")

file_name <- "employeeData.xml"
rawData <- paste(folder, file_name, sep="/")

# Convert the input xml file to a data frame.
xmldataframe <- xmlToDataFrame(rawData)

print(xmldataframe)


######################################################
############### Working with Directories #############
######################################################

getwd() #shows the working directory

setwd("C:/workshop/data") # changes the working directory

dir() # Lists files in the working directory

dir.create("C:/workshop/test") # Creates folder 'test' in  'c:/workshop'


#############Download from the Internet ######################################
## For testing purposes, download a file from the Internet ###


download.file("https://github.com/vnrajesh/workshop/raw/master/House_Price.csv",
              "C:/workshop/workshop_house_data.csv",
              method="auto",
              quiet=FALSE,
              mode = "wb",
              cacheOK = TRUE)



##############################################################################
################### Exploratory Data Analysis ################################
##############################################################################

file_name <- "Auto.csv"

auto = read.csv(file = paste(folder, file_name, sep='/'), header = TRUE,
                stringsAsFactors = FALSE)


str(auto) # Provides structure of the data

head(auto) # Prints the first six rows
head(auto, n=10) # Prints the first ten rows of the dataset

tail(auto) # Prints the last six rows
tail(auto, n=10) # Prints the last ten rows of the data set

summary(auto) # Prints the basic descriptive characteristics of the various variables

mean(auto$mpg)
sd(auto$mpg)

###########  Scatter Plots ##############

plot( auto$cylinders, auto$horsepower)

#Higher the number of cylinders, greater is the horsepower

plot(auto$horsepower, auto$acceleration)
plot(auto$weight, auto$acceleration)


##### Simple Linear Regression ###########################

model <- lm(formula = acceleration ~ weight, data= auto)

summary(model)

library('ggplot2') # Load the package

ggplot(auto, aes(weight, acceleration)) +
  geom_point() +
  stat_smooth(method = lm)

############Example 2 ######################################


# Load the package
data("marketing", package = "datarium")

head(marketing, 4)

write.csv(marketing, "C:/workshop/data/marketing.csv", row.names = FALSE)

# The marketing dataset contains the impact of budget spend on 
# three advertising channels (youtube, facebook and newspaper) on sales.
# Data are the advertising budget in thousands of dollars along with the sales.
# The advertising experiment has been repeated 200 times with different budgets 
# and the observed sales have been recorded.

marketing = read.csv("C:/workshop/data/marketing.csv")

# We want to predict future sales on the basis of advertising budget spent 
# on youtube.

# Visualization - create a scatter plot displaying the sales units with youtube advertising 
# budget and add a smoothened line.

ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth()

# Creating a new variable as sum of all individual advertising expenses
marketing$advertising_budget = marketing$youtube + 
  marketing$facebook + marketing$newspaper

ggplot(marketing, aes(x = advertising_budget, y = sales)) +
  geom_point() +
  stat_smooth()

# Computing the correlation coefficient between sales and the independent variables

cor(marketing$sales, marketing$advertising_budget)

cor(marketing$sales, marketing$youtube)
cor(marketing$sales, marketing$facebook)
cor(marketing$sales, marketing$newspaper)

#Fitting a regression model

model1 <- lm(sales~ youtube, data=marketing)
summary(model1)

# Compute the confidence intervals for the two beta coefficients

confint(model1, level=0.95) #95% confidence interval

confint(model1, level=0.99) # 99% confidence interval

sigma(model1)*100/mean(marketing$sales) # Percentage error

##write the summary to a text file

s <- summary(model1)
capture.output(s, file=paste(folder, "model1_summary.txt", sep="/"))

###################################################################



ggplot(marketing, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm)


model <- lm(sales~advertising_budget, data=marketing)
summary(model)

ggplot(marketing, aes(advertising_budget, sales)) +
  geom_point() +
  stat_smooth(method = lm)


#The total advertising budget is a much better predictor of sales than only the 
# amount spent on youtube.


###############################################################################
library("dplyr")

file_name <- "House_Price.csv"

data_file <- paste(folder, file_name, sep = '/')

housing_data <- read.csv(data_file, stringsAsFactors = FALSE, header = TRUE )
head(housing_data)
str(housing_data)

housing_data <- dplyr::select(housing_data, c("LotArea", "YrSold", "SalePrice",
                              "YearBuilt")) %>%
                dplyr::mutate(Age = YrSold - YearBuilt) 

ggplot(housing_data, aes(x = LotArea, y = SalePrice)) +
  geom_point() +
  stat_smooth()

###Simple Regression Model 1- Sale Price as a function of LotArea ######

model_housing1 <- lm(SalePrice ~ LotArea, data = housing_data)
summary(model_housing1)

ggplot(housing_data, aes(LotArea, SalePrice)) +
  geom_point() +
  stat_smooth(method = lm)

####Simple Regression Model 2- Sale Price as a function of House Age ####

ggplot(housing_data, aes(x = Age, y = SalePrice)) +
  geom_point() +
  stat_smooth()

## As age increases the price decreases

model_housing2 <- lm(SalePrice ~ Age, data = housing_data)
summary(model_housing2)

ggplot(housing_data, aes(Age, SalePrice)) +
  geom_point() +
  stat_smooth(method = lm)

###########################################################################
#### Both models do not completely explain the variance of the dependent variable

###### Multiple Linear Regression

model_housing_multiple <- lm(SalePrice ~ Age + LotArea , data = housing_data)

summary(model_housing_multiple)

##############################################################################

### Training and Testing a Linear Regression Model

##Step 1: Create the training (development) and test (validation) 
# data samples from original data.
## Data == housing_data, Multiple Regression Model

#############################################################################

nrow(housing_data)

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling

trainingRowIndex <- sample(1:nrow(housing_data), 0.7*nrow(housing_data))  # row indices for training data

trainingData <- housing_data[trainingRowIndex, ]  # model training data
testData  <- housing_data[-trainingRowIndex, ]   # test data

#########################

# Step 2: Develop the model on the training data and 
#use it to predict the SalePrice on test data

# Build the model on training data -

lmMod <- lm(SalePrice ~ Age + LotArea , data = trainingData)  # build the model
SalePricePred <- predict(lmMod, testData)  # predict Sales Price

#Step 3: Review diagnostic measures.

summary(lmMod) #Model Summary

# Step 4: Calculate prediction accuracy and error rates

actuals_preds <- data.frame(cbind(actuals=testData$SalePrice, predicteds=SalePricePred)) 
# make actuals_predicteds dataframe.

head(actuals_preds)

correlation_accuracy <- cor(actuals_preds$actuals, actuals_preds$predicteds)
#correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy # 59.4%
head(actuals_preds)

#Now lets calculate the Min Max accuracy and MAPE: 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 3.22073e-06, min_max accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 28.23% , mean absolute percentage deviation

################################################################################
################################################################################
#### Cluster Analysis - K-means ################################################

library('rattle')

data(wine, package='rattle')

head(wine)
nrow(wine)  # Find the number of types of wines
str(wine)

#Standardize the variables
wine.stand <- scale(wine[-1])  # To standarize the variables

# K-Means
k.means.fit <- kmeans(wine.stand, 3) # k = 3, number of clusters

attributes(k.means.fit)

# Centroids:

k.means.fit$centers

## Clusters: which wine belongs to which cluster

k.means.fit$cluster

# Cluster size:
k.means.fit$size

# How to choose the number of clusters

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wine.stand, nc=6) 

# Library clusters allow us to represent (with the aid of PCA) 
#the cluster solution into 2 dimensions:

library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#######Performing Hierarchical Clustering ##################

## Euclidean distance matrix

d <- dist(wine.stand, method = "euclidean") # Euclidean distance matrix.

# Euclidean distance is the input for the clustering algorithm 
#(Ward's minimum variance criterion minimizes the total within-cluster variance)

H.fit <- hclust(d, method="ward")


# The clustering output is displayed in a dendrogram

plot(H.fit) # display dendogram

groups <- cutree(H.fit, k=3) # cut tree into 3 clusters

# draw dendogram with red borders around the 3 clusters
rect.hclust(H.fit, k=3, border="red")

#confusion matrix

table(wine[,1],groups)

### European Protein Consumption ###########

# 25 countries
# Protein Intake from nine major food sources

url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'

food <- read.csv(url)

write.csv(food, file = "C:/workshop/data/food.csv", row.names = FALSE)

head(food)

## Which countries are close to each other in terms of their consumption
## of food from different sources?

## Cluster on two parameters Red and White meat (p=2) and k=3 clusters.

set.seed(123456789) ## to fix the random starting clusters

grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)

grpMeat

## list of cluster assignments

o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

## Graphical representation of the Cluster solution

plot(food$Red, food$White, type="n", xlim=c(3,19), 
      xlab="Red Meat", ylab="White Meat")

text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)


##Clustering on all attributes
## same analysis, but now with clustering on all
## protein groups change the number of clusters to 7

set.seed(123456789)
grpProtein <- kmeans(food[,-1], centers=7, nstart=10)
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])

library(cluster)
clusplot(food[,-1], grpProtein$cluster, 
         main='2D representation of the Cluster solution', 
         color=TRUE, shade=TRUE, labels=2, lines=0)

## Hierarchical Clustering

# Function used: agnes from the package cluster. 
# Argument diss=FALSE indicates that we use the dissimilarity matrix 
# calculated from raw data. 
# Argument metric="euclidian" indicates that we use Euclidean distance.
# No standardization is used and the link function is the "average" linkage.

foodagg = agnes(food,diss=FALSE,metric="euclidian")

plot(foodagg, main='Dendrogram') ## dendrogram

groups <- cutree(foodagg, k=4) # cut tree into 4 clusters
rect.hclust(foodagg, k=4, border="red") 
