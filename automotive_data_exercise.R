# ---
# Title: "Automotive data analysis"
# Author: "Shantanu Neema"
# Date: "March 24th, 2017"
# ---
  
# Problem 1.
# Read the automobile data into R

Autodata <- read.csv("Auto.csv", header = TRUE,stringsAsFactors=FALSE)
# Replace "?" with "NA"
Autodata <- replace(Autodata,Autodata == "?", "NA")
Autodata$horsepower <- as.integer(Autodata$horsepower)

# 1.	Plot mpg as the response and horsepower as the predictor. 
# Use the abline() function to display the least squares regression line [5 points].

LMSum <- lm(Autodata$mpg~horsepower, data = Autodata)
cf <- round(coef(LMSum),3) 
## sign check to avoid having plus followed by minus for negative coefficients
eq <- paste0("mpg = ",cf[2], "hp",ifelse(sign(cf[1])==1," + "," - "),abs(cf[1]))
plot(Autodata$horsepower,Autodata$mpg,pch=1,col='black',
     main='Horsepower vs mpg',xlab='Horsepower',ylab='mpg',cex.main=1.25)
mtext(eq,3,line=-2)
abline(LMSum,col='black',lwd=2)

# 2.	Use the lm() function to perform a simple linear regression with;
# mpg as the response and horsepower as the predictor [5 points]. 
# Use the summary() function to print the results and answer the following questions: 
  
LMSum <- lm(Autodata$mpg~horsepower, data = Autodata)
summary(LMSum)

# Part (a)
# To extract P-value
cat("p-value for the regression is:",anova(LMSum)$'Pr(>F)'[1])
# Above results shows a lower p-value (close to 0) 

# part (b)
# To extract R-square
cat("R-square for the regression line is:",summary(LMSum)$r.square)
# Results from R-square shows that about 60% of the variation 
# in mpg can be explained from the linear relationship with the horsepower. 

# Part (c)
slope <- summary(LMSum)$coefficient[2]
cat("The slope of the regression line is:",round(slope,3))
# Since, the slope of the linear regression is negative response 
# (i.e. mpg) decreases with increase in predictor (i.e. horsepower)

# Part (d)
mpg_predicted <- predict(LMSum, data.frame(horsepower = 98))
cat("predicted mpg with given horsepower = 98 is:",round(mpg_predicted,2),"mpg")
# Predicted mpg is: 24.47 (approx.)

# Problem 2: Multiple Regression [30 points]

# 1.	Produce a scatterplot matrix which includes all of the variables in the data set.
plot(na.omit(Autodata[,-9]))

# 2.	Compute the matrix of correlations 
cordata <- round(cor(na.omit(Autodata[,-9])),2)
print("Correlation Matrix for the given data is shown below:")
cordata

library('gplots')
heatmap.2(as.matrix(cordata),col=bluered,scale = "none", 
          breaks= seq(from=-1, to= 1, by = 0.1),Rowv=FALSE,Colv=FALSE,dendrogram="none",trace="none")

# 3.	Use the lm() function to perform a multiple linear regression with mpg as the response
  
MLMSum <- lm(Autodata$mpg~., data = Autodata[,-9])
summary(MLMSum)

# Part (a)
# To extract P-value
cat("p-value for the regression is:",anova(MLMSum)$'Pr(>F)'[1])

# Part (b)
# From summary of the regression model: Displacement, weight, year & Origin 
# are statistically significant as they have p-values lower than 0.05
  
# Part (c)
# The coefficient for year is approximately 0.75 which means 
# when every other predictor held constant, the mpg value increases by 0.75 with each year.
  
# 4. Use the * symbol to fit linear regression models with interaction effects.
# decide interacting parameters 
# (based on highly correlated parameters from the correleation table)  

fit1 <- lm(mpg ~ displacement * horsepower+horsepower * weight, data = Autodata[, -9])
summary(fit1)

fit2 <- lm(mpg ~ displacement * horsepower+displacement * weight, data = Autodata[, -9])
summary(fit2)

fit3 <- lm(mpg ~ displacement * cylinders+displacement * weight, data = Autodata[, -9])
summary(fit3)

fit4 <- lm(mpg ~ displacement * cylinders + displacement * weight + displacement * horsepower 
           + horsepower * weight, data = Autodata[, -9])
summary(fit4)

# From above; fit2 seems to be statistically significant because:
# i.  the p-value for displacement-horsepower pair is the lowest, and
# ii. the r-squared value for the overall model is around 0.75
  
# Problem 3: Implementing linear regression case study 
# Predicting medical expenses using linear regression
  
#--------------------------------------------------------------
  
# Reading file into R

insurance_data <- read.csv("insurance.csv", header = TRUE)
str(insurance_data)

# Displaying summary of expenses variable
summary(insurance_data$expenses)

# Creating histogram of insurance charges
hist(insurance_data$expenses)

# Creating table for region
table(insurance_data$region)

# Exploring relationships among features: correlation matrix
cor(insurance_data[c("age", "bmi", "children", "expenses")])

# Visualing relationships among features: scatterplot matrix
pairs(insurance_data[c("age", "bmi", "children", "expenses")])

# More informative scatterplot matrix
library(psych)
pairs.panels(insurance_data[c("age", "bmi", "children", "expenses")])

# Training a model
insurance_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
data = insurance_data)

# Another way to write
# insurance_model <- lm(expenses ~ ., data = insurance_data) 
# Displaying the estimated beta coefficients
insurance_model

# Evaluating model performance
# Displaying more detail about the estimated beta coefficients

summary(insurance_model)
  
# Improving model performance
# Adding a higher-order "age" term

insurance_data$age2 <- insurance_data$age^2

# Adding an indicator for BMI >= 30

insurance_data$bmi30 <- ifelse(insurance_data$bmi >= 30, 1, 0)

# Creating final model

insurance_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
bmi30*smoker + region, data = insurance_data)

# Displaying summary of second insurance model
summary(insurance_model2)

# Problem 4: Implementing regression & model trees
# Predict quality of wines
  
#--------------------------------------------------------------

# Reading data in R
# Load wine data for white wines and red wines in R
wwine <- read.csv("whitewines.csv",header=TRUE,sep=",")
rwine <- read.csv("redwines.csv",header=TRUE,sep=",")

# Check the data type
  str(wwine)

# Create histogram
hist(wwine$quality) # Optimum bin-size will be selected by R

# Create train & Test data (White wine)
wine_train <- wwine[1:3750, ]
wine_test <- wwine[3751:4898, ]

# Training the data & plot it

library('rpart')
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
  
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart,digits=4,fallen.leaves=TRUE, type=3, extra=101)

# Evaluate the model performance
p.rpart <- predict(m.rpart, wine_test)
print("Summary of predictions:")
summary(p.rpart)
cat("\n")
print("Summary of wine quality from the test data:")
summary(wine_test$quality)

cat("Correlation of predicted and actual values:",round(cor(p.rpart, wine_test$quality),4),"\n")
MAE <- function(actual, predicted) {
mean(abs(actual - predicted))
}
p.mae <- MAE(p.rpart, wine_test$quality)
cat("Mean absolute error for the predicted values is:",round(p.mae,4),"\n")
m.mae <- MAE(mean(wine_train$quality), wine_test$quality)
cat("Mean absolute error if all predictions were same as mean:",round(m.mae,4),"\n")

# Use of M5P function from library RWeka:
# Improve the model

Sys.getenv("WEKA_HOME") # where does it point to? Maybe some obscure path? 
# if yes, correct the variable:
Sys.setenv(WEKA_HOME="C:\\MY\\PATH\\WEKA_WPM")
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
cat("Summary of the M5P model is:","\n")
summary(m.m5p)
cat("\n")
cat("Predict the results using above model","\n")
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
  
cat("The correlation coefficient is:", round(cor(p.m5p, wine_test$quality),4),"\n")
cat("Mean absolute error is:",round(MAE(wine_test$quality, p.m5p),4),"\n")
  
# Both correlation coefficient and the mean absolute error shows little improvement. 
  
  
  
  
  
  
  
  
  
  
  
  