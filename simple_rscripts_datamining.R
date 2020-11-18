
# R Scripts for Data Mining; Problem 2-5
# Author: Shantanu Neema

# Given data (from problem 1):
HomeOwner <- c("Yes", "No", "No", "Yes", "No", "No", "Yes", "No", "No", "No")
MaritalStatus <- c("Single", "Married", "Single", "Married", "Divorced", "Married", "Divorced", "Single", "Married", "Single")
AnnualIncome <- c(125000, 100000, 70000, 120000, 95000, 60000, 220000, 85000, 75000, 90000)
DefaultedBorrower <- c("No", "No", "No", "No", "Yes", "No", "No", "Yes", "No", "Yes")
data.table <- data.frame(HO = factor(HomeOwner), MS = factor(MaritalStatus), AI = AnnualIncome, DB = factor(DefaultedBorrower))
str(data.table) # Check data types (1 numeric, 3 catagorical)
# In above dataset; DB=Defaulted Borrower, AI=Annual Income, MS=Marital Status & HO=Home Owner

# Load required packages
install.packages("C50")
require(C50)
install.packages("rJava")
require(rJava)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
install.packages("RWeka")
require(RWeka)

# Ans 2. Predicting class label for test data
# Test data for problem 2:
test.data <- data.frame(HO = "No", MS = "Married", AI = 120000) 
test.data
# Construction of decision tree model:
model_DT <- C5.0(data.table[,-4],data.table$DB)
summary(model_DT)
# Predict class label for the test data
TestClass <- predict(model_DT, test.data)
TestProb <- predict(model_DT,test.data,type="prob")
cat("Class label for given test data is:",
    as.character(TestClass),"with",max(TestProb)*100,"% probability")

# Ans 3. Try RIPPER algorithm to find rules that accurately depict the complete set
# Total 7 RIPPER Models can be created using different sets of attributes
model_JRip <- JRip(DB ~., data=data.table) #1 Rule (high accuracy)

model_JRip1 <- JRip(DB ~ HO, data=data.table) #1 Rule (high error)
model_JRip2 <- JRip(DB ~ MS, data=data.table) #1 Rule (high error)
model_JRip3 <- JRip(DB ~ AI, data=data.table) #2 Rules (high accuracy)

model_JRip4 <- JRip(DB ~ MS + AI, data=data.table) #2 Rules (high accuracy)
model_JRip5 <- JRip(DB ~ HO + MS, data=data.table) #1 Rule (high error)
model_JRip6 <- JRip(DB ~ HO + AI, data=data.table) #2 Rules (high accuracy)

predict(model_JRip,test.data,type="prob") #Check the probabilities for test data

# From above 7 models from different combinations of attributes;
# 2 models can be summarized; 1. Model with high error & 1 Rule
#                             2. Model with high accuracy & 2 Rules

# Summary of Model 1:
model_JRip # Shows the Rule 
summary(model_JRip) # shows high absolute error
# Summary of Model 2: (model_JRip3, model_JRip4 & model_JRip6)
model_JRip3 # Shows both Rules
summary(model_JRip3) # Shows 100 % accuracy with Rules

# From above; following rule can be concluded:
# Rule: For AI between 85000 to 95000 the person will default the loan

# Ans 4. Learn rules using C5.0 decision tree
model_DT_rule <- C5.0(data.table[,-4],data.table$DB,rules=TRUE)
summary(model_DT_rule)
predict(model_DT_rule,test.data,type="prob")

# After trying with different combinations like in Ans 3; 
# Following 2 combinations generate some rules

model_Rule <- C5.0(DB ~., data=data.table,rules=TRUE) #1 Rule (high error)
model_Rule

# Above model has No of rules = 0; to get some rules; 
# one can use combination of attributes like in Ans 3.

# Following combinations were found generating rules
model_Rule1 <- C5.0(DB ~ AI, data=data.table,rules=TRUE) 
summary(model_Rule1) # 3 Rules with 0 % error
model_Rule2 <- C5.0(DB ~ HO+AI, data=data.table,rules=TRUE) 
summary(model_Rule2) # 3 Rules with 10% error

