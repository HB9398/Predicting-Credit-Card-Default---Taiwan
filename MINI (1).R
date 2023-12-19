# Load necessary libraries
library(readxl)

# Read data from Excel file, skipping the first row
credit <- read_excel("Downloads/credit.csv", skip = 1)
View(credit)  # View the loaded data in RStudio
summary(credit) #View Preliminaryy summary
 
# Convert categorical variables to factors
credit$SEX = as.factor(credit$SEX)
credit$MARRIAGE = as.factor(credit$MARRIAGE)
credit$`default payment next month` = as.factor(credit$`default payment next month`)
credit$EDUCATION = as.factor(credit$EDUCATION)
credit$PAY_0 = as.factor(credit$PAY_0)
credit$PAY_2 = as.factor(credit$PAY_2)
credit$PAY_3 = as.factor(credit$PAY_3)
credit$PAY_4 = as.factor(credit$PAY_4)
credit$PAY_5 = as.factor(credit$PAY_5)
credit$PAY_6 = as.factor(credit$PAY_6)



# Subset data to keep only the desired values in EDUCATION and MARRIAGE
credit <- subset(credit, EDUCATION %in% c("1", "2", "3", "4"))
credit <- subset(credit, MARRIAGE %in% c("1", "2", "3"))

# Rename the 'default payment next month' column to 'payment'
names(credit)[names(credit) == 'default payment next month'] <- 'payment'



# Load the necessary library for logistic regression
library(mfx)

# Fit a logistic regression model
summary(glm(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_3 + PAY_6 + BILL_AMT1 + BILL_AMT3 + BILL_AMT6 + PAY_AMT1 + PAY_AMT3 + PAY_AMT6, credit, family="binomial"))

# Calculate marginal effects for the logistic regression model
logitmfx(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_3 + PAY_6 + BILL_AMT1 + BILL_AMT3 + BILL_AMT6 + PAY_AMT1 + PAY_AMT3 + PAY_AMT6, credit)


# Get numeric variables in the data to run correlation matrix
credit[, c("LIMIT_BAL", "EDUCATION", "MARRIAGE", "AGE", "PAY_0", "PAY_3", "PAY_6", "BILL_AMT1", "BILL_AMT3", "BILL_AMT6", "PAY_AMT1", "PAY_AMT3", "PAY_AMT6")] <- lapply(credit[, c("LIMIT_BAL", "EDUCATION", "MARRIAGE", "AGE", "PAY_0", "PAY_3", "PAY_6", "BILL_AMT1", "BILL_AMT3", "BILL_AMT6", "PAY_AMT1", "PAY_AMT3", "PAY_AMT6")], as.numeric)

# Compute the correlation matrix for selected numeric variables
cor_matrix <- cor(credit[, c("LIMIT_BAL", "EDUCATION", "MARRIAGE", "AGE", "PAY_0", "PAY_3", "PAY_6", "BILL_AMT1", "BILL_AMT3", "BILL_AMT6", "PAY_AMT1", "PAY_AMT3", "PAY_AMT6")])
print(cor_matrix)

# Fit a logistic regression model after normalization
summary(glm(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, credit, family="binomial"))

# Calculate marginal effects for the logistic regression model after normalization
logitmfx(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, credit)

#probit
probit_model <- glm(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, credit, family = binomial(link = "probit"))
summary(probit_model)


# Install and load necessary packages for classification
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
install.packages("party")
library(party)
install.packages("kernlab")
library(kernlab)

# Set seed for reproducibility
set.seed(0)

# Define training control for caret package
train_Control = trainControl(method = "cv", number = 10)

# Train k-Nearest Neighbors classifier
knn_caret = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
                  data = credit, method = "knn", trControl = train_Control,
                  tuneLength = 20)
plot(knn_caret)

y_pred <- predict(knn_caret, newdata = credit)

# Compute confusion matrix for KN
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)

# Train Naive Bayes classifier
nb = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
           data = credit, method = "naive_bayes", trControl = train_Control,
           tuneLength = 10)
summary(nb)
plot(nb)
# Make predictions using Naive Bayes classifier
y_pred <- predict(nb, newdata = credit)

# Compute confusion matrix for Naive Bayes
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)

# Train Naive Bayes classifier Tune Length 30
nbtl = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
           data = credit, method = "naive_bayes", trControl = train_Control,
           tuneLength = 30)
summary(nbtl)
plot(nbtl)

# Make predictions using Naive Bayes classifier Tune Length 30
y_pred <- predict(nbtl, newdata = credit)

# Compute confusion matrix for Naive Bayes Tune Length 30
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)

# Train Support Vector Machine with linear kernel
svm_linear = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
                   data = credit, method = "svmLinear", trControl = train_Control,
                   tuneLength = 10)
svm_linear

# Make predictions using Support Vector Machine with linear kernel
y_pred <- predict(svm_linear, newdata = credit)

# Compute confusion matrix for Support Vector Machine with linear kernel
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)

# Train Random Forest classifier
rf = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
           data = credit, method = "rf", trControl = train_Control,
           tuneLength = 5)
rf
plot(rf)

# Make predictions using RF
y_pred <- predict(rf, newdata = credit)

# Compute confusion matrix for RF
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)


# Train Neural Network classifier
nnet = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
             data = credit, method = "nnet", trControl = train_Control,
             tuneLength = 10)
nnet
plot(nnet)
# Make predictions using RF
y_pred <- predict(nnet, newdata = credit)

# Compute confusion matrix for RF
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)


