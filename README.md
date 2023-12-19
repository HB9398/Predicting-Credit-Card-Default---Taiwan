
Accuracy for models:
1. KNN - Accuracy:  0.7776088  - 41st Neighbour – Tune length = 20
2. Naïve Bayes - Accuracy: 0.7783 – Tune Length = 10 & 30
3. SVM Linear -  Accuracy: 0.7769  - Tune Length  = 10
4. Random Forest -  Accuracy:  0.7885888 – Tune Length = 5
5. Neural Net - Accuracy: 0.7769 – Tune Length = 10
6. Randomized Extra Test:
Random Forest - Accuracy: 0.7844001   – Tune Length = 10 & 20  with different predictors (6) EDUCATION, MARRIAGE, AGE, PAY_6, BILL_AMT1, PAY_AMT3

Conclusion:
1. Finished pre-processing the data by removing rows with erroneous data, converting variables to factor variables, modifying the data as per the data description, and checking for missing values. 
2. Probit and Logit models implemented
3. Examined Collinearity between dependent variables.
4. Probit and Logit models implemented with non-collinear models
5. Used best model as per AIC score (logit)
6. Executed multiple classification models. 
7. A comparison of these models' accuracy. 
8. Concluded that Random Forest can give the best accuracy at 0.7885 at Tune Length = 5


Input and Explanation:

Data Retrieval and Cleaning:
I first read the file and skipped the first column for ease of work. I then viewed the data and ran a preliminary summary of statistics. I confirmed that there were no missing values. I factorized Sex, Marriage, Default payments, education, and the ‘history of past payment’  for clearer analysis. I am then subsetted education. Further, I have converted the name of the last column, the 'default payment next month' column to 'payment', for ease of use within the code.
The history of past payments has data points 0 and -2 which are not described in the data set. We know -1 represents paid duly, 1,2,3,… represent delayed payments by 1,2,3... month respectively. I am not removing these values as people do pay in advance and will provide a more comprehensive overview of people who pay in advance.


# Load necessary libraries
library(readxl)

# Read data from Excel file, skipping the first row
credit <- read_excel("Downloads/credit.csv", skip = 1)
View(credit)  # View the loaded data in RStudio

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


#-------------





Predictors and Reasoning for Logistic Regression:
For Predictors, I first tried to use variables across the given time period to get a historical overview of the predictions. I then obtained a correlation matrix to check if any of the variables had high correlation. I used threshold values between 0.6 and -0.6 to filter out some of the variables.
The final set of variables I used in my logit model were LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3

Reasoning: 
Sex: Is not used as that seems to be irrelevant
Marriage: Does add financial stress and shows optimal collinearity to be used as a predictor.
Age: Often but not always reflects maturity and shows optimal collinearity to be used as a predictor.
PAY_6, BILL_AMT1, PAY_AMT1, PAY_AMT2, PAY_AMT3: These have been used to provide a holistic dataset that covers the entire time frame and avoids highly correlated data. Pay_1 and Bill_AMT6 could have also been used instead of PAY_6, BILL_AMT1, and would have yielded similar results. All cannot be used together simultaneously due to the high correlation.


# Load the necessary library for logistic regression
library(mfx)

# Fit a logistic regression model (first try) AIC 26043 and Fisher Score: 9
summary(glm(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_3 + PAY_6 + BILL_AMT1 + BILL_AMT3 + BILL_AMT6 + PAY_AMT1 + PAY_AMT3 + PAY_AMT6, credit, family="binomial"))

# Calculate marginal effects for the logistic regression model
logitmfx(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_3 + PAY_6 + BILL_AMT1 + BILL_AMT3 + BILL_AMT6 + PAY_AMT1 + PAY_AMT3 + PAY_AMT6, credit)

# Normalize selected numeric variables in the data
credit[, c("LIMIT_BAL", "EDUCATION", "MARRIAGE", "AGE", "PAY_0", "PAY_3", "PAY_6", "BILL_AMT1", "BILL_AMT3", "BILL_AMT6", "PAY_AMT1", "PAY_AMT3", "PAY_AMT6")] <- lapply(credit[, c("LIMIT_BAL", "EDUCATION", "MARRIAGE", "AGE", "PAY_0", "PAY_3", "PAY_6", "BILL_AMT1", "BILL_AMT3", "BILL_AMT6", "PAY_AMT1", "PAY_AMT3", "PAY_AMT6")], as.numeric)

# Compute the correlation matrix for selected numeric variables
cor_matrix <- cor(credit[, c("LIMIT_BAL", "EDUCATION", "MARRIAGE", "AGE", "PAY_0", "PAY_3", "PAY_6", "BILL_AMT1", "BILL_AMT3", "BILL_AMT6", "PAY_AMT1", "PAY_AMT3", "PAY_AMT6")])
print(cor_matrix)

# Fit a logistic regression model after normalization (final logistic regression) 
# AIC: 29490  Number of Fisher Scoring iterations: 6 <Better model>
summary(glm(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, credit, family="binomial"))

#Probit
#AIC: 29585
#Number of Fisher Scoring iterations: 8
probit_model <- glm(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, credit, family = binomial(link = "probit"))
summary(probit_model)

# Calculate marginal effects for the logistic regression model after normalization
logitmfx(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, credit)
#-------------
 
 

Prediction Models:
Predictors used: LIMIT_BAL, EDUCATION, MARRIAGE,  AGE, PAY_6, BILL_AMT1, PAY_AMT1 + PAY_AMT2, PAY_AMT3
Predictors Avoided: Education has been avoided due to its low significance.

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


KNN:
# Train k-Nearest Neighbors classifier
knn_caret = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
                  data = credit, method = "knn", trControl = train_Control,
                  tuneLength = 20)
plot(knn_caret)

y_pred <- predict(knn_caret, newdata = credit)

# Compute confusion matrix for KN
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)
Naïve Bayes:
# Train Naive Bayes classifier Tune Length 10
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

#SVM
#Train Support Vector Machine with linear kernel
svm_linear = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
                   data = credit, method = "svmLinear", trControl = train_Control,
                   tuneLength = 10)
svm_linear

# Make predictions using Support Vector Machine with linear kernel
y_pred <- predict(svm_linear, newdata = credit)

# Compute confusion matrix for Support Vector Machine with linear kernel
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)

#Random Forest
# Train Random Forest classifier
rf = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
           data = credit, method = "rf", trControl = train_Control,
           tuneLength = 20)
rf
plot(rf)

# Make predictions using RF
y_pred <- predict(rf, newdata = credit)

# Compute confusion matrix for RF
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix)





#Neural Net
# Train Neural Network classifier
nnet = train(payment ~ LIMIT_BAL + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
             data = credit, method = "nnet", trControl = train_Control,
             tuneLength = 2)
nnet
plot(nnet)
# Make predictions using RF
y_pred <- predict(nnet, newdata = credit)

# Compute confusion matrix for RF
conf_matrix <- confusionMatrix(data = y_pred, reference = credit$payment)
print(conf_matrix) 


OUTPUT:

#Logit before checking correlation
Call:
glm(formula = payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + 
    PAY_0 + PAY_3 + PAY_6 + BILL_AMT1 + BILL_AMT3 + BILL_AMT6 + 
    PAY_AMT1 + PAY_AMT3 + PAY_AMT6, family = "binomial", data = credit)

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.349e+00  1.074e-01 -12.566  < 2e-16 ***
LIMIT_BAL   -2.065e-06  1.734e-07 -11.906  < 2e-16 ***
EDUCATION2   1.775e-02  3.720e-02   0.477 0.633314    
EDUCATION3  -3.167e-02  4.997e-02  -0.634 0.526201    
EDUCATION4  -1.126e+00  3.993e-01  -2.821 0.004787 ** 
MARRIAGE2   -1.386e-01  3.629e-02  -3.820 0.000133 ***
MARRIAGE3    5.778e-02  1.449e-01   0.399 0.690079    
AGE          4.938e-03  1.953e-03   2.528 0.011461 *  
PAY_0-1      3.867e-01  8.676e-02   4.457 8.31e-06 ***
PAY_00      -2.545e-01  8.749e-02  -2.909 0.003625 ** 
PAY_01       8.217e-01  7.970e-02  10.309  < 2e-16 ***
PAY_02       2.076e+00  9.295e-02  22.332  < 2e-16 ***
PAY_03       2.100e+00  1.629e-01  12.891  < 2e-16 ***
PAY_04       1.617e+00  2.684e-01   6.023 1.71e-09 ***
PAY_05       1.131e+00  4.499e-01   2.514 0.011939 *  
PAY_06       1.506e+00  6.813e-01   2.210 0.027101 *  
PAY_07       2.878e+00  1.026e+00   2.805 0.005037 ** 
PAY_08       4.018e-01  1.288e+00   0.312 0.755115    
PAY_3-1     -1.625e-01  7.592e-02  -2.141 0.032265 *  
PAY_30       8.407e-02  8.076e-02   1.041 0.297892    
PAY_31       2.528e-01  1.188e+00   0.213 0.831415    
PAY_32       5.838e-01  8.202e-02   7.118 1.10e-12 ***
PAY_33       5.633e-01  1.719e-01   3.277 0.001048 ** 
PAY_34       1.566e-01  2.986e-01   0.525 0.599909    
PAY_35      -6.429e-01  6.368e-01  -1.010 0.312691    
PAY_36       5.451e-01  1.205e+00   0.452 0.651042    
PAY_37       3.842e-01  6.621e-01   0.580 0.561731    
PAY_38      -6.334e-01  1.500e+00  -0.422 0.672921    
PAY_6-1     -2.358e-01  6.375e-02  -3.699 0.000216 ***
PAY_60      -3.461e-01  6.382e-02  -5.424 5.84e-08 ***
PAY_62       2.434e-01  7.664e-02   3.175 0.001496 ** 
PAY_63       6.066e-01  2.010e-01   3.017 0.002552 ** 
PAY_64      -7.444e-02  3.450e-01  -0.216 0.829153    
PAY_65       2.013e-01  6.308e-01   0.319 0.749635    
PAY_66       1.148e+00  6.419e-01   1.788 0.073697 .  
PAY_67       3.535e-01  5.091e-01   0.694 0.487516    
PAY_68       1.072e+01  8.340e+01   0.129 0.897714    
BILL_AMT1    1.140e-06  5.797e-07   1.966 0.049341 *  
BILL_AMT3    1.178e-06  6.869e-07   1.714 0.086479 .  
BILL_AMT6   -3.603e-07  6.171e-07  -0.584 0.559345    
PAY_AMT1    -1.428e-05  2.258e-06  -6.325 2.53e-10 ***
PAY_AMT3    -3.133e-06  1.523e-06  -2.057 0.039647 *  
PAY_AMT6    -3.503e-06  1.328e-06  -2.637 0.008354 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 31427  on 29600  degrees of freedom
Residual deviance: 25957  on 29558  degrees of freedom
AIC: 26043

Number of Fisher Scoring iterations: 9

> 
> # Calculate marginal effects for the logistic regression model
> logitmfx(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_3 + PAY_6 + BILL_AMT1 + BILL_AMT3 + BILL_AMT6 + PAY_AMT1 + PAY_AMT3 + PAY_AMT6, credit)
Call:
logitmfx(formula = payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + 
    AGE + PAY_0 + PAY_3 + PAY_6 + BILL_AMT1 + BILL_AMT3 + BILL_AMT6 + 
    PAY_AMT1 + PAY_AMT3 + PAY_AMT6, data = credit)

Marginal Effects:
                 dF/dx   Std. Err.        z     P>|z|    
LIMIT_BAL  -3.1613e-07  2.6362e-08 -11.9920 < 2.2e-16 ***
EDUCATION2  2.7180e-03  5.6992e-03   0.4769 0.6334246    
EDUCATION3 -4.8175e-03  7.5500e-03  -0.6381 0.5234185    
EDUCATION4 -1.1901e-01  2.6253e-02  -4.5334 5.805e-06 ***
MARRIAGE2  -2.1289e-02  5.5870e-03  -3.8104 0.0001387 ***
MARRIAGE3   9.0035e-03  2.2972e-02   0.3919 0.6951015    
AGE         7.5605e-04  2.9901e-04   2.5286 0.0114533 *  
PAY_0-1     6.3659e-02  1.5264e-02   4.1706 3.038e-05 ***
PAY_00     -3.8918e-02  1.3358e-02  -2.9135 0.0035740 ** 
PAY_01      1.4973e-01  1.6667e-02   8.9834 < 2.2e-16 ***
PAY_02      4.4443e-01  2.1326e-02  20.8403 < 2.2e-16 ***
PAY_03      4.6469e-01  3.6938e-02  12.5803 < 2.2e-16 ***
PAY_04      3.5046e-01  6.6652e-02   5.2581 1.456e-07 ***
PAY_05      2.3010e-01  1.0946e-01   2.1021 0.0355487 *  
PAY_06      3.2307e-01  1.7020e-01   1.8982 0.0576741 .  
PAY_07      6.1650e-01  1.6099e-01   3.8296 0.0001284 ***
PAY_08      6.9244e-02  2.4654e-01   0.2809 0.7788158    
PAY_3-1    -2.4134e-02  1.0921e-02  -2.2099 0.0271090 *  
PAY_30      1.2857e-02  1.2336e-02   1.0422 0.2972958    
PAY_31      4.1775e-02  2.1064e-01   0.1983 0.8427895    
PAY_32      1.0149e-01  1.5929e-02   6.3713 1.874e-10 ***
PAY_33      1.0111e-01  3.5264e-02   2.8673 0.0041405 ** 
PAY_34      2.5147e-02  5.0177e-02   0.5012 0.6162513    
PAY_35     -7.9781e-02  6.1900e-02  -1.2889 0.1974431    
PAY_36      9.7606e-02  2.4620e-01   0.3964 0.6917779    
PAY_37      6.5888e-02  1.2563e-01   0.5245 0.5999485    
PAY_38     -7.8831e-02  1.4682e-01  -0.5369 0.5913170    
PAY_6-1    -3.4491e-02  8.8929e-03  -3.8785 0.0001051 ***
PAY_60     -5.3506e-02  9.9510e-03  -5.3769 7.578e-08 ***
PAY_62      3.9578e-02  1.3207e-02   2.9968 0.0027286 ** 
PAY_63      1.1014e-01  4.2038e-02   2.6199 0.0087958 ** 
PAY_64     -1.1136e-02  5.0393e-02  -0.2210 0.8251123    
PAY_65      3.2763e-02  1.0876e-01   0.3012 0.7632387    
PAY_66      2.3427e-01  1.5664e-01   1.4956 0.1347615    
PAY_67      6.0091e-02  9.5121e-02   0.6317 0.5275595    
PAY_68      8.1127e-01  8.3293e-03  97.3996 < 2.2e-16 ***
BILL_AMT1   1.7449e-07  8.8744e-08   1.9662 0.0492782 *  
BILL_AMT3   1.8031e-07  1.0515e-07   1.7148 0.0863762 .  
BILL_AMT6  -5.5169e-08  9.4494e-08  -0.5838 0.5593312    
PAY_AMT1   -2.1870e-06  3.4275e-07  -6.3809 1.761e-10 ***
PAY_AMT3   -4.7973e-07  2.3302e-07  -2.0588 0.0395160 *  
PAY_AMT6   -5.3636e-07  2.0319e-07  -2.6397 0.0082969 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

dF/dx is for discrete change for the following variables:

 [1] "EDUCATION2" "EDUCATION3" "EDUCATION4" "MARRIAGE2"  "MARRIAGE3"  "PAY_0-1"    "PAY_00"     "PAY_01"    
 [9] "PAY_02"     "PAY_03"     "PAY_04"     "PAY_05"     "PAY_06"     "PAY_07"     "PAY_08"     "PAY_3-1"   
[17] "PAY_30"     "PAY_31"     "PAY_32"     "PAY_33"     "PAY_34"     "PAY_35"     "PAY_36"     "PAY_37"    
[25] "PAY_38"     "PAY_6-1"    "PAY_60"     "PAY_62"     "PAY_63"     "PAY_64"     "PAY_65"     "PAY_66"    
[33] "PAY_67"     "PAY_68"    
#CorrelationMatrix:
LIMIT_BAL   EDUCATION     MARRIAGE         AGE       PAY_0        PAY_3       PAY_6   BILL_AMT1   BILL_AMT3
LIMIT_BAL  1.0000000 -0.24403881 -0.109756464  0.14421445 -0.27234213 -0.287092909 -0.23160209  0.28369503  0.28173828
EDUCATION -0.2440388  1.00000000 -0.147976601  0.18742546  0.12363449  0.136362328  0.11430473  0.00511991 -0.00349911
MARRIAGE  -0.1097565 -0.14797660  1.000000000 -0.41828350  0.01789002  0.032136130  0.04111106 -0.02497083 -0.02617118
AGE        0.1442145  0.18742546 -0.418283503  1.00000000 -0.03936825 -0.052866538 -0.05286839  0.05470419  0.05183887
PAY_0     -0.2723421  0.12363449  0.017890022 -0.03936825  1.00000000  0.574095530  0.46998996  0.18688939  0.17944160
PAY_3     -0.2870929  0.13636233  0.032136130 -0.05286654  0.57409553  1.000000000  0.63730086  0.20854324  0.22752912
PAY_6     -0.2316021  0.11430473  0.041111061 -0.05286839  0.46998996  0.637300857  1.00000000  0.25472871  0.28814854
BILL_AMT1  0.2836950  0.00511991 -0.024970830  0.05470419  0.18688939  0.208543238  0.25472871  1.00000000  0.89188640
BILL_AMT3  0.2817383 -0.00349911 -0.026171180  0.05183887  0.17944160  0.227529116  0.28814854  0.89188640  1.00000000
BILL_AMT6  0.2897006 -0.01519047 -0.022198227  0.04671250  0.17677914  0.222007750  0.32468058  0.80483404  0.85557069
PAY_AMT1   0.1956660 -0.04597195 -0.005120012  0.02544026 -0.08013912  0.001454054  0.01427887  0.14048941  0.24456415
PAY_AMT3   0.2105229 -0.05196621 -0.002921295  0.02947816 -0.07090592 -0.053357406  0.01900635  0.15681277  0.13111196
PAY_AMT6   0.2196764 -0.05366980 -0.006024437  0.01913883 -0.05994336 -0.036647403 -0.01742108  0.17579819  0.17953887
            BILL_AMT6     PAY_AMT1     PAY_AMT3     PAY_AMT6
LIMIT_BAL  0.28970062  0.195666036  0.210522872  0.219676382
EDUCATION -0.01519047 -0.045971951 -0.051966213 -0.053669800
MARRIAGE  -0.02219823 -0.005120012 -0.002921295 -0.006024437
AGE        0.04671250  0.025440258  0.029478165  0.019138833
PAY_0      0.17677914 -0.080139116 -0.070905919 -0.059943359
PAY_3      0.22200775  0.001454054 -0.053357406 -0.036647403
PAY_6      0.32468058  0.014278866  0.019006350 -0.017421084
BILL_AMT1  0.80483404  0.140489414  0.156812770  0.175798186
BILL_AMT3  0.85557069  0.244564146  0.131111963  0.179538873
BILL_AMT6  1.00000000  0.202036242  0.234607759  0.115524568
PAY_AMT1   0.20203624  1.000000000  0.253683037  0.186283089
PAY_AMT3   0.23460776  0.253683037  1.000000000  0.160547507
PAY_AMT6   0.11552457  0.186283089  0.160547507  1.000000000


#Probit
> summary(probit_model)

Call:
glm(formula = payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + 
    PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, family = binomial(link = "probit"), 
    data = credit)

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -8.047e-01  8.178e-02  -9.840  < 2e-16 ***
LIMIT_BAL   -1.291e-06  8.306e-08 -15.545  < 2e-16 ***
EDUCATION   -1.812e-02  1.256e-02  -1.442    0.149    
MARRIAGE    -9.801e-02  1.767e-02  -5.548 2.88e-08 ***
AGE          4.058e-03  9.903e-04   4.097 4.18e-05 ***
PAY_6        1.771e-01  9.584e-03  18.478  < 2e-16 ***
BILL_AMT1    2.233e-07  1.374e-07   1.625    0.104    
PAY_AMT1    -8.967e-06  1.135e-06  -7.900 2.79e-15 ***
PAY_AMT2    -5.774e-06  9.393e-07  -6.147 7.90e-10 ***
PAY_AMT3    -3.255e-06  8.129e-07  -4.004 6.24e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 31427  on 29600  degrees of freedom
Residual deviance: 29988  on 29591  degrees of freedom
AIC: 30008

Number of Fisher Scoring iterations: 8

#Correlation Matrix:
 IMIT_BAL   EDUCATION     MARRIAGE         AGE       PAY_0        PAY_3       PAY_6   BILL_AMT1   BILL_AMT3
LIMIT_BAL  1.0000000 -0.24403881 -0.109756464  0.14421445 -0.27234213 -0.287092909 -0.23160209  0.28369503  0.28173828
EDUCATION -0.2440388  1.00000000 -0.147976601  0.18742546  0.12363449  0.136362328  0.11430473  0.00511991 -0.00349911
MARRIAGE  -0.1097565 -0.14797660  1.000000000 -0.41828350  0.01789002  0.032136130  0.04111106 -0.02497083 -0.02617118
AGE        0.1442145  0.18742546 -0.418283503  1.00000000 -0.03936825 -0.052866538 -0.05286839  0.05470419  0.05183887
PAY_0     -0.2723421  0.12363449  0.017890022 -0.03936825  1.00000000  0.574095530  0.46998996  0.18688939  0.17944160
PAY_3     -0.2870929  0.13636233  0.032136130 -0.05286654  0.57409553  1.000000000  0.63730086  0.20854324  0.22752912
PAY_6     -0.2316021  0.11430473  0.041111061 -0.05286839  0.46998996  0.637300857  1.00000000  0.25472871  0.28814854
BILL_AMT1  0.2836950  0.00511991 -0.024970830  0.05470419  0.18688939  0.208543238  0.25472871  1.00000000  0.89188640
BILL_AMT3  0.2817383 -0.00349911 -0.026171180  0.05183887  0.17944160  0.227529116  0.28814854  0.89188640  1.00000000
BILL_AMT6  0.2897006 -0.01519047 -0.022198227  0.04671250  0.17677914  0.222007750  0.32468058  0.80483404  0.85557069
PAY_AMT1   0.1956660 -0.04597195 -0.005120012  0.02544026 -0.08013912  0.001454054  0.01427887  0.14048941  0.24456415
PAY_AMT3   0.2105229 -0.05196621 -0.002921295  0.02947816 -0.07090592 -0.053357406  0.01900635  0.15681277  0.13111196
PAY_AMT6   0.2196764 -0.05366980 -0.006024437  0.01913883 -0.05994336 -0.036647403 -0.01742108  0.17579819  0.17953887
            BILL_AMT6     PAY_AMT1     PAY_AMT3     PAY_AMT6
LIMIT_BAL  0.28970062  0.195666036  0.210522872  0.219676382
EDUCATION -0.01519047 -0.045971951 -0.051966213 -0.053669800
MARRIAGE  -0.02219823 -0.005120012 -0.002921295 -0.006024437
AGE        0.04671250  0.025440258  0.029478165  0.019138833
PAY_0      0.17677914 -0.080139116 -0.070905919 -0.059943359
PAY_3      0.22200775  0.001454054 -0.053357406 -0.036647403
PAY_6      0.32468058  0.014278866  0.019006350 -0.017421084
BILL_AMT1  0.80483404  0.140489414  0.156812770  0.175798186
BILL_AMT3  0.85557069  0.244564146  0.131111963  0.179538873
BILL_AMT6  1.00000000  0.202036242  0.234607759  0.115524568
PAY_AMT1   0.20203624  1.000000000  0.253683037  0.186283089
PAY_AMT3   0.23460776  0.253683037  1.000000000  0.160547507
PAY_AMT6   0.11552457  0.186283089  0.160547507  1.000000000

#GLM2 after checking correlated variables and discarding them

Call:
glm(formula = payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + 
    PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, family = "binomial", 
    data = credit)

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.292e+00  1.415e-01  -9.131  < 2e-16 ***
LIMIT_BAL   -2.218e-06  1.506e-07 -14.726  < 2e-16 ***
EDUCATION   -3.858e-02  2.184e-02  -1.766 0.077358 .  
MARRIAGE    -1.724e-01  3.050e-02  -5.652 1.59e-08 ***
AGE          6.536e-03  1.692e-03   3.864 0.000112 ***
PAY_6        3.207e-01  1.669e-02  19.209  < 2e-16 ***
BILL_AMT1    6.610e-07  2.526e-07   2.617 0.008878 ** 
PAY_AMT1    -2.227e-05  2.620e-06  -8.497  < 2e-16 ***
PAY_AMT2    -1.635e-05  2.259e-06  -7.239 4.50e-13 ***
PAY_AMT3    -7.506e-06  1.710e-06  -4.391 1.13e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 31427  on 29600  degrees of freedom
Residual deviance: 29892  on 29591  degrees of freedom
AIC: 29912

Number of Fisher Scoring iterations: 6

Warning message:
glm.fit: fitted probabilities numerically 0 or 1 occurred 
> logitmfx(payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, credit)
Call:
logitmfx(formula = payment ~ LIMIT_BAL + EDUCATION + MARRIAGE + 
    AGE + PAY_6 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3, 
    data = credit)

Marginal Effects:
                dF/dx   Std. Err.        z     P>|z|    
LIMIT_BAL -3.5795e-07  2.4168e-08 -14.8111 < 2.2e-16 ***
EDUCATION -6.2265e-03  3.5245e-03  -1.7667 0.0772839 .  
MARRIAGE  -2.7817e-02  4.9186e-03  -5.6555 1.554e-08 ***
AGE        1.0549e-03  2.7301e-04   3.8638 0.0001116 ***
PAY_6      5.1752e-02  2.6856e-03  19.2700 < 2.2e-16 ***
BILL_AMT1  1.0668e-07  4.0648e-08   2.6245 0.0086774 ** 
PAY_AMT1  -3.5935e-06  4.1600e-07  -8.6383 < 2.2e-16 ***
PAY_AMT2  -2.6394e-06  3.5971e-07  -7.3375 2.176e-13 ***
PAY_AMT3  -1.2114e-06  2.7524e-07  -4.4014 1.075e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 

#Models

#KNN
k-Nearest Neighbours 

29601 samples
    8 predictor
    2 classes: '0', '1' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 26641, 26641, 26641, 26642, 26641, 26641, ... 
Resampling results across tuning parameters:

  k   Accuracy   Kappa     
   5  0.7510559  0.11184765
   7  0.7593663  0.10142374
   9  0.7645352  0.09231741
  11  0.7679808  0.08567867
  13  0.7702441  0.08204652
  15  0.7712914  0.07664889
  17  0.7727440  0.07249406
  19  0.7743656  0.07160246
  21  0.7743317  0.06476297
  23  0.7753789  0.06265177
  25  0.7760209  0.06039217
  27  0.7758183  0.05615043
  29  0.7764601  0.05415961
  31  0.7768317  0.05340467
  33  0.7770683  0.05042600
  35  0.7772036  0.04857493
  37  0.7766631  0.04398479
  39  0.7773048  0.04435048
  41  0.7776088  0.04449007
  43  0.7767981  0.03929183

Accuracy was used to select the optimal model using the
 largest value.
The final value used for the model was k = 41.
Accuracy was used to select the optimal model using the largest value  = 0.7776
#KNN Confusion Matrix
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 22794  6310
         1   202   295
                                          
               Accuracy : 0.78            
                 95% CI : (0.7752, 0.7847)
    No Information Rate : 0.7769          
    P-Value [Acc > NIR] : 0.09815         
                                          
                  Kappa : 0.0535          
                                          
 Mcnemar's Test P-Value : < 2e-16         
                                          
            Sensitivity : 0.99122         
            Specificity : 0.04466         
         Pos Pred Value : 0.78319         
         Neg Pred Value : 0.59356         
             Prevalence : 0.77687         
         Detection Rate : 0.77004         
   Detection Prevalence : 0.98321         
      Balanced Accuracy : 0.51794         
                                          
       'Positive' Class : 0
 
 
#Naive Bayes
============= Naive Bayes ====================================== 
 
- Call: naive_bayes.default(x = x, y = y, laplace = param$laplace, usekernel = TRUE,      adjust = param$adjust) 
- Laplace: 0 
- Classes: 2 
- Samples: 29601 
- Features: 8 
- Conditional distributions: 
    - KDE: 8
- Prior probabilities: 
    - 0: 0.7769
    - 1: 0.2231
# NB Confusion Matrix:
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 22885  6452
         1   111   153
                                         
               Accuracy : 0.7783         
                 95% CI : (0.7735, 0.783)
    No Information Rate : 0.7769         
    P-Value [Acc > NIR] : 0.2815         
                                         
                  Kappa : 0.0279         
                                         
 Mcnemar's Test P-Value : <2e-16         
                                         
            Sensitivity : 0.99517        
            Specificity : 0.02316        
         Pos Pred Value : 0.78007        
         Neg Pred Value : 0.57955        
             Prevalence : 0.77687        
         Detection Rate : 0.77312        
   Detection Prevalence : 0.99108        
      Balanced Accuracy : 0.50917        
                                         
       'Positive' Class : 0      
  


#Naive Bayes (2) with  tune length=30


======================== Naive Bayes ================================== 
 
- Call: naive_bayes.default(x = x, y = y, laplace = param$laplace, usekernel = TRUE,      adjust = param$adjust) 
- Laplace: 0 
- Classes: 2 
- Samples: 29601 
- Features: 8 
- Conditional distributions: 
    - KDE: 8
- Prior probabilities: 
    - 0: 0.7769
    - 1: 0.2231
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 22885  6452
         1   111   153
                                         
               Accuracy : 0.7783          
                 95% CI : (0.7735, 0.783)
    No Information Rate : 0.7769         
    P-Value [Acc > NIR] : 0.2815         
                                         
                  Kappa : 0.0279         
                                         
 Mcnemar's Test P-Value : <2e-16         
                                         
            Sensitivity : 0.99517        
            Specificity : 0.02316        
         Pos Pred Value : 0.78007        
         Neg Pred Value : 0.57955        
             Prevalence : 0.77687        
         Detection Rate : 0.77312        
   Detection Prevalence : 0.99108        
      Balanced Accuracy : 0.50917        
                                         
       'Positive' Class : 0          
  

#SVM - linear

Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 22996  6605
         1     0     0
                                          
               Accuracy : 0.7769          
                 95% CI : (0.7721, 0.7816)
    No Information Rate : 0.7769          
    P-Value [Acc > NIR] : 0.5033          
                                          
                  Kappa : 0               
                                          
 Mcnemar's Test P-Value : <2e-16          
                                          
            Sensitivity : 1.0000          
            Specificity : 0.0000          
         Pos Pred Value : 0.7769          
         Neg Pred Value :    NaN          
             Prevalence : 0.7769          
         Detection Rate : 0.7769          
   Detection Prevalence : 1.0000          
      Balanced Accuracy : 0.5000          
                                          
       'Positive' Class : 0    
#Random Forest
Random Forest 

29601 samples
    8 predictor
    2 classes: '0', '1' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 26641, 26641, 26641, 26640, 26642, 26642, ... 
Resampling results across tuning parameters:

  mtry  Accuracy   Kappa    
  2     0.7885888  0.2152694
  3     0.7864267  0.2209792
  5     0.7836227  0.2265342
  6     0.7824405  0.2248564
  8     0.7811228  0.2255029

Accuracy was used to select the optimal model using the largest value.
The final value used for the model was mtry = 2.

Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 22978   426
         1    18  6179
                                          
               Accuracy : 0.985           
                 95% CI : (0.9836, 0.9864)
    No Information Rate : 0.7769          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9558          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.9992          
            Specificity : 0.9355          
         Pos Pred Value : 0.9818          
         Neg Pred Value : 0.9971          
             Prevalence : 0.7769          
         Detection Rate : 0.7763          
   Detection Prevalence : 0.7906          
      Balanced Accuracy : 0.9674          
                                          
       'Positive' Class : 0    
  


#RandomForest with  6 Predictors  
Random Forest 

30000 samples
    6 predictor
    2 classes: '0', '1' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 27001, 27000, 27000, 26999, 26999, 27001, ... 
Resampling results across tuning parameters:

  mtry  Accuracy   Kappa    
  2     0.7844001  0.1689593
  3     0.7705669  0.1612579
  4     0.7612002  0.1609022
  5     0.7567671  0.1552205
  6     0.7548337  0.1503233

Accuracy was used to select the optimal model using the largest value. = 0.784

 
The final value used for the model was mtry = 2.
 

#RF(2) with tuning=20 
 
30000 samples
    6 predictor
    2 classes: '0', '1' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 27000, 26999, 26999, 27001, 27000, 27001, ... 
Resampling results across tuning parameters:

  mtry  Accuracy   Kappa    
  2     0.7838333  0.1663789
  3     0.7690334  0.1573197
  4     0.7603672  0.1567321
  5     0.7567674  0.1568833
  6     0.7552340  0.1526626

Accuracy was used to select the optimal model using the largest value.
The final value used for the model was mtry = 2. Accuracy = 0.784


#Neural Network 

30000 samples
    8 predictor
    2 classes: '0', '1' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 27001, 26999, 27001, 27001, 27000, 27000, ... 
Resampling results across tuning parameters:

# weights:  191
initial  value 14373.054023 
iter  10 value 14023.593765
iter  20 value 14013.083650
iter  30 value 13965.658643
iter  40 value 13956.027870
iter  50 value 13936.900900
iter  60 value 13933.306593
iter  70 value 13903.013382
iter  80 value 13858.184075
iter  90 value 13768.551377
iter 100 value 13719.315263
final  value 13719.315263 
stopped after 100 iterations
# weights:  11
initial  value 20411.293507 
iter  10 value 14097.281741
iter  20 value 13984.202275
iter  30 value 13870.621881
iter  40 value 13863.850339
iter  50 value 13792.551916
iter  60 value 13682.024506
iter  70 value 13652.500823
iter  80 value 13642.301972
final  value 13641.609253 
converged
# weights:  31
initial  value 25057.332258 
iter  10 value 14141.777146
iter  20 value 14140.110534
iter  30 value 14138.726045
iter  40 value 14131.391200
iter  50 value 14113.879652
iter  60 value 14069.472214
iter  70 value 14066.970174
iter  80 value 14048.802160
iter  90 value 14035.505242
iter 100 value 14032.129191
final  value 14032.129191 
stopped after 100 iterations
# weights:  51
initial  value 22885.593803 
iter  10 value 14031.730963
iter  20 value 14028.443898
iter  30 value 14028.140781
iter  40 value 14020.143140
iter  50 value 14018.827484
iter  60 value 14018.749179
iter  70 value 14017.241014
iter  80 value 14016.287222
iter  90 value 14016.106538
iter 100 value 14016.074700
final  value 14016.074700 
stopped after 100 iterations
# weights:  71
initial  value 20070.130193 
iter  10 value 14138.273393
iter  20 value 14138.132960
iter  30 value 14137.878204
iter  30 value 14137.878151
iter  30 value 14137.878151
final  value 14137.878151 
converged
# weights:  91
initial  value 34813.196085 
iter  10 value 14143.940030
iter  20 value 14140.661531
iter  30 value 13910.697445
iter  40 value 13845.144044
iter  50 value 13749.873762
iter  60 value 13729.451445
iter  70 value 13519.321533
iter  80 value 13443.168849
iter  90 value 13418.753459
iter 100 value 13359.706444
final  value 13359.706444 
stopped after 100 iterations
# weights:  111
initial  value 16557.346602 
iter  10 value 14165.087010
iter  20 value 14034.145746
iter  30 value 14032.710376
iter  40 value 14025.021652
iter  50 value 14020.542529
iter  60 value 14015.929627
iter  70 value 14015.146868
iter  80 value 13993.246397
iter  90 value 13979.025431
iter 100 value 13955.453227
final  value 13955.453227 
stopped after 100 iterations
# weights:  131
initial  value 39718.104142 
iter  10 value 14094.298588
iter  20 value 14036.611829
iter  30 value 14024.255137
iter  40 value 13991.636640
iter  50 value 13893.456253
iter  60 value 13773.979800
iter  70 value 13707.800661
iter  80 value 13590.585612
iter  90 value 13533.028364
iter 100 value 13326.610661
final  value 13326.610661 
stopped after 100 iterations
# weights:  151
initial  value 18159.564921 
iter  10 value 14069.401840
iter  20 value 13970.890292
iter  30 value 13950.796054
iter  40 value 13946.623066
iter  50 value 13945.253571
iter  60 value 13931.096043
iter  70 value 13922.842070
iter  80 value 13921.962933
iter  90 value 13915.465675
iter 100 value 13914.941786
final  value 13914.941786 
stopped after 100 iterations
# weights:  171
initial  value 77670.367958 
iter  10 value 14080.516339
iter  20 value 14015.840949
iter  30 value 13997.213582
iter  40 value 13992.518102
iter  50 value 13985.837262
iter  60 value 13985.449610
iter  70 value 13984.822287
iter  80 value 13984.189673
iter  90 value 13983.723566
iter 100 value 13982.115388
final  value 13982.115388 
stopped after 100 iterations
# weights:  191
initial  value 14475.071727 
iter  10 value 14124.517846
iter  20 value 14031.901857
iter  30 value 14015.831834
iter  40 value 14014.793390
iter  50 value 14011.912725
iter  60 value 14010.561698
iter  70 value 14008.153620
iter  80 value 14004.576972
iter  90 value 14003.708234
iter 100 value 14003.021106
final  value 14003.021106 
stopped after 100 iterations
# weights:  11
initial  value 19788.639567 
iter  10 value 14139.918081
final  value 14139.911379 
converged
# weights:  31
initial  value 18982.196332 
iter  10 value 14139.969411
final  value 14139.968254 
converged
# weights:  51
initial  value 19965.137095 
iter  10 value 14163.375151
iter  20 value 14124.652863
iter  30 value 14124.056855
iter  40 value 14103.694376
iter  50 value 14069.337623
iter  60 value 14058.847101
iter  70 value 14042.741611
iter  80 value 14031.770556
iter  90 value 14027.883331
iter 100 value 14024.615629
final  value 14024.615629 
stopped after 100 iterations
# weights:  71
initial  value 17595.781566 
iter  10 value 14105.580330
iter  20 value 14101.056374
iter  30 value 14100.522567
iter  40 value 13945.757694
iter  50 value 13941.391330
iter  60 value 13928.866842
iter  70 value 13917.208208
iter  80 value 13911.104403
iter  90 value 13905.414155
iter 100 value 13904.958024
final  value 13904.958024 
stopped after 100 iterations
# weights:  91
initial  value 29075.104997 
iter  10 value 14139.098391
iter  20 value 14070.731141
iter  30 value 13971.700663
iter  40 value 13959.061652
iter  50 value 13955.413025
iter  60 value 13948.372609
iter  70 value 13945.455410
iter  80 value 13941.853279
iter  90 value 13941.576856
iter 100 value 13941.321162
final  value 13941.321162 
stopped after 100 iterations
# weights:  111
initial  value 22737.860574 
iter  10 value 14053.335242
iter  20 value 14036.873898
iter  30 value 14019.421636
iter  40 value 14015.360398
iter  50 value 14014.050806
iter  60 value 14008.144930
iter  70 value 14004.783852
iter  80 value 14004.293567
iter  90 value 14003.980402
iter 100 value 14003.908217
final  value 14003.908217 
stopped after 100 iterations
# weights:  131
initial  value 21804.810670 
iter  10 value 14129.031951
iter  10 value 14129.031888
iter  20 value 13977.693158
iter  30 value 13966.602164
iter  40 value 13958.209425
iter  50 value 13957.418207
iter  60 value 13954.779317
iter  70 value 13952.927898
final  value 13952.859613 
converged
# weights:  151
initial  value 38096.787825 
iter  10 value 14296.827711
iter  20 value 14160.655424
iter  30 value 14084.337923
iter  40 value 14068.865496
iter  50 value 14050.737530
iter  60 value 14024.060909
iter  70 value 13978.764866
iter  80 value 13944.567766
iter  90 value 13938.838067
iter 100 value 13937.547966
final  value 13937.547966 
stopped after 100 iterations
# weights:  171
initial  value 39419.421646 
iter  10 value 14044.645773
iter  20 value 14029.179595
iter  30 value 13998.879569
iter  40 value 13970.667258
iter  50 value 13951.291735
iter  60 value 13948.265081
iter  70 value 13942.319980
iter  80 value 13929.201429
iter  90 value 13927.121615
iter 100 value 13926.268442
final  value 13926.268442 
stopped after 100 iterations
# weights:  191
initial  value 14746.727694 
iter  10 value 14220.538430
iter  20 value 14076.138668
iter  30 value 14030.495057
iter  40 value 14025.753886
iter  50 value 14020.169382
iter  60 value 14016.086794
iter  70 value 14012.434619
iter  80 value 14011.064888
iter  90 value 14008.775251
iter 100 value 14004.346948
final  value 14004.346948 
stopped after 100 iterations
# weights:  11
initial  value 17539.316231 
final  value 14141.385488 
converged
# weights:  31
initial  value 21573.332129 
iter  10 value 14076.619495
iter  20 value 14032.773556
iter  30 value 14025.174048
iter  40 value 14023.817342
iter  50 value 14022.031379
iter  60 value 14020.942220
final  value 14020.713582 
converged
# weights:  51
initial  value 16923.694487 
iter  10 value 14013.306080
iter  20 value 14010.530213
iter  30 value 13989.430561
iter  40 value 13949.668444
iter  50 value 13930.294465
iter  60 value 13906.369566
iter  70 value 13899.138233
iter  80 value 13896.951349
iter  90 value 13892.309435
iter 100 value 13824.153280
final  value 13824.153280 
stopped after 100 iterations
# weights:  71
initial  value 36205.609532 
iter  10 value 14125.705850
iter  20 value 14074.983944
iter  30 value 14074.011027
iter  40 value 14069.814343
iter  50 value 14069.335213
iter  60 value 14069.070814
iter  70 value 14067.327419
final  value 14067.118909 
converged
# weights:  91
initial  value 14317.621396 
iter  10 value 14070.269413
iter  20 value 14026.327464
iter  30 value 13991.714318
iter  40 value 13956.061395
iter  50 value 13949.446772
iter  60 value 13916.183326
iter  70 value 13910.867923
iter  80 value 13902.558701
iter  90 value 13898.767423
iter 100 value 13885.767601
final  value 13885.767601 
stopped after 100 iterations
# weights:  111
initial  value 15188.774087 
iter  10 value 14132.836842
iter  20 value 14067.019944
iter  30 value 14059.399107
iter  40 value 14053.201500
iter  50 value 14052.759908
iter  60 value 14046.462034
iter  70 value 14032.272763
iter  80 value 14030.874310
iter  90 value 14026.168630
iter 100 value 14020.732437
final  value 14020.732437 
stopped after 100 iterations
# weights:  131
initial  value 41990.572335 
iter  10 value 14139.535764
iter  20 value 14136.592337
iter  30 value 14135.818339
final  value 14135.403182 
converged
# weights:  151
initial  value 19774.868162 
iter  10 value 14021.973984
iter  20 value 14007.792388
iter  30 value 13989.100466
iter  40 value 13980.901546
iter  50 value 13965.652769
iter  60 value 13962.825958
iter  70 value 13959.068624
iter  80 value 13951.202351
iter  90 value 13933.484516
iter 100 value 13930.410727
final  value 13930.410727 
stopped after 100 iterations
# weights:  171
initial  value 31092.434553 
iter  10 value 14509.602494
iter  20 value 14356.331902
iter  30 value 14002.122602
iter  40 value 13964.174704
iter  50 value 13957.095758
iter  60 value 13946.148823
iter  70 value 13914.407032
iter  80 value 13912.316234
iter  90 value 13909.336760
iter 100 value 13908.233057
final  value 13908.233057 
stopped after 100 iterations
# weights:  191
initial  value 19619.780031 
iter  10 value 14091.317934
iter  20 value 14076.892689
iter  30 value 14073.966312
iter  40 value 14061.589227
iter  50 value 14061.282980
iter  60 value 14059.183980
iter  70 value 14058.213426
iter  80 value 14056.083194
iter  90 value 14019.573557
iter 100 value 13940.808683
final  value 13940.808683 
stopped after 100 iterations
# weights:  11
initial  value 16886.307804 
final  value 14141.378758 
converged
# weights:  31
initial  value 22396.577991 
final  value 14138.608062 
converged
# weights:  51
initial  value 19702.354423 
iter  10 value 14049.097512
iter  20 value 13956.765338
iter  30 value 13901.607915
iter  40 value 13862.179280
iter  50 value 13857.342419
iter  60 value 13854.555204
iter  70 value 13853.573168
iter  80 value 13852.288249
iter  90 value 13852.032065
iter 100 value 13851.627495
final  value 13851.627495 
stopped after 100 iterations
# weights:  71
initial  value 15073.963448 
iter  10 value 14137.614709
iter  20 value 14134.498746
iter  30 value 14134.090377
iter  40 value 14134.047202
iter  50 value 14099.541029
iter  60 value 14094.181185
iter  70 value 14016.076201
iter  80 value 13964.853891
iter  90 value 13939.671622
iter 100 value 13938.518672
final  value 13938.518672 
stopped after 100 iterations
# weights:  91
initial  value 16183.502758 
iter  10 value 14095.372121
iter  20 value 14041.332603
iter  30 value 14023.360822
iter  40 value 14019.102998
iter  50 value 14007.750420
iter  60 value 13986.343470
iter  70 value 13955.969182
iter  80 value 13903.986332
iter  90 value 13788.596583
iter 100 value 13622.355206
final  value 13622.355206 
stopped after 100 iterations
# weights:  111
initial  value 15794.695210 
iter  10 value 14057.538757
iter  20 value 14023.566202
iter  30 value 13975.525414
iter  40 value 13953.725834
iter  50 value 13938.817163
iter  60 value 13926.161468
iter  70 value 13913.760052
iter  80 value 13907.493116
iter  90 value 13900.516533
iter 100 value 13900.371771
final  value 13900.371771 
stopped after 100 iterations
# weights:  131
initial  value 14803.214310 
iter  10 value 14035.962082
iter  20 value 14030.419820
iter  30 value 14003.240376
iter  40 value 13995.111953
iter  50 value 13992.638699
iter  60 value 13992.425074
iter  70 value 13959.379037
iter  80 value 13943.829371
iter  90 value 13936.056200
iter 100 value 13935.357302
final  value 13935.357302 
stopped after 100 iterations
# weights:  151
initial  value 35329.584754 
iter  10 value 14065.397782
iter  20 value 14007.357297
iter  30 value 13996.107440
iter  40 value 13994.896284
iter  50 value 13993.599289
iter  60 value 13993.049076
iter  70 value 13992.885537
iter  80 value 13989.789792
iter  90 value 13989.155814
iter 100 value 13988.974257
final  value 13988.974257 
stopped after 100 iterations
# weights:  171
initial  value 14434.220321 
iter  10 value 14118.336151
iter  20 value 14023.598289
iter  30 value 14004.235407
iter  40 value 13999.554579
iter  50 value 13968.920519
iter  60 value 13955.433183
iter  70 value 13945.340642
iter  80 value 13943.005129
iter  90 value 13936.433812
iter 100 value 13934.818119
final  value 13934.818119 
stopped after 100 iterations
# weights:  191
initial  value 17502.475613 
iter  10 value 14126.480504
iter  20 value 14090.208333
iter  30 value 14068.359141
iter  40 value 14063.063728
iter  50 value 14057.274566
iter  60 value 13966.756701
iter  70 value 13951.491382
iter  80 value 13939.028040
iter  90 value 13925.403377
iter 100 value 13917.361767
final  value 13917.361767 
stopped after 100 iterations
# weights:  11
initial  value 15776.899879 
final  value 14141.399128 
converged
# weights:  31
initial  value 18514.338806 
final  value 14141.396149 
converged
# weights:  51
initial  value 16549.685019 
iter  10 value 14050.340128
iter  20 value 14034.104671
iter  30 value 14029.486368
iter  40 value 14027.799890
final  value 14027.799367 
converged
# weights:  71
initial  value 14161.591892 
iter  10 value 14087.421154
iter  20 value 14066.580592
iter  30 value 13971.205446
iter  40 value 13952.070500
iter  50 value 13946.152184
iter  60 value 13941.549654
iter  70 value 13937.781215
final  value 13937.610995 
converged
# weights:  91
initial  value 25213.470968 
iter  10 value 14057.604379
iter  20 value 14021.824530
iter  30 value 14013.041182
iter  40 value 14012.323167
iter  50 value 14009.856857
iter  60 value 14009.336718
iter  70 value 14000.319628
iter  80 value 13975.309880
iter  90 value 13947.790631
iter 100 value 13928.440319
final  value 13928.440319 
stopped after 100 iterations
# weights:  111
initial  value 39238.478630 
iter  10 value 14316.396752
iter  20 value 14065.968331
iter  30 value 14021.807232
iter  40 value 14017.727576
iter  50 value 14016.383741
iter  60 value 14014.978632
iter  70 value 14014.447942
iter  80 value 14014.402575
iter  90 value 14008.354366
iter 100 value 14002.179004
final  value 14002.179004 
stopped after 100 iterations
# weights:  131
initial  value 14541.350315 
iter  10 value 14112.961602
iter  20 value 14032.989159
iter  30 value 14022.842957
iter  40 value 14008.375984
iter  50 value 14003.392278
iter  60 value 14000.577136
iter  70 value 14000.008924
iter  80 value 13999.206047
iter  90 value 13998.912113
iter 100 value 13995.291866
final  value 13995.291866 
stopped after 100 iterations
# weights:  151
initial  value 17013.989824 
iter  10 value 14041.090066
iter  20 value 14015.194010
iter  30 value 14011.193666
iter  40 value 14003.235763
iter  50 value 13999.542391
iter  60 value 13998.190303
iter  70 value 13965.274398
iter  80 value 13956.112800
iter  90 value 13952.757706
iter 100 value 13951.915897
final  value 13951.915897 
stopped after 100 iterations
# weights:  171
initial  value 36725.117302 
iter  10 value 14070.594741
iter  20 value 13996.584739
iter  30 value 13981.463860
iter  40 value 13971.472134
iter  50 value 13957.009395
iter  60 value 13943.321193
iter  70 value 13920.240798
iter  80 value 13909.851691
iter  90 value 13898.767419
iter 100 value 13893.121414
final  value 13893.121414 
stopped after 100 iterations
# weights:  191
initial  value 37355.798471 
iter  10 value 14027.966041
iter  20 value 14007.605653
iter  30 value 14006.789197
iter  40 value 14003.372555
iter  50 value 13999.039138
iter  60 value 13997.477715
iter  70 value 13997.246694
final  value 13997.160238 
converged
# weights:  11
initial  value 18978.994372 
final  value 14141.385993 
converged
# weights:  31
initial  value 20000.103589 
final  value 14141.434528 
converged
# weights:  51
initial  value 15680.799055 
iter  10 value 14073.413621
iter  20 value 14049.230520
iter  30 value 14030.302501
iter  40 value 14026.802449
iter  50 value 14014.771305
iter  60 value 14005.771395
iter  70 value 14004.788044
iter  80 value 14004.033756
iter  90 value 13998.977123
iter 100 value 13997.911540
final  value 13997.911540 
stopped after 100 iterations
# weights:  71
initial  value 18209.214106 
iter  10 value 14024.377346
iter  20 value 13995.257953
iter  30 value 13934.243834
iter  40 value 13925.893076
iter  50 value 13919.885840
iter  60 value 13915.373003
iter  70 value 13909.150146
iter  80 value 13908.301239
iter  90 value 13906.336090
iter 100 value 13905.612446
final  value 13905.612446 
stopped after 100 iterations
# weights:  91
initial  value 15237.757197 
iter  10 value 14043.503060
iter  20 value 14017.504511
iter  30 value 13985.625428
iter  40 value 13982.383527
iter  50 value 13981.798018
iter  60 value 13976.746588
iter  70 value 13973.049439
iter  80 value 13972.915222
final  value 13972.914278 
converged
# weights:  111
initial  value 16638.779827 
iter  10 value 14049.937016
iter  20 value 14028.220587
iter  30 value 14025.284673
iter  40 value 14019.979367
iter  50 value 14017.842586
iter  60 value 14016.770503
iter  70 value 14016.551696
iter  80 value 14014.681018
final  value 14014.633682 
converged
# weights:  131
initial  value 15556.763292 
iter  10 value 14041.682014
iter  20 value 13970.058990
iter  30 value 13941.996459
iter  40 value 13928.075380
iter  50 value 13925.254630
iter  60 value 13923.617377
iter  70 value 13918.320612
iter  80 value 13875.679751
iter  90 value 13829.018361
iter 100 value 13782.428849
final  value 13782.428849 
stopped after 100 iterations
# weights:  151
initial  value 29331.516487 
iter  10 value 14057.459931
iter  20 value 14042.717986
iter  30 value 14036.122620
iter  40 value 14034.911828
final  value 14034.644220 
converged
# weights:  171
initial  value 15267.584351 
iter  10 value 14139.625229
iter  20 value 14133.961061
iter  30 value 14124.527717
iter  40 value 14112.120161
iter  50 value 14093.871352
iter  60 value 14089.668072
iter  70 value 14069.673656
iter  80 value 14067.507712
iter  90 value 14066.641329
iter 100 value 14066.026838
final  value 14066.026838 
stopped after 100 iterations
# weights:  191
initial  value 30392.650177 
iter  10 value 14045.604141
iter  20 value 13939.441757
iter  30 value 13884.165493
iter  40 value 13867.497849
iter  50 value 13861.930728
iter  60 value 13858.022099
iter  70 value 13855.544310
iter  80 value 13854.342633
iter  90 value 13853.802694
iter 100 value 13850.306197
final  value 13850.306197 
stopped after 100 iterations
# weights:  11
initial  value 23171.914596 
iter  10 value 14140.609519
final  value 14140.608116 
converged
# weights:  31
initial  value 17902.995748 
iter  10 value 14127.296176
iter  20 value 14037.118785
iter  30 value 14028.743934
iter  40 value 14027.816001
iter  50 value 14027.402118
iter  60 value 14005.720763
iter  70 value 13997.431686
iter  80 value 13997.421272
iter  80 value 13997.421141
iter  90 value 13960.728113
iter 100 value 13954.325585
final  value 13954.325585 
stopped after 100 iterations
# weights:  51
initial  value 27562.444518 
iter  10 value 14139.869400
final  value 14139.868983 
converged
# weights:  71
initial  value 20657.106025 
iter  10 value 14135.818896
final  value 14135.815654 
converged
# weights:  91
initial  value 29573.591727 
iter  10 value 14233.908060
iter  20 value 14074.642462
iter  30 value 14039.553137
iter  40 value 14027.928694
iter  50 value 14018.025365
iter  60 value 14017.584895
iter  70 value 14016.466082
final  value 14016.450728 
converged
# weights:  111
initial  value 14608.467877 
iter  10 value 14037.448599
iter  20 value 13950.757687
iter  30 value 13900.251662
iter  40 value 13885.241588
iter  50 value 13880.918813
iter  60 value 13878.747521
iter  70 value 13862.418917
iter  80 value 13851.715915
iter  90 value 13846.795239
iter 100 value 13842.606516
final  value 13842.606516 
stopped after 100 iterations
# weights:  131
initial  value 21908.105334 
iter  10 value 14123.169331
iter  20 value 14098.122137
iter  30 value 14087.977667
iter  40 value 14013.529089
iter  50 value 14010.675052
iter  60 value 14009.863313
iter  70 value 14007.000577
iter  80 value 14005.554301
iter  90 value 14005.430561
final  value 14005.430243 
converged
# weights:  151
initial  value 23266.298303 
iter  10 value 14189.853769
iter  20 value 14054.908908
iter  30 value 14029.184537
iter  40 value 14006.195561
iter  50 value 13978.137929
iter  60 value 13970.913927
iter  70 value 13969.665973
iter  80 value 13967.514397
iter  90 value 13966.109093
iter 100 value 13965.208302
final  value 13965.208302 
stopped after 100 iterations
# weights:  171
initial  value 17404.305280 
iter  10 value 14123.428719
iter  20 value 14029.192307
iter  30 value 14021.145885
iter  40 value 14015.815964
iter  50 value 14013.894828
iter  60 value 14010.715412
iter  70 value 14009.681630
iter  80 value 14009.535564
iter  90 value 14009.521737
final  value 14009.460273 
converged
# weights:  191
initial  value 46015.991459 
iter  10 value 14045.534780
iter  20 value 14023.846097
iter  30 value 14014.686143
iter  40 value 14010.303267
iter  50 value 14008.645767
iter  60 value 14007.445636
iter  70 value 14007.136056
iter  80 value 14005.458301
iter  90 value 14003.858227
iter 100 value 14003.588128
final  value 14003.588128 
stopped after 100 iterations
# weights:  71
initial  value 18335.368294 
iter  10 value 15695.400927
iter  20 value 15598.404297
iter  30 value 15572.201194
iter  40 value 15567.921552
iter  50 value 15479.444867
iter  60 value 15448.042550
iter  70 value 15436.743519
iter  80 value 15432.976325
iter  90 value 15432.003690
iter 100 value 15431.354551
final  value 15431.354551 
stopped after 100 iterations
> nnet
Neural Network 

29601 samples
    8 predictor
    2 classes: '0', '1' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 26640, 26642, 26641, 26641, 26641, 26641, ... 
Resampling results across tuning parameters:

  size  decay         Accuracy   Kappa        
   1    0.0000000000  0.7768657   0.000000e+00
   1    0.0001000000  0.7768657   0.000000e+00
   1    0.0002371374  0.7768657   0.000000e+00
   1    0.0005623413  0.7768657   0.000000e+00
   1    0.0013335214  0.7768657   0.000000e+00
   1    0.0031622777  0.7768657   0.000000e+00
   1    0.0074989421  0.7768657   0.000000e+00
   1    0.0177827941  0.7768657   0.000000e+00
   1    0.0421696503  0.7768657   0.000000e+00
   1    0.1000000000  0.7778792   1.779518e-02
   3    0.0000000000  0.7768657   0.000000e+00
   3    0.0001000000  0.7768657   0.000000e+00
   3    0.0002371374  0.7768657   0.000000e+00
   3    0.0005623413  0.7774400   1.885605e-02
   3    0.0013335214  0.7768657   0.000000e+00
   3    0.0031622777  0.7768657   0.000000e+00
   3    0.0074989421  0.7774062   1.923807e-02
   3    0.0177827941  0.7768657   0.000000e+00
   3    0.0421696503  0.7768657   0.000000e+00
   3    0.1000000000  0.7768319  -6.753372e-05
   5    0.0000000000  0.7768657   0.000000e+00
   5    0.0001000000  0.7768319  -6.748823e-05
   5    0.0002371374  0.7768657   0.000000e+00
   5    0.0005623413  0.7768657   0.000000e+00
   5    0.0013335214  0.7768657   0.000000e+00
   5    0.0031622777  0.7768657   0.000000e+00
   5    0.0074989421  0.7768657   0.000000e+00
   5    0.0177827941  0.7768657   0.000000e+00
   5    0.0421696503  0.7768657   0.000000e+00
   5    0.1000000000  0.7773722   1.071784e-02
   7    0.0000000000  0.7768319  -6.748823e-05
   7    0.0001000000  0.7768319  -6.751105e-05
   7    0.0002371374  0.7767981   1.987400e-04
   7    0.0005623413  0.7768657   0.000000e+00
   7    0.0013335214  0.7768657   1.676138e-04
   7    0.0031622777  0.7768657   0.000000e+00
   7    0.0074989421  0.7779809   1.757478e-02
   7    0.0177827941  0.7767305  -2.698854e-04
   7    0.0421696503  0.7768657   0.000000e+00
   7    0.1000000000  0.7768657   0.000000e+00
   9    0.0000000000  0.7767981  -1.350221e-04
   9    0.0001000000  0.7767305  -2.696602e-04
   9    0.0002371374  0.7768657   0.000000e+00
   9    0.0005623413  0.7768657   0.000000e+00
   9    0.0013335214  0.7768319  -6.748823e-05
   9    0.0031622777  0.7768657   0.000000e+00
   9    0.0074989421  0.7768657   0.000000e+00
   9    0.0177827941  0.7768657   0.000000e+00
   9    0.0421696503  0.7768657   0.000000e+00
   9    0.1000000000  0.7773724   7.017726e-03
  11    0.0000000000  0.7767643  -2.021265e-04
  11    0.0001000000  0.7768994   2.352652e-04
  11    0.0002371374  0.7767643  -2.025330e-04
  11    0.0005623413  0.7768657   0.000000e+00
  11    0.0013335214  0.7768657   0.000000e+00
  11    0.0031622777  0.7768657   0.000000e+00
  11    0.0074989421  0.7767643  -2.025558e-04
  11    0.0177827941  0.7768657   0.000000e+00
  11    0.0421696503  0.7768319  -6.753372e-05
  11    0.1000000000  0.7761224   8.767490e-03
  13    0.0000000000  0.7768657   0.000000e+00
  13    0.0001000000  0.7768319  -6.748823e-05
  13    0.0002371374  0.7768657   0.000000e+00
  13    0.0005623413  0.7767981   3.241078e-05
  13    0.0013335214  0.7768319  -6.751105e-05
  13    0.0031622777  0.7768657   0.000000e+00
  13    0.0074989421  0.7768994   2.352652e-04
  13    0.0177827941  0.7768319   1.002660e-04
  13    0.0421696503  0.7768657   0.000000e+00
  13    0.1000000000  0.7768319   2.671555e-04
  15    0.0000000000  0.7768657   0.000000e+00
  15    0.0001000000  0.7768657   0.000000e+00
  15    0.0002371374  0.7768657   0.000000e+00
  15    0.0005623413  0.7768319  -6.748823e-05
  15    0.0013335214  0.7768319  -6.753372e-05
  15    0.0031622777  0.7768319  -6.751105e-05
  15    0.0074989421  0.7767981   3.679497e-04
  15    0.0177827941  0.7768319  -6.751105e-05
  15    0.0421696503  0.7768994   2.352652e-04
  15    0.1000000000  0.7767981  -1.349086e-04
  17    0.0000000000  0.7768319  -6.753372e-05
  17    0.0001000000  0.7768657   0.000000e+00
  17    0.0002371374  0.7768657   0.000000e+00
  17    0.0005623413  0.7760889  -1.524173e-03
  17    0.0013335214  0.7768657   0.000000e+00
  17    0.0031622777  0.7767981  -1.349092e-04
  17    0.0074989421  0.7768657   1.676138e-04
  17    0.0177827941  0.7768994   8.981872e-04
  17    0.0421696503  0.7768657   0.000000e+00
  17    0.1000000000  0.7768657   0.000000e+00
  19    0.0000000000  0.7766967   1.150995e-03
  19    0.0001000000  0.7768657   0.000000e+00
  19    0.0002371374  0.7767305   6.402557e-05
  19    0.0005623413  0.7768319  -6.751105e-05
  19    0.0013335214  0.7768994   2.352652e-04
  19    0.0031622777  0.7767981  -1.350219e-04
  19    0.0074989421  0.7768656   6.658146e-04
  19    0.0177827941  0.7767643  -2.024424e-04
  19    0.0421696503  0.7768657   0.000000e+00
  19    0.1000000000  0.7768657   0.000000e+00

Accuracy was used to select the optimal model using the largest value.
The final values used for the model were size = 7 and decay = 0.007498942.
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 22996  6605
         1     0     0
                                          
               Accuracy : 0.7769          
                 95% CI : (0.7721, 0.7816)
    No Information Rate : 0.7769          
    P-Value [Acc > NIR] : 0.5033          
                                          
                  Kappa : 0               
                                          
 Mcnemar's Test P-Value : <2e-16          
                                          
            Sensitivity : 1.0000          
            Specificity : 0.0000          
         Pos Pred Value : 0.7769          
         Neg Pred Value :    NaN          
             Prevalence : 0.7769          
         Detection Rate : 0.7769          
   Detection Prevalence : 1.0000          
      Balanced Accuracy : 0.5000          
                                          
       'Positive' Class : 0               
                                   

 


![image](https://github.com/HB9398/Predicting-Credit-Card-Default---Taiwan/assets/48625540/fd4dcb28-f50a-4254-ba62-4beb1ed00513)
