### logistic regression on german credit dataset 

## create dataframe 

setwd("D:/AP/Logistic/Logistic Regression")

credit = read.csv("german_credit.csv")

table(credit$Creditability)
### recreate credibiity as loan rejected / not rejected
credit$loan_reject = ifelse( credit$Creditability == 0, 1, 0  )

###drop the credibiity variable 

credit$Creditability = NULL

### structure of the dataset 

str(credit)

## check few varibales 

table(credit$Telephone)

credit$loan_reject= as.factor( credit$loan_reject)
credit$Foreign_Worker = as.factor(credit$Foreign_Worker)
### Loan amount and laon_reject 

library(ggplot2)

ggplot( credit, aes(loan_reject, Credit_Amount )) + geom_boxplot()

### eplore a factor variable 

ggplot(credit, aes( Foreign_Worker, fill = loan_reject)) + geom_bar()

## Divide this into a train and test set 

set.seed(858)

ids = sample(nrow(credit), nrow(credit)*0.8)

train = credit[ ids,]
test = credit[-ids, ]


### Logistic regression 

credit_model = glm( loan_reject ~ ., data=train, family = "binomial")

### summary of model 

summary(credit_model)

Account_Balance + Duration_of_Credit_in_month + Payment_Status_of_Previous_Credit
Credit_Amount +Instalment_per_cent+Value_Savings_and_or_Stocks

credit_model2 = glm ( loan_reject ~ Account_Balance + Duration_of_Credit_in_month + Payment_Status_of_Previous_Credit+
                      Credit_Amount +Instalment_per_cent+Value_Savings_and_or_Stocks,
                      , data = train,
                      family="binomial"
)

### summary of model2 

summary(credit_model2)

### model scoring on test dataset 

test$pred1 = predict(credit_model, newdata=test, type = "response")

test$pred2 = predict( credit_model2, newdata= test, type="response")


### ROC curve of the model 

library(ROCR)

pred = prediction(test$pred2, test$loan_reject)
perf  = performance(pred,"tpr","fpr")

plot(perf)

### add the ROC graph of credit_model1 on the same plot 
pred1 = prediction(test$pred1 , test$loan_reject)
perf1 = performance(pred1, "tpr","fpr")


plot(perf1, add = TRUE, colorize = TRUE)

### AUC of model1
AUC_1 = performance(pred1, measure = 'auc')@y.values[[1]]
AUC_1
## Auc of model2 
AUC_2 = performance(pred, measure = 'auc')@y.values[[1]]
AUC_2


## Confusion matrix 

test$pred_class1 = ifelse( test$pred1 >= 0.5, 1, 0)

test$pred_class2 = ifelse( test$pred >= 0.5 , 1 , 0)


## confusion matrix of model1 and model2 

table( test$loan_reject, test$pred_class1)
table( test$loan_reject, test$pred_class2)

p = 25/(25+19)
r = 26/(26+32)
2 *p*r/(p+r)

### precison vs Recall curve 

library("DMwR")

PRcurve(test$pred, test$loan_reject)

## model for backward or forward selection 


Model = glm( loan_reject ~ . , data=train , family = "binomial")



## forward selection 

forwardmodel = step( Model, direction = "forward")

backwardmodel = step( Model, direction = "backward")

stepwisemod = step( Model, direction = "both")

####   use the backward model for prediction 

test$pred_bak = predict(backwardmodel, newdata=test, type="response")

test$pred_bak_class = ifelse( test$pred_bak >= 0.5, 1, 0)

### confusion matrix 

table(test$loan_reject, test$pred_bak_class)
