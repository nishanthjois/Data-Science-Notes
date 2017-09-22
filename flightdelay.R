setwd("D:\\Data Science - Course\\Logistic")

library(boot)
library(MASS)


flights <- read.csv("FlightDelays.csv", header=T, sep=",")

head(flights)

table(flights$weather)

table(flights$delay)

## class Imbalance 

prop.table(table(flights$delay ))*100

flights$delay = ifelse( flights$delay == "ontime", 0, 1)

table( flights$delay)

### 

flights$weather <- as.factor(flights$weather)
#flights$delay <- as.factor(flights$delay)

table(flights$weather)

library(ggplot2)

ggplot(flights, aes( delay,fill= weather)) + geom_bar()

## convert the delay variable to binary ( 0 , 1)

flights$delay = ifelse(flights$delay =="ontime", 0, 1)


flights$delay = as.factor(flights$delay)

## diff b/w scheduled and departure time 

flights$diff =flights$schedtime - flights$deptime

names(flights)


summary(flights$diff)

file3 = flights[ , -c(7,12)]

flights$diff = NULL

## Handling schedule time and deptime 
time = "855"

time2 = "910"

nchar(time)

## logic to create delay in mins 
if ( nchar(time2) == 4){
  t_hour = substr(time2, 1,2)
  t_mins = substr(time2, 3,4)
}else {
  t_hour = substr(time2, 1,1)
  t_mins = substr(time2, 2,3)
}



t_mins = as.numeric(t_mins)
t_hour = as.numeric(t_hour)
mins = as.numeric(t_mins)
hour = as.numeric(t_hour)
delay = ( hour-t_hour)*60 + (mins-t_mins)

delay
 

### create new variables from schedtime and deptime 

for( i in 1:nrow(flights)){ 
  
  if ( nchar(flights$schedtime[i]) ==4){
    flights$schedhour[i] = as.numeric(substr(flights$schedtime[i], 1,2))
    flights$schedmins[i] = as.numeric(substr(flights$schedtime[i], 3,4))
  }else {
    flights$schedhour[i] = as.numeric(substr(flights$schedtime[i], 1,1))
    flights$schedmins[i] = as.numeric(substr(flights$schedtime[i], 2,3))
  }
  
}

for( i in 1:nrow(flights)){ 
  
  if ( nchar(flights$deptime[i]) == 4){
    flights$dephour[i] = as.numeric(substr(flights$deptime[i], 1,2))
    flights$depmins[i] = as.numeric(substr(flights$deptime[i], 3,4))
  }else {
    flights$dephour[i] = as.numeric(substr(flights$deptime[i], 1,1))
    flights$depmins[i] = as.numeric(substr(flights$deptime[i], 2,3))
  }
  
}

## convert the delay hour and mins into numeric variable
flights$schedhour = as.numeric(flights$schedhour)
flights$dephour = as.numeric(flights$dephour)

flights$schedmins = as.numeric(flights$schedmins)
flights$depmins = as.numeric(flights$depmins)


## calculate the diff in the scheduele and delay in mins 
flights$hourdiff = (flights$schedhour - flights$dephour)*60

flights$mindif = flights$schedmins - flights$depmins

flights$delaymins = flights$hourdiff + flights$mindif

names(flights)

### Drop all other un necessary variables


flights = flights[,-c(6, 7, 12, 14, 15, 16,17, 18,19)]

### ggplot(file2, aes(delay)) + geom_bar(aes(fill=weather),position = "stack")
 

set.seed(1234)


sub = sample(nrow(flights), nrow(flights) * 0.8)

train = flights[sub,]

test = flights[-sub,]


### Generalised linear models : logistic regression

# -weather -dayweek -diff

model1 = glm(delay ~ . -weather -dayweek - daymonth -dest -origin - deptime,  data=train, family="binomial")

summary(model1)

### Predict probability on test data using model1

test$pred = predict(model1, newdata=test, type="response")


### convert the robability to a class variable ( > 0.5 is delayed , <=0.5 is "ontime" )

test$pred_class = ifelse(test$pred >= 0.5, 1,0)



 
## cross table(confusion matrix) of two discrete variables

table(test$delay, test$pred_class)

(358+44)/441

p = 52/(52+6)

recall = 52/(52+31)



library(caret)

confusionMatrix(test$delay, test$pred_class)



## roc graph 
library(ROCR)
library(AUC)

pred = prediction(test$pred, test$delay)
perf  = performance(pred,"tpr","fpr")

plot(perf)

plot( perf)

## if there is another model to overlay the ROC curves on the same graph 

#pred2 <- prediction(test$pred1, test$delay)
##perf2 <- performance(pred2,"tpr","fpr")
##plot( perf)
#plot(perf2, add = TRUE, colorize = TRUE)


# And the AUC is
AUC_Log = performance(pred, measure = 'auc')@y.values[[1]]
AUC_Log
## 0.868

#### Precision vs Recall graph 

library("DMwR")

PRcurve(test$pred, test$delay)






