
bank = read.csv('datasets/UniversalBank.csv')
head(bank)









drops <- c("ID","ZIP.Code","Mortgage")
bank <- bank[ , !(names(bank) %in% drops)]

str(bank)

hist(bank$Age)

hist(bank$Experience)

hist(bank$Age)


bank$Age2 = ifelse(bank$Age <=18, '<18', ifelse((bank$Age>18 & bank$Age <=30),'18_30',ifelse((bank$Age>30 & bank$Age <=40),'30_40',ifelse((bank$Age>40 & bank$Age <=50),'40_50',ifelse((bank$Age>50 & bank$Age <=60),'50_60','>60')))))

table(bank$Age2)



bank$Experience2 = ifelse(bank$Experience <=1, '<1', ifelse((bank$Experience>1 & bank$Experience <=5),'1_5',ifelse((bank$Experience>5 & bank$Experience <=10),'5_10',ifelse((bank$Experience>10 & bank$Age <=15),'10_15',ifelse((bank$Experience>15 & bank$Experience <=20),'15_20',ifelse((bank$Experience>20 & bank$Experience <=30),'20_30','>30'))))))

table(bank$Experience2)

hist(bank$CCAvg)

bank$CCAvg2 = ifelse(bank$CCAvg <=1, '<1', ifelse((bank$CCAvg>1 & bank$CCAvg <=2),'1_2',ifelse((bank$CCAvg>2 & bank$CCAvg <=3),'2_3',ifelse((bank$CCAvg>3 & bank$CCAvg <=4),'3_4',ifelse((bank$CCAvg>4 & bank$CCAvg <=6),'4_6',ifelse((bank$CCAvg>6 & bank$CCAvg <=10),'6_10','>10'))))))

table(bank$CCAvg2)

quantile(bank$CCAvg,0.5)

bank$CCAvg2 = ifelse(bank$CCAvg <=quantile(bank$CCAvg,0.25),'Low',
                    ifelse(bank$CCAvg >=quantile(bank$CCAvg,0.75),'High', 'Med'
                   ))


table(bank$CCAvg2)

quantile(bank$Income)

bank$Income2 = ifelse(bank$Income <=quantile(bank$Income,0.25),'L',
                    ifelse(bank$Income>=quantile(bank$Income,0.75),'H', 'M'
                   ))


table(bank$Income2)

str(bank)

drops <- c("Income","Age","Experience","CCAvg")
bank <- bank[ , !(names(bank) %in% drops)]

str(bank)

library(arules)

## Replace zeros with missing values (note this code has to be present before converting into factors)
binaryvals = c ("Securities.Account","Personal.Loan","CD.Account","Online","CreditCard")
for (i in binaryvals){
  bank[bank[,i]==0,i] = NA
}




# Convert all columns into Factors
for(i in 1:ncol(bank)){
  bank[,i] = as.factor(bank[,i])
}

str(bank)

df_trans = as(bank,'transactions')
inspect(head(df_trans,6))

##creating rules
rules = apriori(df_trans,parameter = list(supp = 0.001, target = "rules"))
inspect(head(rules,20))

## Subsetting Rules
rules_bank = subset(rules,subset = rhs %pin% "Personal.Loan")
inspect(head(rules_bank,5))


