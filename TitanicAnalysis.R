#Importing Training Data Set
train <- read.csv("C:/Sid/R Workspace/Data-Science-Legato/train.csv")
View(train)
table(train$Survived)
prop.table(table(train$Survived))
#Importing Test Data Set
test <- read.csv("C:/Sid/R Workspace/Data-Science-Legato/test.csv")
View(test)
test$Survived <- rep(0,418)
# Lets say all the passengers perish.
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

#Initial Analysis
summary(train$Sex)
prop.table(table(train$Sex,train$Survived),1)

#A greater proportion Females survived as compared to males.
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

# Age variable analysis
summary(train$Age)
# Age variable has about 177 Missing Values (NA's)
# In order to categorize age and then analyzing survival proportions based on age and gender,
# we can introduce a categorical variable child, which will be true(1) for age < 18 and 
#false(0) otherwise.

train$child <-0
train$child[train$Age < 18] <- 1

# Survival Proportions by age and gender.
aggregate(Survived~Sex+child,data=train,FUN = sum)

# Number of people in each subset
aggregate(Survived~child+Sex,data=train,FUN = length)

aggregate(Survived~child+Sex,data=train,FUN=function(x){sum(x)/length(x)})
# Analysis with class variable
summary(as.factor(train$Pclass))

# Making bins for fare variable
train$Fare2 <- "30+"
train$Fare2[train$Fare < 30 & train$Fare >=20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >=10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
# Survival proportion on basis of class, sex and fare
aggregate(Survived~Fare2+Pclass+Sex,data=train,FUN=function(x){sum(x)/length(x)})
# Women in class 3 who paid more than 20, survived less than those who paid less than 20
#making another rough prediction
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Buiding a decision tree
library(rpart)

fit <- rpart(Survived~Pclass+Sex+Fare+Age+SibSp+Parch+Embarked,data=train,method="class")
plot(fit)
text(fit)
#Fancy tree Plot
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
#Submitting my first decision tree
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#overriding the default complexity caps
fit <- rpart(Survived~Pclass+Sex+Fare+Age+SibSp+Parch+Embarked,data=train,method="class",
             control = rpart.control(minsplit=2,cp=0))
fancyRpartPlot(fit)

#The secong decision tree overfitted the data.
#To Analyze names we need to combine the train and test data

test$Survived <- NA

combi <- rbind(train,test)
summary(combi$Name)
# Changing name from factor to character
combi$Name <- as.character(combi$Name)
combi$Name[1]
#
strsplit(combi$Name[1],split = '[,.]' )[[1]][2]
#Applying this function to the whole name column
combi$Title <- sapply(combi$Name,FUN=function(x){strsplit(x,split = '[,.]' )[[1]][2]})
combi$Title <- sub(' ','',combi$Title)
table(combi$Title)
#Combining titles like Mlle Mme to Mlle
combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle' 

