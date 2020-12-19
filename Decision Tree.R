install.packages("FSelector")  # to get entropy, chi-square, information gain etc
install.packages("rpart")      # to partition dataset 
install.packages("caret")      # to split dataset into into training, testing, validation
install.packages("dplyr")
install.packages("rpart.plot") # plot for trees
install.packages("caTools")

titanic = read.csv(choose.files())
titanic = titanic[,c(1,2,4,5)]

library(dplyr)
titanic = mutate(titanic, Survived=factor(Survived), Pclass=as.numeric(Pclass),Age=as.numeric(Age)) 


# splitting into training and testing dataset
n=891
set.seed(891)
ran = sample(1:n, 0.7*n)
train = titanic[ran,]
test = titanic[-ran,]


# Training the decision tree classifier
library(rpart)
tree = rpart(Survived ~., data=train)
library(rpart.plot)
rpart.plot(tree, type=3)


# Prediction 
tree.Survived.predicted = predict(tree, test, type='class')

# Confusion Matrix to predict the model
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(tree.Survived.predicted, test$Survived)
r = table(tree.Survived.predicted, test$Survived)

accuracy = ((r[1,1] + r[2,2])/sum(r))*100
accuracy

# Visualization 
prp(tree)

