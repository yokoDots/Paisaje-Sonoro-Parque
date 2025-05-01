library(randomForest)
require(caTools)
library(randomForestExplainer)
#data<-read.csv("FactorsAI.csv", header = T)
data<-tibby1[-c(1, 16,19, 20)]

head(data)

###To simplify the problem, attempt only to distinguish if it belongs to morning or no

#rf1 = change 11-13 to 16-8
#rf2 = #change 11-13 to 4-6

data$Horas[data$Horas=="4-6"] <- "11-13"
data<-subset(data, Horas=="11-13"|Horas=="16-18")
#data<-data[1:540,]

str(data$Horas)
summary(data)
#get rid of columns and make factors
randrow<-sample(231:820, 285, replace=FALSE)
data<-data[-c(randrow), ] 

#check class of variable to make factors
sapply(data, class)

#what variables do you want to include?
#taking out non significant and puntos which is random
#get rid of NP, SPL50, SPLmean and Puntos
b<-data
data=b
data<-data[-c(1, 2, 12, 16)]

data <- transform(
  data,
  Horas=as.factor(Horas)
#  Puntos=as.factor(Puntos)
#  Dia=as.factor(Dia)
)
#data$Horas <- factor(data$Horas, levels = c('4-6', '16-18'))

sapply(data, class)

summary(data)


# Categorical variables are expressed as the counts
# R expects missing values to be written as NA. 
# Use the colSums function  to view the missing
# value counts of each column.
# () ???? why did I have this?
#data[ data == "?"] <- NA
colSums(is.na(data))

#remove some excess 4-6 to give balance
randrow<-sample(150:399, 60, replace=FALSE)
data<-data[-c(randrow), ] 

summary(data)

#sample of data for testing using factor of interest

sample = sample.split(data$Horas, SplitRatio = .80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

#initialize randomForest class. 
#Unlike scikit-learn, we don’t need to explicily call the fit method # to train our model.
rf4vsrest <- randomForest(
  Horas ~ .,
  data=train,
  localImp = TRUE)
rf4vsrest
 # The model will automatically attempt to classify each of 
 # the samples in the Out-Of-Bag dataset and display a confusion matrix 
 # with the results.

# predict whether the variables belong to morning or no

pred4vsrest = predict(rf4vsrest, newdata=test[-12])
pred4vsrest
# Use a confusion matrix to evaluate the performance of our model. 
# Values on the diagonal correspond to true positives 
# and true negatives (correct predictions) whereas the others 
# correspond to false positives and false negatives.

cm4vsrest = table(test[,12], pred4vsrest)
cm4vsrest

varImpPlot(rf4vsrest, main = "Mañana vs Resto del día")
explain_forest(rf4vsrest)

plot_predict_interaction(rf4vsrest, data, "ACI", "BI", main = "Mañana vs Resto del día")

vu<-varImp(rf4vsrest)
vu
