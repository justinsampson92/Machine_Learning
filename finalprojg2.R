library(ROCR)
library('e1071')
library('SDMTools')
library("randomForest", lib.loc="~/R/win-library/3.0")
library('caret')
?accuracy
############################################# Loading in the data
df <- read.csv("C:/Users/Geoff Kaufman/Favorites/Dropbox/Stat 154 Final Project/dfg.csv", header=FALSE)
unique <- read.csv("C:/Users/Geoff Kaufman/Favorites/Dropbox/Stat 154 Final Project/uniqueg.csv", header=FALSE)
PF <- read.csv("C:/Users/Geoff Kaufman/Favorites/Dropbox/Stat 154 Final Project/matrix_selectedPF.csv", header=TRUE, stringsAsFactors=FALSE)
#create list of unique words
unique = lapply(unique, as.character)
unique = unlist(unique)
#assign list as column names of the dataframe
colnames(df) = unique[1:(length(unique)-2)]
#extract labels and remove them from the word matrix/unique vector
label = ifelse(df[,24] > 0, 1, 0)
summary(label)
df = df[,c(-1,-24)]
unique = unique[c(-1,-24)]

#pfs is the power feature matrix sans labels
PFs = PF[,c(-1,-2)]

###############################################Normalization/Filtering

#create columns which show simply whether or not a word appeared
#in a sentence, as opposed to a count. For use in calculating frequency
#and eliminating colinearity
occurs = apply(df,2, function(x) x > 0)

#colinearity: eliminate columns(words) that appear together 100% of the time
duplicates=which(duplicated(occurs, MARGIN = 2))
occurs=occurs[,-c(duplicates)]
df = df[,-c(duplicates)]

#creat frequncy counts
num_occurance = apply(occurs,2, sum)

#function to remove words that are below a given frequency threshold
remove_words = function(threshold, dataframe){
boolean_vec = num_occurance < threshold
  
  return (dataframe[,-(which(boolean_vec == TRUE))])
}

#normalize
row_sums = apply(df,1, sum)
perc = df / row_sums


words = remove_words(24, perc)



########################################## Training, Word Features Only

#remove rows with all stopwords, as power features are not involved
hasNan = apply(words,1,function(x) any(is.nan(x)))
words1 = words[!hasNan,]

#build dataframe
data = data.frame(y=as.factor(label), words1)

#partition train/test 
index = seq(1,ceiling(0.8*nrow(data)))
training = data[index,]
test = data[-index,]

#cross validate on number of features at each split
cv.mtry = rfcv(training[,-1], training$y, cv.fold=10)
plot(cv.mtry2$n.var,cv.mtry2$error.cv)

#train the random forest with optimal mtry=50
model = randomForest(y~., data=training, mtry=50)

#predict new class values
ypred = predict(model, type="class", newdata=test, na.action=na.omit)

#calculate test error
sum(ifelse(ypred!=test$y, 1, 0), na.rm=TRUE)/length(ypred)

#build confusion matrix
predictions = ypred
table(predictions, test$y)

#plot roc curve using ROCR lib
rf.pr = predict(model,type="prob",newdata=test)[,2]
rf.pred = prediction(rf.pr, test$y)
rf.perf = performance(rf.pred,"tpr","fpr")
plot(rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

################################## Word + PF combo

#set NA values to zero. stopword rows are not removed since power features may exist
words[is.na(words)]=0

#build data frame, repeat steps above for new data frame
data2 = data.frame(y=as.factor(label), words2, PFs)


index2 = seq(1,ceiling(0.8*nrow(data2)))
training2 = data2[index2,]
test2 = data2[-index2,]

cv.mtry2 = rfcv(training2[,-1], training2$y, cv.fold=10)
plot(cv.mtry2$n.var,cv.mtry2$error.cv)

model2 = randomForest(y~., data=training2, mtry=50)
ypred2 = predict(model2, type="class", newdata=test2, na.action=na.omit)
sum(ifelse(ypred2!=test2$y, 1, 0), na.rm=TRUE)/length(ypred2)

predictions = ypred2
table(predictions, test2$y)


rf.pr2 = predict(model2,type="prob",newdata=test2)[,2]

rf.pred2 = prediction(rf.pr2, test2$y)
rf.perf2 = performance(rf.pred2,"tpr","fpr")
plot(rf.perf2,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


######################### conform unseen test data and make predictions

#load in data
View(df_test) <- read.csv("C:/Users/Geoff Kaufman/Favorites/Dropbox/Stat 154 Final Project/dfg_test.csv", header=FALSE)
unique_test <- read.csv("C:/Users/Geoff Kaufman/Favorites/Dropbox/Stat 154 Final Project/uniqueg_test.csv", header=FALSE)
PF_test <- read.csv("C:/Users/Geoff Kaufman/Favorites/Dropbox/Stat 154 Final Project/test_PF.csv", header=TRUE, stringsAsFactors=FALSE)

#name columns
unique_test = lapply(unique_test, as.character)
unique_test = unlist(unique_test)
colnames(df_test) = unique_test

#extract labels
l = df_test[,24]

#remove labels
df_test = df_test[,c(-1,-24)]

#normalize
row_sums = apply(df_test,1, sum)
perc_test = df_test / row_sums

#create empty matrix, populate it with test values for model-relevant features
cells = length(words)*nrow(perc_test)

test_data = rep(0,cells) ##generate 0s
dim(test_data) = c(nrow(perc_test),length(words)) # make nrow = text rows, make ncol = ncol(unsupervised matrix)
test_data = data.frame(test_data) #make data frame
colnames(test_data) = colnames(words) # make var_names the same
index = (match(names(perc_test),names(words))) #get the column number of trained matrix that matches the columns of perc_test


for(i in 1:ncol(perc_test)){
  if (!is.na(index[i]) ){
    test_data[,index[i]] = perc_test[,i]
  }
} ##replace the correct columns in the test_data to the values in perc
test_data = data.frame(y = as.factor(l),test_data, PF_test[,c(-1,-2)]) ## add 'x.' to the names so it reads correctly
which(names(words)!=names(test_data))
ypred = predict(model2, newdata=test_data) ## predict
sum(ifelse(ypred!=test_data$y, 1, 0), na.rm=TRUE)/length(ypred)

predictions = ypred
table(predictions, test_data$y)
#roc curve
rf.pr2 = predict(model2,type="prob",newdata=test_data)[,2]
rf.pred2 = prediction(rf.pr2, test_data$y)
rf.perf2 = performance(rf.pred2,"tpr","fpr")
plot(rf.perf2,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
     
