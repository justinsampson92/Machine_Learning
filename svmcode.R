library('e1071')
library('SDMTools')
############################################# The following lines of code set up
unique = lapply(unique, as.character)       # our data frame with a names for each
unique = unlist(unique)                     # unique word that occurs in the 
colnames(df) = unique                       # training set. Columns 1 and 24
label = df[,1]                              # correspond to the words 'ham'
label[match(2,label)] = 1                   # and 'spam' respectively. Labels
View(df)                                    # that have a number greater than
df = df[,c(-1,-24)]                         # 1 are set equal to 1. 
unique = unique[c(-1,-24)]                  #
                                            #
############################################### The following code marks a cell as
occurs = apply(df,2, function(x) x > 0)       # TRUE if a word occurs in a text one
num_occurance = apply(occurs,2, sum)          # or more times. The number of TRUEs are
                                              # then counted to get total number of times
remove_words = function(threshold, dataframe){# a word occurs in the training set. A
boolean_vec = num_occurance < threshold       # function is then written to remove 
  return (dataframe[,-(which(boolean_vec == TRUE))])# words that occur less than 
}                                             # a threshold amount of times. 

############################################### The following code sums all the rows
row_sums = apply(df,1, sum)                   # in the data frame and divides each
perc = df / row_sums                          # cell by the row sum. If any row sums
hasNan = apply(perc,1,function(x) any(is.nan(x)))# are zero they are removed from both
perc = perc[!hasNan,]                         # the data frame and the labels.
label = label[!hasNan]                        #
############################################### The following code loops through 
acc = c()                                     # a threshold of removing words that
for (j in 10:30){                             # occur less than a certain amount of
  data = remove_words(j,perc)                 # times. The data is then split into
                                              # training (.8) and test (.2) sets
  train_data = data[(1:(0.8*nrow(data))),]    # of the data and labels to accompany.
  train_label = label[(1:(0.8*nrow(data)))]   # An svm model is then tuned over
                                              # a range of cost parameters and the
  test_data = data[-(1:(0.8*nrow(data))),]    # best model is chosen. The model is then
  test_label = label[-(1:(0.8*nrow(data)))]   # run on the training data using the predict
                                              # function. In different iteration of the code
  model_data = data.frame(x=train_data,       # either, accuracy or PPV,
  y=as.factor(train_label))                   # is appended to a list for the user to chose
  tune.mod = tune(svm, y~., data = model_data,#  the most appropriate word count threshold.
  kernel = 'linear', ranges = list(cost= (0.001, 0.01,0.1,1,5)))
  bestmod = tune.mod$best.model               # 
                                              # 
  test_dat = data.frame(x =test_data,         # 
         y = as.factor(test_label))           #
                                              #
  y = predict(bestmod, newdata=test_dat)      #
  acc = append(acc,(table(y,test_label)[4]/   #
  sum(table(y,test_label)[3]+table(y,test_label)[4])))
}                                             #
                                              #
############################################### The following code is to handle test data
l = df_test[,1]                               # based on the output of our Python parsing
df_test = df_test[,-c(1,24)]                  # script. Columns 1 and 24 are the 'spam'   
unique_test = unique_test[-c(1,24)]           # and 'ham' columns. The unique list of 
unique_test = lapply(unique_test, as.character)# test words is then set as the data frame's
unique_test = unlist(unique_test)             # column names
colnames(df_test) = unique_test               #
############################################### The following code turns the count
row_sums = apply(df_test,1, sum)              # matrix into a percentage matrix
perc_test = df_test / row_sums                # the same way as done in the above 
hasNan = apply(perc_test,1,function(x) any(is.nan(x)))# training matrix. This time
perc_test[which(hasNan== T),] = 0             # however we cannot simply remove rows
                                              # that sum to zero so we just set them to zero.
                                              #
                                              #
############################################### This code generates a data frame to be
cells = length(data)*nrow(perc_test)          # populated with the same number of columns as
test_data = rep(0,cells)                      # the training matrix and the same number of rows
dim(test_data) = c(nrow(perc_test),length(data)) # as the test matrix.It also sets the names
test_data = data.frame(test_data)             # of the new test matrix to the same as the 
colnames(test_data) = colnames(data)          # training matrix. 
index = match(names(perc_test),names(data))   #
############################################### This loop checks to see if a word in the 
for(i in 1:ncol(perc_test)){                  # test matrix matches a word in the empty
  if (!is.na(index[i]) ){                     # matrix to be populated. If it does then 
    test_data[,index[i]] = perc_test[,i]      # those values are filled in for model to 
  }                                           # be ran on. The basic idea here is to get
}                                             # a matrix with all the same words as the training
                                              # set having started with a unique set of words from
############################################### the test set.  
                                              #
test_dat = data.frame(x =test_data, y = as.factor(l))# This code sets up the test data
y = predict(bestmod, newdata=test_dat)        # and predicts it using our best model.      
save(preds, file = 'pred_group_2.rda')        # Finally the predictions are saved as a rda.
###############################################
