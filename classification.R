#install packages
install.packages('plotly')
install.packages('e1071', dependencies=TRUE)

#init packages
libs <- c("tm", "plyr", "class", "caret", "MASS","caTools","rpart","rpart.plot","rlist")
lapply(libs, require, character.only = TRUE)

#set parameters
categories <- c("comp.graphics",
               "comp.os.ms-windows.misc",
               "comp.sys.ibm.pc.hardware",
               "comp.sys.mac.hardware",
               "comp.windows.x",
               "misc.forsale",
               "rec.autos",
               "rec.motorcycles",
               "rec.sport.baseball",
               "rec.sport.hockey",
               "sci.crypt",
               "sci.electronics",
               "sci.med",
               "sci.space",
               "talk.politics.misc",
               "talk.politics.guns",
               "talk.politics.mideast",
               "talk.religion.misc",
               "alt.atheism",
               "soc.religion.christian")

#parent directory name
pathname <- "D:/Sem 2/Temporal and spatial data/HW2/20news-18828"

#data preprocessing
cleancorpus <- function(corpus){
  corpus.tmp <- tm_map (corpus, removePunctuation)
  corpus.tmp <- tm_map (corpus.tmp, removeNumbers)
  corpus.tmp <- tm_map (corpus.tmp, stripWhitespace)
  corpus.tmp  <- tm_map (corpus.tmp, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(removeWords),stopwords("english"))
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(removeWords),stopwords("SMART"))
  corpus.tmp <- tm_map (corpus.tmp, stemDocument)
  return(corpus.tmp)
}

#build TDM
buildTDM <- function(cate, path){
    s.dir <- sprintf("%s/%s", path, cate)
    s.cor <- Corpus(DirSource(directory = s.dir, encoding = "UTF-8"),
                    readerControl=list(reader=readPlain,language="en"))
    s.cor.cl <- cleancorpus(s.cor)
    s.tdm <- TermDocumentMatrix(s.cor.cl)
    s.tdm <- removeSparseTerms(s.tdm, 0.85)
    result <- list(name = cate, tdm = s.tdm) 
}
  
tdm <- lapply(categories, buildTDM, path = pathname)

#attach category name
bindCategoryToTdm <- function(tdm){
  s.mat <- t(data.matrix(tdm[["tdm"]]))               #transposed to Document Term Matrix
  s.df <- as.data.frame(s.mat)
  s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "newsgroup"
  return(s.df)
}
  
CateTdm <- lapply(tdm, bindCategoryToTdm)

#stack
tdm.stack <- do.call(rbind.fill, CateTdm)
tdm.stack[is.na(tdm.stack)] <- 0
tdm.stack.df <- as.data.frame(tdm.stack)
dim(tdm.stack.df)


#===========================================================
#knn before feature selection
set.seed(400)

#Create train and test dataset. Target variable is newsgroup
idx = createDataPartition(tdm.stack.df$newsgroup, p = 4/5, list= FALSE)
trainDF <- tdm.stack.df[idx,]
testDF <- tdm.stack.df[-idx,]

#choose the parameters for the train function (5-fold cross validation)
ControlParameters <- trainControl(method = "cv",
                                  number = 5,
                                  savePredictions = TRUE)

#model knn
modelknn <- train(newsgroup~.,
                  data = trainDF,
                  method = "knn",
                  trControl = ControlParameters,
                  preProcess = c("center","scale"))
                  #tuneLength = 10,
modelknn
plot(modelknn)

#check the predictions on the test dataset
predictions <- predict(modelknn, testDF)

#check the confusion matrix
conf.mat <- table(predictions = predictions, actual = testDF$newsgroup)
(accuracy.knn <- sum(diag(conf.mat)) / length(testDF$newsgroup) * 100)
cat(paste("Accuracy(%):", format(accuracy.knn), "\n",sep=" "))

#Per-class Precision, Recall, and F-1
diag = diag(conf.mat ) # number of correctly classified instances per class 
rowsums = apply(conf.mat , 1, sum) # number of instances per class
colsums = apply(conf.mat , 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 

#overall f-measure
precision = conf.mat[1,1]/sum(conf.mat[,1])
recall = conf.mat[1,1]/sum(conf.mat[1,])
f = 2 * (precision * recall) / (precision + recall)
cat(paste("F-measure:(%)", format(f * 100), "\n",sep=" "))

#==================================================================
#decision tree model before feature selection
set.seed(3333)

model.dtree <- train(newsgroup~., 
                        data = trainDF, 
                        method = "rpart",
                        parms = list(split = "information"),
                        trControl=ControlParameters)

plot(model.dtree)
prp(model.dtree$finalModel, box.palette = "Reds")
summary(model.dtree)

#check predictions on test dataset and confusion matrix
dtree.pred <- predict(model.dtree, testDF)
dtree.conf.mat <- table(predictions = dtree.pred , actual = testDF$newsgroup)
(accuracy.dtree <- sum(diag(dtree.conf.mat)) / length(testDF$newsgroup) * 100)
cat(paste("Accuracy(%):", format(accuracy.dtree), "\n",sep=" "))

#f-measures
diag = diag(dtree.conf.mat) # number of correctly classified instances per class 
rowsums = apply(dtree.conf.mat , 1, sum) # number of instances per class
colsums = apply(dtree.conf.mat , 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)


#Plotting ROC curve and calculate AUC metric
predwithprob <- predict(model.dtree, testDF, type='prob')
auc <- auc(testDF$newsgroup, predwithprob[,2])
plot(roc(testDF$newsgroup, predwithprob[,2]))

#==================================================================
#FEATURE SELECTION

#seperate column with newsgroup
tdm.stack.group <- tdm.stack[,"newsgroup"]
tdm.stack.nogroup <- tdm.stack[,!colnames(tdm.stack) %in% "newsgroup"]

#sort matrix by sum of frequencies of words
sortedMatrix <- sort(colSums(tdm.stack.nogroup), decreasing=TRUE)  

#select top 100 words from sorted matrix
top100 <- data.frame(head(sortedMatrix, 100))
top100.list <- list(rownames(top100))

#extract values in all the documents for top 100 words
tdm.100.df <- data.frame(tdm.stack[,top100.list[[1]]])
tdm.stack.group.df <- data.frame(tdm.stack.group)
tdm.stack.100 <- cbind(tdm.100.df,tdm.stack.group.df)

#================================================================

#knn after feature selection
set.seed(500)

#Create train and test dataset for 100 words
idx = createDataPartition(tdm.stack.100$tdm.stack.group, p = 4/5, list= FALSE)
trainDF.100 <- tdm.stack.100[idx,]
testDF.100 <- tdm.stack.100[-idx,]

#model knn with 5-fold cv
modelknn.100 <- train(tdm.stack.group~., 
                  data = trainDF.100,
                  method = "knn",
                  trControl = ControlParameters,
                  preProcess = c("center","scale"))

plot(modelknn.100)

#check the predictions on the test dataset
p <- predict(modelknn.100, testDF.100)
conf <- table(predictions = p, actual = testDF.100$tdm.stack.group)
(accuracy.knn.100 <- sum(diag(conf)) / length(testDF.100$tdm.stack.group) * 100)
cat(paste("Accuracy(%):", format(accuracy.knn.100), "\n",sep=" ") )

#compute f-measure
precision.100 = conf[1,1]/sum(conf[,1])
recall.100 = conf[1,1]/sum(conf[1,])
f.100 = 2 * (precision.100 * recall.100) / (precision.100 + recall.100)
cat(paste("F-measure(%):", format(f.100 * 100), "\n",sep=" ") )

#Per-class Precision, Recall, and F-1
diag = diag(conf ) # number of correctly classified instances per class 
rowsums = apply(conf, 1, sum) # number of instances per class
colsums = apply(conf, 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 

#======================================================================

#Decision tree model after feature selection
set.seed(666)

model.dtree.100 <- train(tdm.stack.group~., 
                     data = trainDF.100, 
                     method = "rpart",
                     parms = list(split = "information"),
                     trControl=ControlParameters)

plot(model.dtree)

#check predictions on test dataset and confusion matrix
dtree.pred.100 <- predict(model.dtree.100, testDF.100)
dtree.conf.mat.100 <- table(predictions = dtree.pred.100 , actual = testDF.100$tdm.stack.group)
(accuracy.dtree.100 <- sum(diag(dtree.conf.mat.100)) / length(testDF.100$tdm.stack.group) * 100)
cat(paste("Accuracy(%):", format(accuracy.dtree.100), "\n",sep=" ") )

#========================================================================

#6. comparing frequencies for all categories with both the classfiers

#Per-class Precision, Recall, and F-1
#knn
t1 = diag(conf.mat ) # number of correctly classified instances per class 
t2 = apply(conf.mat , 2, sum) # number of predicttions per class
acc = (t1 / t2 ) *  100
data.frame(format(acc, digits=2)) 

#decision tree
t3 = diag(dtree.conf.mat)
t4 = apply(dtree.conf.mat , 2, sum)
acc = (t3 / t4 ) *  100
data.frame(format(acc, digits=2)) 

x <- c('comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware', 
       'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 
       'rec.motorcycles', 'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 
       'sci.electronics','sci.med', 'sci.space','talk.politics.misc','talk.politics.guns',
       'talk.politics.mideast','talk.religion.misc', 'alt.atheism','soc.religion.christian')
y1 <- c(60,80,62,72,65,88,77,89,82,87,83,89,74,78,83,84,82,69,81,79)
y2 <- c(0,73,0,0,0,60,0,100,0,57,0,0,0,0,0,0,48,0,0,0)
data <- data.frame(x, y1, y2)

p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'Knn Accuracy', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~y2, name = 'Decision Tree Accuracy', marker = list(color = 'rgb(252,141,89)')) %>%
  layout(xaxis = list(title = "Article Category", tickangle = -45),
         yaxis = list(title = "Accuracy(%)"),
         margin = list(b = 100),
         barmode = 'group')

#==============================================================
#point 5
x <- c('Accuracy', 'F-measure')
y1 <- c(78.32, 66.48)
y2 <- c(17.58, 11.90)
data <- data.frame(x, y1, y2)

p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'Knn', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~y2, name = 'Decision Tree', marker = list(color = 'rgb(252,141,89)')) %>%
  layout(xaxis = list(title = "Measures", tickangle = -45),
         yaxis = list(title = "Percentage(%)"),
         margin = list(b = 100),
         barmode = 'group')


#==============================================================
#point 4
x <- c(5,7,9)
y1 <- c(76.89, 76.18, 75.46)
y2 <- c(58.27, 58.03,57.74)
data <- data.frame(x, y1, y2)

p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'Accuracy before feature selection', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~y2, name = 'Accuracy after feature selection', marker = list(color = 'rgb(252,141,89)')) %>%
  layout(xaxis = list(title = "k value"),
         yaxis = list(title = "Accuracy in Percentage(%)"),
         margin = list(b = 100),
         barmode = 'group')

