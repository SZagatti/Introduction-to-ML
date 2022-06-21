require(tree)
require(randomForest)
require(caret)
require(e1071)
require(pROC)

iris <- iris
set <- iris[51:150, ]
noseplen <- set[ ,2:5]
nosepwid <- set[ ,c(1,3:5)]
nopetlen <- set[ ,c(1,2,4,5)]
nopetwid <- set[ ,c(1:3,5)]

set <- droplevels(set)
noseplen <- droplevels(noseplen)
nosepwid <- droplevels(nosepwid)
nopetlen <- droplevels(nopetlen)
nopetwid <- droplevels(nopetwid)


rf.set <- randomForest(Species~.,data=set,importance=TRUE)
rf.noseplen <- randomForest(Species~., data=noseplen, importance=TRUE)
rf.nosepwid <- randomForest(Species~., data=nosepwid, importance=TRUE)
rf.nopetlen <- randomForest(Species~., data=nopetlen, importance=TRUE)
rf.nopetwid <- randomForest(Species~., data=nopetwid, importance=TRUE)

rf.set
round(importance(rf.set), 2)
rf.noseplen
rf.nosepwid
rf.nopetlen
rf.nopetwid

set.seed(2354)
trainingset <- sample(1:nrow(set),60)

rf.set <- randomForest(Species~., data=set, subset = trainingset, importance=TRUE, ntree=100)
rf.pred <- predict(rf.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(rf.pred, Species))
confusionMatrix(tab)

rf.set <- randomForest(Species~., data=set, subset = trainingset, importance=TRUE, ntree=250)
rf.pred <- predict(rf.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(rf.pred, Species))
confusionMatrix(tab)

rf.set <- randomForest(Species~., data=set, subset = trainingset, importance=TRUE, ntree=500)
rf.pred <- predict(rf.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(rf.pred, Species))
confusionMatrix(tab)

rf.set <- randomForest(Species~., data=set, subset = trainingset, importance=TRUE, ntree=750)
rf.pred <- predict(rf.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(rf.pred, Species))
confusionMatrix(tab)

rf.set <- randomForest(Species~., data=set, subset = trainingset, importance=TRUE, ntree=1000)
rf.pred <- predict(rf.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(rf.pred, Species))
confusionMatrix(tab)

rf.set <- randomForest(Species~., data=set, subset = trainingset, importance=TRUE, ntree=2500)
rf.pred <- predict(rf.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(rf.pred, Species))
confusionMatrix(tab)

rf.set <- randomForest(Species~., data=set, subset = trainingset, importance=TRUE, ntree=5000)
rf.pred <- predict(rf.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(rf.pred, Species))
confusionMatrix(tab)


tree.set <- tree(Species~., set, subset = trainingset)
tree.pred <- predict(tree.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(tree.pred, Species))
confusionMatrix(tab)

rf.set <- randomForest(Species~., data=set, subset = trainingset, importance=TRUE)
rf.pred <- predict(rf.set, set[-trainingset,], type="class")
tab <-with(set[-trainingset,], table(rf.pred, Species))
confusionMatrix(tab)

newset <- data.frame(set[-trainingset,],tree.pred)
newset$tree.pred<- as.numeric(newset$tree.pred)
newset$Species<- as.numeric(newset$Species)
newset2 <- data.frame(set[-trainingset,],rf.pred)
newset2$rf.pred<- as.numeric(newset2$rf.pred)
newset2$Species<- as.numeric(newset2$Species)
roc <- roc(newset, tree.pred, Species)
roc2 <- roc(newset2, rf.pred, Species)
plot(roc,col="red")
lines(roc2, col="blue")
auc(roc)
auc(roc2)