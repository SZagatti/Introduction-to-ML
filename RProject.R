require(tree)
require(randomForest)
require(caret)
require(e1071)
require(ggplot2)
  
set.seed(777)


#Operazioni sui Dati
leaf <- read.csv("leaf.csv", header = FALSE)
dataleaf <- data.frame(Name=leaf[ ,1], Eccentricity=leaf[ ,3], Aspect.Ratio=leaf[ ,4], Elongation=leaf[ ,5], Solidity=leaf[ ,6], Stochastic.Convexity=leaf[ ,7], Isoperimetric.Factor=leaf[ ,8], Maximal.Indentation.Depth=leaf[ ,9], Lobedness=leaf[ ,10], Average.Intensity=leaf[ ,11], Average.Contrast=leaf[ ,12], Smoothness=leaf[ ,13], Third.Moment=leaf[ ,14], Uniformity=leaf[ ,15], Entropy=leaf[ ,16])
dataleaf$Name <- as.factor(dataleaf$Name)

#Istogramma della frequenza di ogni classe:
plot <- ggplot(dataleaf, aes(x=dataleaf$Name)) + geom_bar()
print(plot + labs(y="Frequencies", x = "Classes of Leaves", Name="Classes"))

#Tree:
tree.leaf <- tree(Name~., dataleaf)
tree.pred <-  predict(tree.leaf, dataleaf, type="class")
tab1 <- with(dataleaf, table(tree.pred, Name))

#Random Forest:
rf.leaf <- randomForest(Name~.,data=dataleaf, importance=TRUE)
rf.pred <- predict(rf.leaf, dataleaf, type="class")
tab2 <- with(dataleaf, table(rf.pred, Name))

#SVM:
svm.leaf <- svm(Name~.,data=dataleaf)
svm.pred <- predict(svm.leaf, dataleaf, type="class")
tab3 <- with(dataleaf, table(svm.pred, Name))

#K-Fold Cross Validation Tree:
n <- nrow(dataleaf)
K <- 5
dim.fold <- n%/%K
alea <- runif(n)
rang <- rank(alea)
fold <- (rang-1)%/%dim.fold + 1
fold <- as.factor(fold)

Acc.Var.Tree <- data.frame(Accuracy1=numeric(0), Accuracy2=numeric(0), Accuracy3=numeric(0), Accuracy4=numeric(0), Accuracy5=numeric(0), Accuracy6=numeric(0), Accuracy7=numeric(0), Accuracy8=numeric(0), Accuracy9=numeric(0), Accuracy10=numeric(0), Accuracy11=numeric(0), Accuracy12=numeric(0), Accuracy13=numeric(0), Accuracy14=numeric(0), Accuracy15=numeric(0), Accuracy16=numeric(0), Accuracy17=numeric(0), Accuracy18=numeric(0), Accuracy19=numeric(0), Accuracy20=numeric(0), Accuracy21=numeric(0), Accuracy22=numeric(0), Accuracy23=numeric(0), Accuracy24=numeric(0), Accuracy25=numeric(0), Accuracy26=numeric(0), Accuracy27=numeric(0), Accuracy28=numeric(0), Accuracy29=numeric(0), Accuracy30=numeric(0))
Accuracy.Tree <- numeric(0)

for (k in 1:K) {
  somma <- 0
  tree <- tree(Name~., data=dataleaf[fold!=k,], method="class")
  pred <- predict(tree, newdata=dataleaf[fold==k,], type="class")
  tab <- with(dataleaf[fold==k,], table(pred, Name))
  
  for (i in 1:30) {
    somma=somma + tab[i,i]
  }
  for (a in 1:30) {
    somma2 <- 0
    for (b in 1:30) {
    somma2 <- somma2 + tab[b,a]
    }
  Acc.Var.Tree[k,a] <- round(tab[a,a]/somma2,4)
  }
  Accuracy.Tree[k] <- round(somma/sum(tab),4)
}

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

Accuracy.Tree[is.nan(Accuracy.Tree)] <- 0
Acc.Var.Tree[is.nan(Acc.Var.Tree)] <- 0

Accuracy.Tree.cv <- mean(Accuracy.Tree)
Acc.Var.Tree.cv <- numeric(0)
for (k in 1:30) {
  Acc.Var.Tree.cv[k] <-  mean(Acc.Var.Tree[ ,k])
}

#K-Fold Cross Validation RF:

Acc.Var.RF <- data.frame(Accuracy1=numeric(0), Accuracy2=numeric(0), Accuracy3=numeric(0), Accuracy4=numeric(0), Accuracy5=numeric(0), Accuracy6=numeric(0), Accuracy7=numeric(0), Accuracy8=numeric(0), Accuracy9=numeric(0), Accuracy10=numeric(0), Accuracy11=numeric(0), Accuracy12=numeric(0), Accuracy13=numeric(0), Accuracy14=numeric(0), Accuracy15=numeric(0), Accuracy16=numeric(0), Accuracy17=numeric(0), Accuracy18=numeric(0), Accuracy19=numeric(0), Accuracy20=numeric(0), Accuracy21=numeric(0), Accuracy22=numeric(0), Accuracy23=numeric(0), Accuracy24=numeric(0), Accuracy25=numeric(0), Accuracy26=numeric(0), Accuracy27=numeric(0), Accuracy28=numeric(0), Accuracy29=numeric(0), Accuracy30=numeric(0))
Accuracy.RF <- numeric(0)

for (k in 1:K) {
  somma <- 0
  rf <- randomForest(Name~., data=dataleaf[fold!=k,], method="class")
  pred <- predict(rf, newdata=dataleaf[fold==k,], type="class")
  tab <- with(dataleaf[fold==k,], table(pred, Name))
  for (i in 1:30) {
    somma=somma + tab[i,i]
  }
  for (a in 1:30) {
    somma2 <- 0
    for (b in 1:30) {
      somma2 <- somma2 + tab[b,a]
    }
    Acc.Var.RF[k,a] <- round(tab[a,a]/somma2,4)
  }
  Accuracy.RF[k] <- round(somma/sum(tab),4)
}

Accuracy.RF[is.nan(Accuracy.RF)] <- 0
Acc.Var.RF[is.nan(Acc.Var.RF)] <- 0

Accuracy.RF.cv <- mean(Accuracy.RF)
Acc.Var.RF.cv <- numeric(0)
for (k in 1:30) {
  Acc.Var.RF.cv[k] <-  mean(Acc.Var.RF[ ,k])
}

#K-Fold Cross Validation SVM:

Acc.Var.SVM <- data.frame(Accuracy1=numeric(0), Accuracy2=numeric(0), Accuracy3=numeric(0), Accuracy4=numeric(0), Accuracy5=numeric(0), Accuracy6=numeric(0), Accuracy7=numeric(0), Accuracy8=numeric(0), Accuracy9=numeric(0), Accuracy10=numeric(0), Accuracy11=numeric(0), Accuracy12=numeric(0), Accuracy13=numeric(0), Accuracy14=numeric(0), Accuracy15=numeric(0), Accuracy16=numeric(0), Accuracy17=numeric(0), Accuracy18=numeric(0), Accuracy19=numeric(0), Accuracy20=numeric(0), Accuracy21=numeric(0), Accuracy22=numeric(0), Accuracy23=numeric(0), Accuracy24=numeric(0), Accuracy25=numeric(0), Accuracy26=numeric(0), Accuracy27=numeric(0), Accuracy28=numeric(0), Accuracy29=numeric(0), Accuracy30=numeric(0))
Accuracy.SVM <- numeric(0)

for (k in 1:K) {
  somma <- 0
  svm <- svm(Name~., data=dataleaf[fold!=k,], method="class")
  pred <- predict(svm, newdata=dataleaf[fold==k,], type="class")
  tab <- with(dataleaf[fold==k,], table(pred, Name))
  for (i in 1:30) {
    somma=somma + tab[i,i]
  }
  for (a in 1:30) {
    somma2 <- 0
    for (b in 1:30) {
      somma2 <- somma2 + tab[b,a]
    }
    Acc.Var.SVM[k,a] <- round(tab[a,a]/somma2,4)
  }
  Accuracy.SVM[k] <- round(somma/sum(tab),4)
}

Accuracy.SVM[is.nan(Accuracy.SVM)] <- 0
Acc.Var.SVM[is.nan(Acc.Var.SVM)] <- 0

Accuracy.SVM.cv <- mean(Accuracy.SVM)
Acc.Var.SVM.cv <- numeric(0)
for (k in 1:30) {
  Acc.Var.SVM.cv[k] <-  mean(Acc.Var.SVM[ ,k])
}

Sing.Var.Accuracy <- data.frame(Tree=Acc.Var.Tree.cv, RF=Acc.Var.RF.cv, SVM=Acc.Var.SVM.cv)

# K FOLD CROSS VALIDATION TREE SICURO

flds <- createFolds(dataleaf$Name , k = K, list = TRUE, returnTrain = FALSE)

Acc.Var.Tree2 <- data.frame(Accuracy1=numeric(0), Accuracy2=numeric(0), Accuracy3=numeric(0), Accuracy4=numeric(0), Accuracy5=numeric(0), Accuracy6=numeric(0), Accuracy7=numeric(0), Accuracy8=numeric(0), Accuracy9=numeric(0), Accuracy10=numeric(0), Accuracy11=numeric(0), Accuracy12=numeric(0), Accuracy13=numeric(0), Accuracy14=numeric(0), Accuracy15=numeric(0), Accuracy16=numeric(0), Accuracy17=numeric(0), Accuracy18=numeric(0), Accuracy19=numeric(0), Accuracy20=numeric(0), Accuracy21=numeric(0), Accuracy22=numeric(0), Accuracy23=numeric(0), Accuracy24=numeric(0), Accuracy25=numeric(0), Accuracy26=numeric(0), Accuracy27=numeric(0), Accuracy28=numeric(0), Accuracy29=numeric(0), Accuracy30=numeric(0))
Accuracy.Tree2 <- numeric(0)

for (s in 1:K) {
  tree <- tree(Name~., data=dataleaf[-flds[[s]], ], method="class")
  pred <- predict(tree, newdata=dataleaf[flds[[s]],], type="class")
  tab <- with(dataleaf[flds[[s]],], table(pred, Name))

  somma <- 0
  for (i in 1:30) {
    somma=somma + tab[i,i]
  }
  for (a in 1:30) {
    somma2 <- 0
    for (b in 1:30) {
      somma2 <- somma2 + tab[b,a]
    }
    Acc.Var.Tree2[s,a] <- round(tab[a,a]/somma2,4)
  }
  Accuracy.Tree2[s] <- round(somma/sum(tab),4)
}

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

Accuracy.Tree2[is.nan(Accuracy.Tree2)] <- 0
Acc.Var.Tree2[is.nan(Acc.Var.Tree2)] <- 0

Accuracy.Tree.cv2 <- mean(Accuracy.Tree2)
Acc.Var.Tree.cv2 <- numeric(0)
for (k in 1:30) {
  Acc.Var.Tree.cv2[k] <-  mean(Acc.Var.Tree2[ ,k])
}


# K FOLD CROSS VALIDATION RF

Acc.Var.RF2 <- data.frame(Accuracy1=numeric(0), Accuracy2=numeric(0), Accuracy3=numeric(0), Accuracy4=numeric(0), Accuracy5=numeric(0), Accuracy6=numeric(0), Accuracy7=numeric(0), Accuracy8=numeric(0), Accuracy9=numeric(0), Accuracy10=numeric(0), Accuracy11=numeric(0), Accuracy12=numeric(0), Accuracy13=numeric(0), Accuracy14=numeric(0), Accuracy15=numeric(0), Accuracy16=numeric(0), Accuracy17=numeric(0), Accuracy18=numeric(0), Accuracy19=numeric(0), Accuracy20=numeric(0), Accuracy21=numeric(0), Accuracy22=numeric(0), Accuracy23=numeric(0), Accuracy24=numeric(0), Accuracy25=numeric(0), Accuracy26=numeric(0), Accuracy27=numeric(0), Accuracy28=numeric(0), Accuracy29=numeric(0), Accuracy30=numeric(0))
Accuracy.RF2 <- numeric(0)

for (s in 1:K) {
  rf <- randomForest(Name~., data=dataleaf[-flds[[s]], ], method="class")
  pred <- predict(rf, newdata=dataleaf[flds[[s]],], type="class")
  tab <- with(dataleaf[flds[[s]],], table(pred, Name))

  somma <- 0
  for (i in 1:30) {
    somma=somma + tab[i,i]
  }
  for (a in 1:30) {
    somma2 <- 0
    for (b in 1:30) {
      somma2 <- somma2 + tab[b,a]
    }
    Acc.Var.RF2[s,a] <- round(tab[a,a]/somma2,4)
  }
  Accuracy.RF2[s] <- round(somma/sum(tab),4)
}

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

Accuracy.RF2[is.nan(Accuracy.RF2)] <- 0
Acc.Var.RF2[is.nan(Acc.Var.RF2)] <- 0

Accuracy.RF.cv2 <- mean(Accuracy.RF2)
Acc.Var.RF.cv2 <- numeric(0)
for (k in 1:30) {
  Acc.Var.RF.cv2[k] <-  mean(Acc.Var.RF2[ ,k])
}

# K FOLD CROSS VALIDATION SVM

Acc.Var.SVM2 <- data.frame(Accuracy1=numeric(0), Accuracy2=numeric(0), Accuracy3=numeric(0), Accuracy4=numeric(0), Accuracy5=numeric(0), Accuracy6=numeric(0), Accuracy7=numeric(0), Accuracy8=numeric(0), Accuracy9=numeric(0), Accuracy10=numeric(0), Accuracy11=numeric(0), Accuracy12=numeric(0), Accuracy13=numeric(0), Accuracy14=numeric(0), Accuracy15=numeric(0), Accuracy16=numeric(0), Accuracy17=numeric(0), Accuracy18=numeric(0), Accuracy19=numeric(0), Accuracy20=numeric(0), Accuracy21=numeric(0), Accuracy22=numeric(0), Accuracy23=numeric(0), Accuracy24=numeric(0), Accuracy25=numeric(0), Accuracy26=numeric(0), Accuracy27=numeric(0), Accuracy28=numeric(0), Accuracy29=numeric(0), Accuracy30=numeric(0))
Accuracy.SVM2 <- numeric(0)

for (s in 1:K) {
  svm <- svm(Name~., data=dataleaf[-flds[[s]], ], method="class")
  pred <- predict(svm, newdata=dataleaf[flds[[s]],], type="class")
  tab <- with(dataleaf[flds[[s]],], table(pred, Name))

  somma <- 0
  for (i in 1:30) {
    somma=somma + tab[i,i]
  }
  for (a in 1:30) {
    somma2 <- 0
    for (b in 1:30) {
      somma2 <- somma2 + tab[b,a]
    }
    Acc.Var.SVM2[s,a] <- round(tab[a,a]/somma2,4)
  }
  Accuracy.SVM2[s] <- round(somma/sum(tab),4)
}

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

Accuracy.SVM2[is.nan(Accuracy.SVM2)] <- 0
Acc.Var.SVM2[is.nan(Acc.Var.SVM2)] <- 0

Accuracy.SVM.cv2 <- mean(Accuracy.SVM2)
Acc.Var.SVM.cv2 <- numeric(0)
for (k in 1:30) {
  Acc.Var.SVM.cv2[k] <-  mean(Acc.Var.SVM2[ ,k])
}

Sing.Var.Accuracy2 <- data.frame(Tree=Acc.Var.Tree.cv2, RF=Acc.Var.RF.cv2, SVM=Acc.Var.SVM.cv2)

ConfrontoTOT <- data.frame(Tree=Accuracy.Tree.cv, Tree.Sicuro=Accuracy.Tree.cv2, RF=Accuracy.RF.cv, RF.Sicuro=Accuracy.RF.cv2, SVM=Accuracy.SVM.cv, SVM.Sicuro=Accuracy.SVM.cv2)
ConfrontoSINGLEVAR <- data.frame(Tree=Sing.Var.Accuracy[ ,1], Tree.Sicuro=Sing.Var.Accuracy2[ ,1], RF=Sing.Var.Accuracy[ ,2], RF.Sicuro=Sing.Var.Accuracy2[ ,2], SVM=Sing.Var.Accuracy[ ,3], SVM.Sicuro=Sing.Var.Accuracy2[ ,3])
View(ConfrontoTOT)
View(ConfrontoSINGLEVAR)

