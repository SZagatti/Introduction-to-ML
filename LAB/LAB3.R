require(tree)
require(randomForest)
require(caret)
require(e1071)
set.seed(123)
iris <- iris
iris2 <- iris
training <- sample(1:nrow(iris), 80)
iris <- iris[3:5]
Binary <- iris[ ,3]
Binary <- as.character(Binary)
Binary[1:50]="Virginica/Setosa"
Binary[51:100]="Versicolor"
Binary[101:150]="Virginica/Setosa"
Binary <- as.factor(Binary)
iris <- iris[1:2]
iris <- data.frame(iris,Binary)

iris.svm <- svm(Binary~., iris, subset = training)
plot(iris.svm, iris, Petal.Length~Petal.Width)
plot(iris.svm, iris[-training,], Petal.Length~Petal.Width)
svm.pred <- predict(iris.svm, iris[-training,], type="class")
with(iris[-training,], table(svm.pred, Binary))


Binary2 <- iris2[ ,5]
Binary2 <- as.character(Binary2)
Binary2[1:50]="Virginica/Setosa"
Binary2[51:100]="Versicolor"
Binary2[101:150]="Virginica/Setosa"
Binary2 <- as.factor(Binary2)
iris2 <- iris2[1:4]
iris2 <- data.frame(iris2,Binary2)
iris.svm2 <- svm(Binary~., iris2, subset = training)
svm.pred2 <- predict(iris.svm2, iris2[-training,], type="class")
with(iris2[-training,], table(svm.pred2, Binary2))