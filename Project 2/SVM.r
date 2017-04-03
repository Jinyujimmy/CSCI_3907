library(e1071)
library(caret)

wine.cols <- c('fixed_acidity', 'volatile_acidity', 'citric_acid', 'residual_sugar', 'chlorids', 'free_sulfur_dioxide', 'total_sulfur_dioxide', 'density', 'pH', 'sulphates', 'alcohol', 'quality')

wine.white <- read.csv('../winequality-white.csv',  sep=";", skip = 1, header=F)
wine.red <- read.csv('../winequality-red.csv',  sep=";", skip =1, header=F)

names(wine.white) <- wine.cols
names(wine.red) <- wine.cols

wine.white['type'] <- 1
wine.red['type'] <- 2

wines <- rbind(wine.white, wine.red)
wines$type <- factor(wines$type, labels=c('white', 'red'))

train_test_split <- function(dataset, ratio) {
  set.seed(101)
  no_of_samples <- floor(ratio*nrow(dataset))
  sample <- sample.int(nrow(dataset), no_of_samples, replace = FALSE)
  data <- list()
  data$full <- dataset
  data$train <- dataset[sample, ]
  data$test <- dataset[-sample, ]
  return(data)
}

train_test <- train_test_split(wines, 0.6)

mymodel<- svm(type~.,data=train_test$train)

summary(mymodel)

prediction <- predict(mymodel, train_test$test)

plot(prediction)

train_test$test$prediction <- prediction

test <- train_test$test

confusionMatrix(test$prediction, test$type)

as.table(confusionMatrix(test$prediction, test$type))

plot(as.table(confusionMatrix(test$prediction, test$type)))

fourfoldplot(as.table(confusionMatrix(test$prediction, test$type)))