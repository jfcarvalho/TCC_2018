arvore_dataset_1000 <- amostra_travis_1000
arvore_dataset_500 <- amostra_travis_500
arvore_dataset_50 <- amostra_travis_50
arvore_dataset_10 <- amostra_travis_10
arvore_dataset_5 <- amostra_travis_5
arvore_dataset_2 <- amostra_travis_2

trainIndex_100 <- createDataPartition(arvore_dataset_100$build_successful, p=0.50, list=FALSE)
arvore_dataset_100.treino <- arvore_dataset_100[ trainIndex_100,]
arvore_dataset_100.teste <- arvore_dataset_100[-trainIndex_100,]

trainIndex_500 <- createDataPartition(arvore_dataset_500$build_successful, p=0.50, list=FALSE)
arvore_dataset_500.treino <- arvore_dataset_500[ trainIndex_500,]
arvore_dataset_500.teste <- arvore_dataset_500[-trainIndex_500,]

trainIndex_1000 <- createDataPartition(arvore_dataset_1000$build_successful, p=0.50, list=FALSE)
arvore_dataset_1000.treino <- arvore_dataset_1000[ trainIndex_1000,]
arvore_dataset_1000.teste <- arvore_dataset_1000[-trainIndex_1000,]

trainIndex_50 <- createDataPartition(arvore_dataset_50$build_successful, p=0.50, list=FALSE)
arvore_dataset_50.treino <- arvore_dataset_50[ trainIndex_50,]
arvore_dataset_50.teste <- arvore_dataset_50[-trainIndex_50,]

trainIndex_10 <- createDataPartition(arvore_dataset_10$build_successful, p=0.80, list=FALSE)
arvore_dataset_10.treino <- arvore_dataset_10[ trainIndex_10,]
arvore_dataset_10.teste <- arvore_dataset_10[-trainIndex_10,]

trainIndex_5 <- createDataPartition(arvore_dataset_5$build_successful, p=0.80, list=FALSE)
arvore_dataset_5.treino <- arvore_dataset_10[ trainIndex_10,]
arvore_dataset_5.teste <- arvore_dataset_10[-trainIndex_10,]

trainIndex_2 <- createDataPartition(arvore_dataset_2$build_successful, p=0.80, list=FALSE)
arvore_dataset_2.treino <- arvore_dataset_2[ trainIndex_2,]
arvore_dataset_2.teste <- arvore_dataset_2[-trainIndex_2,]

tree_100 = rpart(build_successful ~ ., data = arvore_dataset_100.treino, control = rpart.control(cp=0.00149092), method="class")
predictions_arvore_100 <- predict(tree_100, newdata = arvore_dataset_100.teste)
confusionMatrix(ifelse(predictions_arvore_100[,1] > 0.5, "false.", "true."), arvore_dataset_100.teste$build_successful)

folds_500 <- createFolds(arvore_dataset_500, k=10, returnTrain = TRUE)
folds_1000 <- createFolds(arvore_dataset_1000, k=10, returnTrain = TRUE)
folds_100 <- createFolds(arvore_dataset_100, k=10, returnTrain = TRUE)
folds_50 <- createFolds(arvore_dataset_50, k=10, returnTrain = TRUE)

k <- 10
dataset_cv_500 <- list()
predictions_cv_500 <- list()
dataset_cv_1000 <- list()
predictions_cv_1000 <- list()
dataset_cv_100 <- list()
predictions_cv_100 <- list()
dataset_cv_50 <- list()
predictions_cv_50 <- list()


for(i in 1:length(folds_500))
{
  dataset_cv_500[[i]] <- as.data.frame(arvore_dataset_500[folds_500[[i]],])
  predictions_cv_500[[i]] <- as.matrix(predict(tree_500, newdata = dataset_cv_500[[i]]))
  confusionMatrix(ifelse(predictions_cv_500[[i]][,1] > 0.5, "false.", "true."), dataset_cv_500[[i]]$build_successful)
}

#tree_500 = rpart(build_successful ~ ., data = arvore_dataset_500.treino, control = rpart.control(cp=0.00149092), method="class")
#predictions_arvore_500_f8 <- predict(tree_500, newdata = arvore_dataset_500[folds_500$Fold8,])
#confusionMatrix(ifelse(predictions_arvore_500_f8[,1] > 0.5, "false.", "true."), arvore_dataset_500[folds_500$Fold8,]$build_successful)

for(i in 1:length(folds_1000))
{
  dataset_cv_1000[[i]] <- as.data.frame(arvore_dataset_1000[folds_1000[[i]],])
  predictions_cv_1000[[i]] <- as.matrix(predict(tree_1000, newdata = dataset_cv_1000[[i]]))
  confusionMatrix(ifelse(predictions_cv_1000[[i]][,1] > 0.5, "false.", "true."), dataset_cv_1000[[i]]$build_successful)
}

for(i in 1:length(folds_100))
{
  dataset_cv_100[[i]] <- as.data.frame(arvore_dataset_100[folds_100[[i]],])
  predictions_cv_100[[i]] <- as.matrix(predict(tree_100, newdata = dataset_cv_100[[i]]))
  confusionMatrix(ifelse(predictions_cv_100[[i]][,1] > 0.5, "false.", "true."), dataset_cv_100[[i]]$build_successful)
}

for(i in 1:length(folds_50))
{
  dataset_cv_50[[i]] <- as.data.frame(arvore_dataset_50[folds_50[[i]],])
  dataset_cv_50[[i]] <- SMOTE(build_successful ~., dataset_cv_50[[i]], perc.over = 100, perc.under=200)
  predictions_cv_50[[i]] <- as.matrix(predict(tree_50, newdata = dataset_cv_50[[i]]))
  confusionMatrix(ifelse(predictions_cv_50[[i]][,1] > 0.5, "false.", "true."), dataset_cv_50[[i]]$build_successful)
}


tree_1000 = rpart(build_successful ~ ., data = arvore_dataset_1000.treino, control = rpart.control(cp=0.00149092), method="class")

predictions_arvore_1000 <- predict(tree_1000, newdata = arvore_dataset_1000.teste)
confusionMatrix(ifelse(predictions_arvore_1000[,1] > 0.5, "false.", "true."), arvore_dataset_1000.teste$build_successful)

tree_1000 = rpart(build_successful ~ ., data = arvore_dataset_1000.treino, control = rpart.control(cp=0.00149092), method="class")
tree_50 = rpart(build_successful ~ ., data = arvore_dataset_50.treino, control = rpart.control(cp=0.00149092), method="class")
tree_100 = rpart(build_successful ~ ., data = arvore_dataset_100.treino, control = rpart.control(cp=0.00149092), method="class")
