arvore_dataset_1000 <- amostra_travis_1000
arvore_dataset_500 <- amostra_travis_500
arvore_dataset_50 <- amostra_travis_50
arvore_dataset_10 <- amostra_travis_10
arvore_dataset_5 <- amostra_travis_5
arvore_dataset_2 <- amostra_travis_2

trainIndex_total <- createDataPartition(travis_selecionado$build_successful, p=0.80, list=FALSE)
arvore_dataset.treino <- travis_selecionado[ trainIndex_total,]
arvore_dataset.teste <- travis_selecionado[-trainIndex_total,]

trainIndex_500 <- createDataPartition(arvore_dataset_500$build_successful, p=0.80, list=FALSE)
arvore_dataset_500.treino <- arvore_dataset_500[ trainIndex_500,]
arvore_dataset_500.teste <- arvore_dataset_500[-trainIndex_500,]

trainIndex_1000 <- createDataPartition(arvore_dataset_1000$build_successful, p=0.80, list=FALSE)
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

## Aplicação do dataset completo

tree_total = rpart(build_successful ~ ., data = arvore_dataset.treino, control = rpart.control(cp= 0.006354781), method="class")
predictions_arvore_total <- predict(tree_total, newdata = arvore_dataset.teste)
confusionMatrix(ifelse(predictions_arvore_total[,1] > 0.5, "false.", "true."), arvore_dataset.teste$build_successful, positive="true.")

folds_500 <- createFolds(arvore_dataset_500$build_successful, k=10)
folds_1000 <- createFolds(arvore_dataset_1000$build_successful, k=10)
folds_100 <- createFolds(arvore_dataset_100$build_successful, k=10)
folds_50 <- createFolds(arvore_dataset_50$build_successful, k=10)

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


tree_total = rpart(build_successful ~ ., data = arvore_dataset.treino, control = rpart.control(cp=0.00149092), method="class")

predictions_arvore_1000 <- predict(tree_1000, newdata = arvore_dataset_1000.teste)
confusionMatrix(ifelse(predictions_arvore_1000[,1] > 0.5, "false.", "true."), arvore_dataset_1000.teste$build_successful)

tree_1000 = rpart(build_successful ~ ., data = arvore_dataset_1000.treino, control = rpart.control(cp=0.00149092), method="class")
tree_50 = rpart(build_successful ~ ., data = arvore_dataset_50.treino, control = rpart.control(cp=0.00149092), method="class")
tree_100 = rpart(build_successful ~ ., data = arvore_dataset_100.treino, control = rpart.control(cp=0.00149092), method="class")


#################################################

t_tree <- trainControl(method="cv", number=10)
model <- train(build_successful~., data=arvore_dataset.treino, trControl=t_tree, method="rpart")
#model_nb <- train(build_successful~., data=arvore_dataset.treino, trControl=t_tree, method="nb")
predictions_arvore_total_cv <- predict(model, newdata = arvore_dataset.teste)
accuracy.meas(arvore_dataset.teste$build_successful, predictions_arvore_total_cv)
confusionMatrix(arvore_dataset.teste$build_successful, predict(model, arvore_dataset.teste), positive="true.")

###################################################

Undersampling

trainIndex_total_undersampling_100 <- createDataPartition(under_completo_100$build_successful, p=0.80, list=FALSE)
arvore_dataset_under_100.treino <- under_completo_100[ trainIndex_total_undersampling_100,]
arvore_dataset_under_100.teste <- under_completo_100[-trainIndex_total_undersampling_100,]
model_under_100 <- train(build_successful~., data=arvore_dataset_under_100.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_under_100.teste$build_successful, predict(model_under_100, arvore_dataset_under_100.teste), positive="true.")

trainIndex_total_undersampling_500 <- createDataPartition(under_completo_500$build_successful, p=0.80, list=FALSE)
arvore_dataset_under_500.treino <- under_completo_500[ trainIndex_total_undersampling_500,]
arvore_dataset_under_500.teste <- under_completo_500[-trainIndex_total_undersampling_500,]
model_under_500 <- train(build_successful~., data=arvore_dataset_under_500.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_500, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_under_500.teste$build_successful, predict(model_under_500, arvore_dataset_under_500.teste), positive="true.")

trainIndex_total_undersampling_50 <- createDataPartition(under_completo_50$build_successful, p=0.80, list=FALSE)
arvore_dataset_under_50.treino <- under_completo_50[ trainIndex_total_undersampling_50,]
arvore_dataset_under_50.teste <- under_completo_50[-trainIndex_total_undersampling_50,]
model_under_50 <- train(build_successful~., data=arvore_dataset_under_50.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_under_50.teste$build_successful, predict(model_under_50, arvore_dataset_under_50.teste), positive="true.")


Oversamplng

trainIndex_total_oversampling_500 <- createDataPartition(over_completo_500$build_successful, p=0.80, list=FALSE)
arvore_dataset_over_500.treino <- over_completo_500[ trainIndex_total_oversampling_500,]
arvore_dataset_over_500.teste <- over_completo_500[-trainIndex_total_oversampling_500,]
model_over_500 <- train(build_successful~., data=arvore_dataset_over_500.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_over_500.teste$build_successful, predict(model_over_500, arvore_dataset_over_500.teste), positive="true.")

trainIndex_total_oversampling_100 <- createDataPartition(over_completo_100$build_successful, p=0.80, list=FALSE)
arvore_dataset_over_100.treino <- over_completo_100[ trainIndex_total_oversampling_100,]
arvore_dataset_over_100.teste <- over_completo_100[-trainIndex_total_oversampling_100,]
model_over_100 <- train(build_successful~., data=arvore_dataset_over_100.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_over_100.teste$build_successful, predict(model_over_100, arvore_dataset_over_100.teste), positive="true.")

trainIndex_total_SMOTE_500 <- createDataPartition(smote_500$build_successful, p=0.80, list=FALSE)
arvore_dataset_smote_500.treino <- smote_500[ trainIndex_total_SMOTE_500,]
arvore_dataset_smote_500.teste <- smote_500[-trainIndex_total_SMOTE_500,]
model_smote_500 <- train(build_successful~., data=arvore_dataset_smote_500.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_smote_500.teste$build_successful, predict(model_smote_500, arvore_dataset_smote_500.teste), positive="true.")

trainIndex_total_SMOTE_100 <- createDataPartition(smote_100$build_successful, p=0.80, list=FALSE)
arvore_dataset_smote_100.treino <- smote_100[ trainIndex_total_SMOTE_100,]
arvore_dataset_smote_100.teste <- smote_100[-trainIndex_total_SMOTE_100,]
model_smote_100 <- train(build_successful~., data=arvore_dataset_smote_100.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_smote_100.teste$build_successful, predict(model_smote_100, arvore_dataset_smote_100.teste), positive="true.")

trainIndex_total_SMOTE_50 <- createDataPartition(smote_50$build_successful, p=0.80, list=FALSE)
arvore_dataset_smote_50.treino <- smote_50[ trainIndex_total_SMOTE_50,]
arvore_dataset_smote_50.teste <- smote_50[-trainIndex_total_SMOTE_50,]
model_smote_50 <- train(build_successful~., data=arvore_dataset_smote_50.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_smote_50.teste$build_successful, predict(model_smote_50, arvore_dataset_smote_50.teste), positive="true.")

trainIndex_total_ROSE_500 <- createDataPartition(rose_500$build_successful, p=0.80, list=FALSE)
arvore_dataset_rose_500.treino <- rose_500[ trainIndex_total_ROSE_500,]
arvore_dataset_rose_500.teste <- rose_500[-trainIndex_total_ROSE_500,]
model_rose_500 <- train(build_successful~., data=arvore_dataset_rose_500.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_rose_500.teste$build_successful, predict(model_rose_500, arvore_dataset_rose_500.teste), positive="true.")

trainIndex_total_ROSE_100 <- createDataPartition(rose_100$build_successful, p=0.80, list=FALSE)
arvore_dataset_rose_100.treino <- rose_100[ trainIndex_total_ROSE_100,]
arvore_dataset_rose_100.teste <- rose_100[-trainIndex_total_ROSE_100,]
model_rose_100 <- train(build_successful~., data=arvore_dataset_rose_100.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_rose_100.teste$build_successful, predict(model_rose_100, arvore_dataset_rose_100.teste), positive="true.")

trainIndex_total_ROSE_50 <- createDataPartition(rose_50$build_successful, p=0.80, list=FALSE)
arvore_dataset_rose_50.treino <- rose_50[ trainIndex_total_ROSE_50,]
arvore_dataset_rose_50.teste <- rose_50[-trainIndex_total_ROSE_50,]
model_rose_50 <- train(build_successful~., data=arvore_dataset_rose_50.treino, trControl=t_tree, method="rpart")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(arvore_dataset_rose_50.teste$build_successful, predict(model_rose_50, arvore_dataset_rose_50.teste), positive="true.")

# # CURVA ROC / PRECISION

accuracy.meas(arvore_dataset_under_500.teste$build_successful, predict(model_under_500, arvore_dataset_under_500.teste))
roc.curve(arvore_dataset_under_500.teste$build_successful, predict(model_under_500, arvore_dataset_under_500.teste))

accuracy.meas(arvore_dataset_under_100.teste$build_successful, predict(model_under_100, arvore_dataset_under_100.teste))
roc.curve(arvore_dataset_under_100.teste$build_successful, predict(model_under_100, arvore_dataset_under_100.teste))

accuracy.meas(arvore_dataset_under_50.teste$build_successful, predict(model_under_50, arvore_dataset_under_50.teste))
roc.curve(arvore_dataset_under_50.teste$build_successful, predict(model_under_50, arvore_dataset_under_50.teste))

accuracy.meas(arvore_dataset_over_500.teste$build_successful, predict(model_over_500, arvore_dataset_over_500.teste))
roc.curve(arvore_dataset_over_500.teste$build_successful, predict(model_over_500, arvore_dataset_over_500.teste))

accuracy.meas(arvore_dataset_over_100.teste$build_successful, predict(model_over_100, arvore_dataset_over_100.teste))
roc.curve(arvore_dataset_over_100.teste$build_successful, predict(model_over_100, arvore_dataset_over_100.teste))

accuracy.meas(arvore_dataset_over_50.teste$build_successful, predict(model_over_50, arvore_dataset_over_50.teste))
roc.curve(arvore_dataset_over_50.teste$build_successful, predict(model_over_50, arvore_dataset_over_50.teste))

accuracy.meas(arvore_dataset_smote_500.teste$build_successful, predict(model_smote_500, arvore_dataset_smote_500.teste))
roc.curve(arvore_dataset_smote_500.teste$build_successful, predict(model_smote_500, arvore_dataset_smote_500.teste))

accuracy.meas(arvore_dataset_smote_100.teste$build_successful, predict(model_smote_100, arvore_dataset_smote_100.teste))
roc.curve(arvore_dataset_smote_100.teste$build_successful, predict(model_smote_100, arvore_dataset_smote_100.teste))

accuracy.meas(arvore_dataset_smote_50.teste$build_successful, predict(model_smote_50, arvore_dataset_smote_50.teste))
roc.curve(arvore_dataset_smote_50.teste$build_successful, predict(model_smote_50, arvore_dataset_smote_50.teste))

accuracy.meas(arvore_dataset_rose_500.teste$build_successful, predict(model_rose_500, arvore_dataset_rose_500.teste))
roc.curve(arvore_dataset_rose_500.teste$build_successful, predict(model_rose_500, arvore_dataset_rose_500.teste))

accuracy.meas(arvore_dataset_rose_100.teste$build_successful, predict(model_rose_100, arvore_dataset_rose_100.teste))
roc.curve(arvore_dataset_rose_100.teste$build_successful, predict(model_rose_100, arvore_dataset_rose_100.teste))

accuracy.meas(arvore_dataset_rose_50.teste$build_successful, predict(model_rose_50, arvore_dataset_rose_50.teste))
roc.curve(arvore_dataset_rose_50.teste$build_successful, predict(model_rose_50, arvore_dataset_rose_50.teste))

accuracy.meas(arvore_dataset_100.teste$build_successful, predict(bagging, arvore_dataset_100.teste))
roc.curve(arvore_dataset_100.teste$build_successful, predict(bagging, arvore_dataset_100.teste))

accuracy.meas(arvore_dataset_100.teste$build_successful, predict(boosting_c50, arvore_dataset_100.teste))
roc.curve(arvore_dataset_100.teste$build_successful, predict(bagging, arvore_dataset_100.teste))

accuracy.meas(arvore_dataset_100.teste$build_successful, predict(boosting_c50, arvore_dataset_100.teste))
roc.curve(arvore_dataset_100.teste$build_successful, predict(boosting_c50, arvore_dataset_100.teste))
