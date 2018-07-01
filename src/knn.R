knn_dataset_1000$gh_is_pr <- NULL
knn_dataset_1000$gh_lang <- NULL
knn_dataset_1000$gh_by_core_team_member <- NULL

knn_dataset_100$gh_is_pr <- NULL
knn_dataset_100$gh_lang <- NULL
knn_dataset_100$gh_by_core_team_member <- NULL

knn_dataset_500$gh_is_pr <- NULL
knn_dataset_500$gh_lang <- NULL
knn_dataset_500$gh_by_core_team_member <- NULL

knn_dataset_50$gh_is_pr <- NULL
knn_dataset_50$gh_lang <- NULL
knn_dataset_50$gh_by_core_team_member <- NULL

knn_dataset_100 <- nb_dataset_100
knn_dataset_1000 <- nb_dataset_1000
knn_dataset_500 <- nb_dataset_500
knn_dataset_50 <- nb_dataset_50

# # Aplicando no dataset inteiro

knn_dataset.treino <- as.data.frame(scale(nb_dataset_total.treino[,-19]))
knn_dataset.teste <- as.data.frame(scale(nb_dataset_total.teste[,-19]))
knn_total <- knn(knn_dataset.treino, knn_dataset.teste, nb_dataset_total.treino$build_successful, k=9)
confusionMatrix(knn_total, nb_dataset_total.teste$build_successful, positive="true.")

dataset_cv_500_knn_vd <- knn_500.treino[,19]
# dataset_cv_1000_knn[[i]] <- cbind(dataset_cv_1000_knn[[i]],knn_1000.teste[folds_1000_knn[[i]],19])
que_porra <- data.frame(dataset_cv_500_knn[[i]])
model_500_knn[[i]] <- knn(scale(knn_500.treino[,-19]), que_porra, dataset_cv_500_knn_vd$build_successful, k=9)

trainIndex_knn_1000 <- createDataPartition(knn_dataset_1000$build_successful, p=0.50, list=FALSE)
knn_1000.treino <-knn_dataset_1000[ trainIndex_knn_1000,]
knn_1000.teste <- knn_dataset_1000[-trainIndex_knn_1000,]

trainIndex_knn_100 <- createDataPartition(knn_dataset_100$build_successful, p=0.50, list=FALSE)
knn_100.treino <-knn_dataset_100[ trainIndex_knn_100,]
knn_100.teste <- knn_dataset_100[-trainIndex_knn_100,]

trainIndex_knn_500 <- createDataPartition(knn_dataset_500$build_successful, p=0.50, list=FALSE)
knn_500.treino <-knn_dataset_500[ trainIndex_knn_500,]
knn_500.teste <- knn_dataset_500[-trainIndex_knn_500,]

trainIndex_knn_50 <- createDataPartition(knn_dataset_50$build_successful, p=0.50, list=FALSE)
knn_50.treino <-knn_dataset_50[ trainIndex_knn_500,]
knn_50.teste <- knn_dataset_50[-trainIndex_knn_500,]


folds_500_knn <- createFolds(knn_500.teste$build_successful, k=10)
folds_1000_knn <- createFolds(knn_1000.teste$build_successful, k=10)
folds_100_knn <- createFolds(knn_100.teste$build_successful, k=10)
folds_50_knn <- createFolds(knn_50.teste$build_successful, k=10)

dataset_cv_1000_knn <- list()
dataset_cv_100_knn <- list()
dataset_cv_500_knn <- list()
dataset_cv_50_knn <- list()
dataset_cv_1000_knn_vd <- list()
dataset_cv_100_knn_vd <- list()
dataset_cv_500_knn_vd <- list()

model_1000_knn <- list()
model_100_knn <- list()
model_500_knn <- list()
model_50_knn <- list()

matrizes_confusao_100 <- list()
matrizes_confusao_1000 <- list()
matrizes_confusao_500 <- list()
matrizes_confusao_50 <- list()

for(i in 1:length(folds_1000_knn))
{
  dataset_cv_1000_knn[[i]] <- as.data.frame(scale(knn_1000.teste[folds_1000_knn[[i]],-19]))
  dataset_cv_1000_knn_vd <- knn_1000.treino[,19]
 # dataset_cv_1000_knn[[i]] <- cbind(dataset_cv_1000_knn[[i]],knn_1000.teste[folds_1000_knn[[i]],19])
  que_porra <- data.frame(dataset_cv_1000_knn[[i]])
  model_1000_knn[[i]] <- knn(scale(knn_1000.treino[,-19]), que_porra, dataset_cv_1000_knn_vd$build_successful, k=9)
}

confusionMatrix(knn_total, knn_dataset.treino$build_successful)
confusionMatrix(ifelse(knn_total[,1] > 0.5, "false.", "true."), knn_dataset.teste$build_successful)


for (i in length(folds_1000_knn))
{
  
  teste <- knn_1000.teste[folds_1000_knn[[i]],]
  matrizes_confusao_1000[[i]] <- as.list(confusionMatrix(model_1000_knn[[i]], teste$build_successful))
  
}

for(i in 1:length(folds_500_knn))
{
  dataset_cv_500_knn[[i]] <- as.data.frame(scale(knn_500.teste[folds_500_knn[[i]],-19]))
  dataset_cv_500_knn_vd <- knn_500.treino[,19]
  # dataset_cv_1000_knn[[i]] <- cbind(dataset_cv_1000_knn[[i]],knn_1000.teste[folds_1000_knn[[i]],19])
  que_porra <- data.frame(dataset_cv_500_knn[[i]])
  model_500_knn[[i]] <- knn(scale(knn_500.treino[,-19]), que_porra, dataset_cv_500_knn_vd$build_successful, k=9)
}

for (i in length(folds_500_knn))
{
  
  teste <- knn_500.teste[folds_500_knn[[i]],]
  matrizes_confusao_500[[i]] <- as.list(confusionMatrix(model_500_knn[[i]], teste$build_successful))
  
}


for(i in 1:length(folds_100_knn))
{
  dataset_cv_100_knn[[i]] <- as.data.frame(scale(knn_100.teste[folds_100_knn[[i]],-19]))
  dataset_cv_100_knn_vd <- knn_100.treino[,19]
  # dataset_cv_1000_knn[[i]] <- cbind(dataset_cv_1000_knn[[i]],knn_1000.teste[folds_1000_knn[[i]],19])
  que_porra <- data.frame(dataset_cv_100_knn[[i]])
  model_100_knn[[i]] <- knn(scale(knn_100.treino[,-19]), que_porra, dataset_cv_100_knn_vd$build_successful, k=9)
}

for (i in length(folds_100_knn))
{
  
  teste <- knn_100.teste[folds_100_knn[[i]],]
  matrizes_confusao_100[[i]] <- as.list(confusionMatrix(model_100_knn[[i]], teste$build_successful))
  
}

for(i in 1:length(folds_50_knn))
{
  dataset_cv_50_knn[[i]] <- as.data.frame(scale(knn_50.teste[folds_50_knn[[i]],-19]))
  dataset_cv_50_knn_vd <- knn_50.treino[,19]
  # dataset_cv_1000_knn[[i]] <- cbind(dataset_cv_1000_knn[[i]],knn_1000.teste[folds_1000_knn[[i]],19])
  que_porra <- data.frame(dataset_cv_50_knn[[i]])
  model_50_knn[[i]] <- knn(scale(knn_50.treino[,-19]), que_porra, dataset_cv_50_knn_vd$build_successful, k=9)
}

for (i in length(folds_100_knn))
{
  
  teste <- knn_100.teste[folds_100_knn[[i]],]
  matrizes_confusao_100[[i]] <- as.list(confusionMatrix(model_100_knn[[i]], teste$build_successful))
  
}

model_knn <- train(build_successful~., data=nb_dataset_total.treino, trControl=t_tree, method="knn")
confusionMatrix(nb_dataset_total.teste$build_successful, predict(model_knn, nb_dataset_total.teste), positive="true.")

smote_scaled <- scale(smote_500[,-19])
smote_scaled <- cbind(smote_scaled, smote_500[,19])
smote_scaled_100 <- scale(smote_100[,-19])
smote_scaled_100 <- as.data.frame(smote_scaled_100)
smote_scaled_100 <- cbind(smote_scaled_100, smote_100[,19])
smote_scaled_50 <- scale(smote_50[,-19])
smote_scaled_50 <- as.data.frame(smote_scaled_50)
smote_scaled_50 <- cbind(smote_scaled_50, smote_50[,19])

# # UnderSampling

trainIndex_total_undersampling_100 <- createDataPartition(under_100_scaled$build_successful, p=0.80, list=FALSE)
knn_dataset_under_100.treino <- under_100_scaled[ trainIndex_total_undersampling_100,]
knn_dataset_under_100.teste <- under_100_scaled[-trainIndex_total_undersampling_100,]
model_under_knn_100 <- train(build_successful~., data=knn_dataset_under_100.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_under_100.teste$build_successful, predict(model_under_knn_100, knn_dataset_under_100.teste), positive="true.")

trainIndex_total_undersampling_500 <- createDataPartition(under_500_scaled$build_successful, p=0.80, list=FALSE)
knn_dataset_under_500.treino <- under_500_scaled[ trainIndex_total_undersampling_500,]
knn_dataset_under_500.teste <- under_500_scaled[-trainIndex_total_undersampling_500,]
model_under_knn_500 <- train(build_successful~., data=knn_dataset_under_500.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_under_500.teste$build_successful, predict(model_under_knn_500, knn_dataset_under_500.teste), positive="true.")

trainIndex_total_undersampling_50 <- createDataPartition(under_50_scaled$build_successful, p=0.80, list=FALSE)
knn_dataset_under_50.treino <- under_50_scaled[ trainIndex_total_undersampling_50,]
knn_dataset_under_50.teste <- under_50_scaled[-trainIndex_total_undersampling_50,]
model_under_knn_50 <- train(build_successful~., data=knn_dataset_under_50.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_under_50.teste$build_successful, predict(model_under_knn_50, knn_dataset_under_50.teste), positive="true.")

# # Oversampling

trainIndex_total_oversampling_500 <- createDataPartition(over_scaled_500$build_successful, p=0.80, list=FALSE)
knn_dataset_over_500.treino <- over_scaled_500[ trainIndex_total_oversampling_500,]
knn_dataset_over_500.teste <- over_scaled_500[-trainIndex_total_oversampling_500,]
model_over_knn_500 <- train(build_successful~., data=knn_dataset_over_500.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_over_500.teste$build_successful, predict(model_over_knn_500, knn_dataset_over_500.teste), positive="true.")

trainIndex_total_oversampling_100 <- createDataPartition(over_scaled_100$build_successful, p=0.80, list=FALSE)
knn_dataset_over_100.treino <- over_scaled_100[ trainIndex_total_oversampling_100,]
knn_dataset_over_100.teste <- over_scaled_100[-trainIndex_total_oversampling_100,]
model_over_knn_100 <- train(build_successful~., data=knn_dataset_over_100.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_over_100.teste$build_successful, predict(model_over_knn_100, knn_dataset_over_100.teste), positive="true.")

trainIndex_total_oversampling_50 <- createDataPartition(over_scaled_50$build_successful, p=0.80, list=FALSE)
knn_dataset_over_50.treino <- over_scaled_50[ trainIndex_total_oversampling_50,]
knn_dataset_over_50.teste <- over_scaled_50[-trainIndex_total_oversampling_50,]
model_over_knn_50 <- train(build_successful~., data=knn_dataset_over_50.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_over_50.teste$build_successful, predict(model_over_knn_50, knn_dataset_over_50.teste), positive="true.")

# # SMOTE

trainIndex_total_scaled_500 <- createDataPartition(smote_scaled$build_successful, p=0.80, list=FALSE)
knn_dataset_smote_500.treino <- smote_scaled[ trainIndex_total_scaled_500,]
knn_dataset_smote_500.teste <- smote_scaled[-trainIndex_total_scaled_500,]
model_smote_knn_500 <- train(build_successful~., data=knn_dataset_smote_500.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_smote_500.teste$build_successful, predict(model_smote_knn_500, knn_dataset_smote_500.teste), positive="true.")


trainIndex_total_scaled_100 <- createDataPartition(smote_scaled_100$build_successful, p=0.80, list=FALSE)
knn_dataset_smote_100.treino <- smote_scaled_100[ trainIndex_total_scaled_100,]
knn_dataset_smote_100.teste <- smote_scaled_100[-trainIndex_total_scaled_100,]
model_smote_knn_100 <- train(build_successful~., data=knn_dataset_smote_100.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_smote_100.teste$build_successful, predict(model_smote_knn_100, knn_dataset_smote_100.teste), positive="true.")

trainIndex_total_scaled_50 <- createDataPartition(smote_scaled_50$build_successful, p=0.80, list=FALSE)
knn_dataset_smote_50.treino <- smote_scaled_50[ trainIndex_total_scaled_50,]
knn_dataset_smote_50.teste <- smote_scaled_50[-trainIndex_total_scaled_50,]
model_smote_knn_50 <- train(build_successful~., data=knn_dataset_smote_50.treino, trControl=t_tree, method="knn")
confusionMatrix(knn_dataset_smote_50.teste$build_successful, predict(model_smote_knn_50, knn_dataset_smote_50.teste), positive="true.")

trainIndex_total <- createDataPartition(travis_selecionado$build_successful, p=0.90, list=FALSE)
knn_dataset_smote_50.treino <- smote_scaled_50[ trainIndex_total_scaled_50,]
knn_dataset.teste <- travis_selecionado[-trainIndex_total,]
model_smote_knn <- train(build_successful~., data=knn_dataset_smote_50.treino, trControl=t_tree, method="knn")
confusionMatrix(scaled_total$build_successful, predict(model_smote_knn_500, scaled_total), positive="true.")

roc.curve(knn_dataset_smote_50.teste$build_successful, predict(model_smote_knn_50, knn_dataset_smote_50.teste))

# # ROSE

trainIndex_total_ROSE_500 <- createDataPartition(rose_scaled_500$build_successful, p=0.80, list=FALSE)
knn_dataset_rose_500.treino <- rose_scaled_500[ trainIndex_total_ROSE_500,]
knn_dataset_rose_500.teste <- rose_scaled_500[-trainIndex_total_ROSE_500,]
model_rose_500 <- train(build_successful~., data=knn_dataset_rose_500.treino, trControl=t_tree, method="knn")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(knn_dataset_rose_500.teste$build_successful, predict(model_rose_500, knn_dataset_rose_500.teste), positive="true.")

trainIndex_total_ROSE_100 <- createDataPartition(rose_scaled_100$build_successful, p=0.80, list=FALSE)
knn_dataset_rose_100.treino <- rose_scaled_100[ trainIndex_total_ROSE_100,]
knn_dataset_rose_100.teste <- rose_scaled_100[-trainIndex_total_ROSE_100,]
model_rose_100 <- train(build_successful~., data=knn_dataset_rose_100.treino, trControl=t_tree, method="knn")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(knn_dataset_rose_100.teste$build_successful, predict(model_rose_100, knn_dataset_rose_100.teste), positive="true.")

trainIndex_total_ROSE_50 <- createDataPartition(rose_scaled_50$build_successful, p=0.80, list=FALSE)
knn_dataset_rose_50.treino <- rose_scaled_50[ trainIndex_total_ROSE_50,]
knn_dataset_rose_50.teste <- rose_scaled_50[-trainIndex_total_ROSE_50,]
model_rose_50 <- train(build_successful~., data=knn_dataset_rose_50.treino, trControl=t_tree, method="knn")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(knn_dataset_rose_50.teste$build_successful, predict(model_rose_50, knn_dataset_rose_50.teste), positive="true.")

# # ROC / PRECISION / RECALL ROSE

accuracy.meas(knn_dataset_rose_500.teste$build_successful, predict(model_rose_500, knn_dataset_rose_500.teste))
roc.curve(knn_dataset_rose_500.teste$build_successful, predict(model_rose_500, knn_dataset_rose_500.teste))

accuracy.meas(knn_dataset_rose_100.teste$build_successful, predict(model_rose_100, knn_dataset_rose_100.teste))
roc.curve(knn_dataset_rose_100.teste$build_successful, predict(model_rose_100, knn_dataset_rose_100.teste))

accuracy.meas(knn_dataset_rose_50.teste$build_successful, predict(model_rose_50, knn_dataset_rose_50.teste))
roc.curve(knn_dataset_rose_50.teste$build_successful, predict(model_rose_50, knn_dataset_rose_50.teste))

# # ROC / PRECISION / RECALL SMOTE

accuracy.meas(knn_dataset_smote_500.teste$build_successful, predict(model_smote_knn_500, knn_dataset_smote_500.teste))
roc.curve(knn_dataset_smote_500.teste$build_successful, predict(model_smote_knn_500, knn_dataset_smote_500.teste))

accuracy.meas(knn_dataset_smote_100.teste$build_successful, predict(model_smote_knn_100, knn_dataset_smote_100.teste))
roc.curve(knn_dataset_smote_100.teste$build_successful, predict(model_smote_knn_100, knn_dataset_smote_100.teste))

accuracy.meas(knn_dataset_smote_50.teste$build_successful, predict(model_smote_knn_50, knn_dataset_smote_50.teste))
roc.curve(knn_dataset_smote_50.teste$build_successful, predict(model_smote_knn_50, knn_dataset_smote_50.teste))

# # ROC / PRECISION / RECALL OVER

accuracy.meas(knn_dataset_over_500.teste$build_successful, predict(model_over_knn_500, knn_dataset_over_500.teste))
roc.curve(knn_dataset_over_500.teste$build_successful, predict(model_over_knn_500, knn_dataset_over_500.teste))

accuracy.meas(knn_dataset_over_100.teste$build_successful, predict(model_over_knn_100, knn_dataset_over_100.teste))
roc.curve(knn_dataset_over_100.teste$build_successful, predict(model_over_knn_100, knn_dataset_over_100.teste))

accuracy.meas(knn_dataset_over_50.teste$build_successful, predict(model_over_knn_50, knn_dataset_over_50.teste))
roc.curve(knn_dataset_over_50.teste$build_successful, predict(model_over_knn_50, knn_dataset_over_50.teste))

# # ROC / PRECISION / RECALL UNDER

accuracy.meas(knn_dataset_under_500.teste$build_successful, predict(model_under_knn_500, knn_dataset_under_500.teste))
roc.curve(knn_dataset_under_500.teste$build_successful, predict(model_under_knn_500, knn_dataset_under_500.teste))

accuracy.meas(knn_dataset_under_100.teste$build_successful, predict(model_under_knn_100, knn_dataset_under_100.teste))
roc.curve(knn_dataset_under_100.teste$build_successful, predict(model_under_knn_100, knn_dataset_under_100.teste))

accuracy.meas(knn_dataset_under_50.teste$build_successful, predict(model_under_knn_50, knn_dataset_under_50.teste))
roc.curve(knn_dataset_under_50.teste$build_successful, predict(model_under_knn_50, knn_dataset_under_50.teste))

# # ROC / PRECISION / RECALL COMPLETO

accuracy.meas(nb_dataset_total.teste$build_successful, predict(model_knn, nb_dataset_total.teste))
roc.curve(nb_dataset_total.teste$build_successful, predict(model_knn, nb_dataset_total.teste))
