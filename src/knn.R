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

