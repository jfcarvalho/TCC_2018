nb_dataset_total.treino <- arvore_dataset.treino
nb_dataset_total.teste <- arvore_dataset.teste

nb_dataset_total.teste$gh_is_pr<- NULL
nb_dataset_total.teste$gh_by_core_team_member <- NULL
nb_dataset_total.treino$gh_is_pr <- NULL
nb_dataset_total.treino$gh_by_core_team_member <- NULL

nb_dataset_50 <- arvore_dataset_50

nb_dataset_1000$gh_lang <- as.factor(nb_dataset_1000$gh_lang)
nb_dataset_1000$gh_is_pr <- as.factor(nb_dataset_1000$gh_is_pr)
nb_dataset_1000$gh_by_core_team_member <- as.factor(nb_dataset_1000$gh_by_core_team_member)

nb_dataset_500$gh_lang <- as.factor(nb_dataset_500$gh_lang)
nb_dataset_500$gh_is_pr <- as.factor(nb_dataset_500$gh_is_pr)
nb_dataset_500$gh_by_core_team_member <- as.factor(nb_dataset_500$gh_by_core_team_member)

nb_dataset_50$gh_lang <- as.factor(nb_dataset_50$gh_lang)
nb_dataset_50$gh_is_pr <- as.factor(nb_dataset_50$gh_is_pr)
nb_dataset_50$gh_by_core_team_member <- as.factor(nb_dataset_50$gh_by_core_team_member)

nb_dataset_100.treino <- nb_dataset_100[trainIndex_100,]
nb_dataset_100.teste <- nb_dataset_100[-trainIndex_100,]

nb_dataset_1000.treino <- nb_dataset_1000[trainIndex_1000,]
nb_dataset_1000.teste <- nb_dataset_1000[-trainIndex_1000,]

nb_dataset_500.treino <- nb_dataset_500[trainIndex_500,]
nb_dataset_500.teste <- nb_dataset_500[-trainIndex_500,]

nb_dataset_50.treino <- nb_dataset_50[trainIndex_50,]
nb_dataset_50.teste <- nb_dataset_50[-trainIndex_50,]

folds_500_nb <- createFolds(nb_dataset_500$build_successful, k=10)
folds_1000_nb <- createFolds(nb_dataset_1000$build_successful, k=10)
folds_100_nb <- createFolds(nb_dataset_100$build_successful, k=10)
folds_50_nb <- createFolds(nb_dataset_50$build_successful, k=10)

k <- 10

dataset_cv_500_nb <- list()
predictions_cv_500_nb <- list()
dataset_cv_1000_nb <- list()
predictions_cv_1000_nb <- list()
dataset_cv_100_nb <- list()
predictions_cv_100_nb <- list()
dataset_cv_50_nb <- list()
predictions_cv_50_nb <- list()

lista_preditores_nb_500 <- list()
lista_preditores_nb_1000 <- list()
lista_preditores_nb_100 <- list()

## Aplicando algoritmo no dataset todo

nb_total <- NaiveBayes(build_successful~., data=nb_dataset_total.treino, usekernel=TRUE)
predictions_nb <- predict(nb_total, nb_dataset_total.teste)
confusionMatrix(predictions_nb$class, nb_dataset_total.teste$build_successful)

model_nb_100 <- NaiveBayes(build_successful~., data=nb_dataset_100.treino)
predictions_100 <- predict(model_nb_100, folds_100_nb)
confusionMatrix(predictions$class, nb_dataset_100.teste$build_successful)

model_nb_1000 <- NaiveBayes(build_successful~., data=nb_dataset_1000.treino)
predictions_1000 <- predict(model_nb_1000, nb_dataset_1000.teste[,-22])
confusionMatrix(predictions_1000$class, nb_dataset_1000.teste$build_successful)

model_nb_500 <- NaiveBayes(build_successful~., data=nb_dataset_500.treino)
predictions_500 <- predict(model_nb_500, nb_dataset_500.teste[,-22])
confusionMatrix(predictions_500$class, nb_dataset_500.teste$build_successful)

model_nb_50 <- NaiveBayes(build_successful~., data=nb_dataset_50.treino)
predictions_50 <- predict(model_nb_50, nb_dataset_50.teste[,-22])
confusionMatrix(predictions_50$class, nb_dataset_50.teste$build_successful)

for(i in 1:length(folds_500_nb))
{
  dataset_cv_500_nb[[i]] <- as.data.frame(nb_dataset_500[folds_500_nb[[i]],])
  predictions_cv_500_nb[[i]] <- as.matrix(predict(model_nb_500, newdata = dataset_cv_500_nb[[i]]))
  #confusionMatrix(ifelse(predictions_cv_500_nb[[i]][,1]$posterior > 0.5, "false.", "true."), dataset_cv_500_nb[[i]]$build_successful)
}


for(i in 1:length(folds_500_nb)) {
  lista_preditores_nb_500[[i]] <- predictions_cv_500_nb[[i]][,1]$posterior
  confusionMatrix(ifelse(lista_preditores_nb_500[[i]][,1] > 0.5, "false.", "true."), dataset_cv_500_nb[[i]]$build_successful)
  
}


for(i in 1:length(folds_1000_nb))
{
  dataset_cv_1000_nb[[i]] <- as.data.frame(nb_dataset_1000[folds_1000_nb[[i]],])
  predictions_cv_1000_nb[[i]] <- as.matrix(predict(model_nb_1000, newdata = dataset_cv_1000_nb[[i]]))
  #confusionMatrix(ifelse(predictions_cv_1000[[i]][,1] > 0.5, "false.", "true."), dataset_cv_1000[[i]]$build_successful)
}

for(i in 1:length(folds_1000_nb)) {
  lista_preditores_nb_1000[[i]] <- predictions_cv_1000_nb[[i]][,1]$posterior
  confusionMatrix(ifelse(lista_preditores_nb_1000[[i]][,1] > 0.5, "false.", "true."), dataset_cv_1000_nb[[i]]$build_successful)
  
}


for(i in 1:length(folds_100_nb))
{
  dataset_cv_100_nb[[i]] <- as.data.frame(nb_dataset_100[folds_100_nb[[i]],])
  predictions_cv_100_nb[[i]] <- as.matrix(predict(model_nb_100, newdata = dataset_cv_100_nb[[i]]))
  #confusionMatrix(ifelse(predictions_cv_100[[i]][,1] > 0.5, "false.", "true."), dataset_cv_100[[i]]$build_successful)
}

for(i in 1:length(folds_100_nb)) {
  lista_preditores_nb_100[[i]] <- predictions_cv_100_nb[[i]][,1]$posterior
  confusionMatrix(ifelse(lista_preditores_nb_100[[i]][,1] > 0.5, "false.", "true."), dataset_cv_100_nb[[i]]$build_successful)
  
}


for(i in 1:length(folds_50))
{
  dataset_cv_50[[i]] <- as.data.frame(arvore_dataset_50[folds_50[[i]],])
  #dataset_cv_50[[i]] <- SMOTE(build_successful ~., dataset_cv_50[[i]], perc.over = 100, perc.under=200)
  predictions_cv_50[[i]] <- as.matrix(predict(tree_50, newdata = dataset_cv_50[[i]]))
  confusionMatrix(ifelse(predictions_cv_50[[i]][,1] > 0.5, "false.", "true."), dataset_cv_50[[i]]$build_successful)
}

# # Cross Validation

model_nb <- train(build_successful~., data=nb_dataset_total.treino, trControl=t_tree, method="nb")
predictions_nb_total_cv <- predict(model_nb, newdata = nb_dataset_total.teste)
confusionMatrix(nb_dataset_total.teste$build_successful, predict(model_nb, nb_dataset_total.teste), positive="true.")

Undersampling

trainIndex_total_undersampling_100 <- createDataPartition(under_completo_100$build_successful, p=0.80, list=FALSE)
nb_dataset_under_100.treino <- under_completo_100[ trainIndex_total_undersampling_100,]
nb_dataset_under_100.teste <- under_completo_100[-trainIndex_total_undersampling_100,]
model_under_nb_100 <- train(build_successful~., data=nb_dataset_under_100.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_under_100.teste$build_successful, predict(model_under_nb_100, nb_dataset_under_100.teste), positive="true.")

trainIndex_total_undersampling_500 <- createDataPartition(under_completo_500$build_successful, p=0.80, list=FALSE)
nb_dataset_under_500.treino <- under_completo_500[ trainIndex_total_undersampling_500,]
nb_dataset_under_500.teste <- under_completo_500[-trainIndex_total_undersampling_500,]
model_under_nb_500 <- train(build_successful~., data=nb_dataset_under_500.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_500, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_under_500.teste$build_successful, predict(model_under_nb_500, nb_dataset_under_500.teste), positive="true.")

trainIndex_total_undersampling_50 <- createDataPartition(under_completo_50$build_successful, p=0.80, list=FALSE)
nb_dataset_under_50.treino <- under_completo_50[ trainIndex_total_undersampling_50,]
nb_dataset_under_50.teste <- under_completo_50[-trainIndex_total_undersampling_50,]
model_under_50 <- train(build_successful~., data=nb_dataset_under_50.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_under_50.teste$build_successful, predict(model_under_50, nb_dataset_under_50.teste), positive="true.")

trainIndex_total_oversampling_500 <- createDataPartition(over_completo_500$build_successful, p=0.80, list=FALSE)
nb_dataset_over_500.treino <- over_completo_500[ trainIndex_total_oversampling_500,]
nb_dataset_over_500.teste <- over_completo_500[-trainIndex_total_oversampling_500,]
model_over_500 <- train(build_successful~., data=nb_dataset_over_500.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_over_500.teste$build_successful, predict(model_over_500, nb_dataset_over_500.teste), positive="true.")

trainIndex_total_oversampling_100 <- createDataPartition(over_completo_100$build_successful, p=0.80, list=FALSE)
nb_dataset_over_100.treino <- over_completo_100[ trainIndex_total_oversampling_100,]
nb_dataset_over_100.teste <- over_completo_100[-trainIndex_total_oversampling_100,]
model_over_100 <- train(build_successful~., data=nb_dataset_over_100.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_over_100.teste$build_successful, predict(model_over_100, nb_dataset_over_100.teste), positive="true.")

trainIndex_total_oversampling_50 <- createDataPartition(over_completo_50$build_successful, p=0.80, list=FALSE)
nb_dataset_over_50.treino <- over_completo_50[ trainIndex_total_oversampling_50,]
nb_dataset_over_50.teste <- over_completo_50[-trainIndex_total_oversampling_50,]
model_over_50 <- train(build_successful~., data=nb_dataset_over_50.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_over_50.teste$build_successful, predict(model_over_50, nb_dataset_over_50.teste), positive="true.")

trainIndex_total_SMOTE_500 <- createDataPartition(smote_500$build_successful, p=0.80, list=FALSE)
nb_dataset_smote_500.treino <- smote_500[ trainIndex_total_SMOTE_500,]
nb_dataset_smote_500.teste <- smote_500[-trainIndex_total_SMOTE_500,]
model_smote_500 <- train(build_successful~., data=nb_dataset_smote_500.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_smote_500.teste$build_successful, predict(model_smote_500, nb_dataset_smote_500.teste), positive="true.")

trainIndex_total_SMOTE_100 <- createDataPartition(smote_100$build_successful, p=0.80, list=FALSE)
nb_dataset_smote_100.treino <- smote_100[ trainIndex_total_SMOTE_100,]
nb_dataset_smote_100.teste <- smote_100[-trainIndex_total_SMOTE_100,]
model_smote_100 <- train(build_successful~., data=nb_dataset_smote_100.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_smote_100.teste$build_successful, predict(model_smote_100, nb_dataset_smote_100.teste), positive="true.")

trainIndex_total_SMOTE_50 <- createDataPartition(smote_50$build_successful, p=0.80, list=FALSE)
nb_dataset_smote_50.treino <- smote_50[ trainIndex_total_SMOTE_50,]
nb_dataset_smote_50.teste <- smote_50[-trainIndex_total_SMOTE_50,]
model_smote_50 <- train(build_successful~., data=nb_dataset_smote_50.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_smote_50.teste$build_successful, predict(model_smote_50, nb_dataset_smote_50.teste), positive="true.")

trainIndex_total_ROSE_500 <- createDataPartition(rose_500$build_successful, p=0.80, list=FALSE)
nb_dataset_ROSE_500.treino <- rose_500[ trainIndex_total_ROSE_500,]
nb_dataset_ROSE_500.teste <- rose_500[-trainIndex_total_ROSE_500,]
model_rose_500 <- train(build_successful~., data=nb_dataset_ROSE_500.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_ROSE_500.teste$build_successful, predict(model_rose_500, nb_dataset_ROSE_500.teste), positive="true.")

trainIndex_total_ROSE_100 <- createDataPartition(rose_100$build_successful, p=0.80, list=FALSE)
nb_dataset_ROSE_100.treino <- rose_100[ trainIndex_total_ROSE_100,]
nb_dataset_ROSE_100.teste <- rose_100[-trainIndex_total_ROSE_100,]
model_rose_100 <- train(build_successful~., data=nb_dataset_ROSE_100.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_ROSE_100.teste$build_successful, predict(model_rose_100, nb_dataset_ROSE_100.teste), positive="true.")

trainIndex_total_ROSE_50 <- createDataPartition(rose_50$build_successful, p=0.80, list=FALSE)
nb_dataset_ROSE_50.treino <- rose_50[ trainIndex_total_ROSE_50,]
nb_dataset_ROSE_50.teste <- rose_50[-trainIndex_total_ROSE_50,]
model_rose_50 <- train(build_successful~., data=nb_dataset_ROSE_50.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset_ROSE_50.teste$build_successful, predict(model_rose_50, nb_dataset_ROSE_50.teste), positive="true.")

trainIndex_total <- createDataPartition(travis_selecionado$build_successful, p=0.90, list=FALSE)
nb_dataset.treino <- travis_selecionado[ trainIndex_total,]
nb_dataset.teste <- travis_selecionado[-trainIndex_total,]
model_rose_50 <- train(build_successful~., data=nb_dataset_ROSE_50.treino, trControl=t_tree, method="nb")
predictions_arvore_under_cv <- predict(model_under_50, newdata = arvore_dataset_under.teste)
confusionMatrix(nb_dataset.treino$build_successful, predict(model_rose_500, nb_dataset.treino), positive="true.")
