nb_dataset_1000 <- arvore_dataset_1000
nb_dataset_500 <- arvore_dataset_500
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
