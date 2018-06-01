# k-fold cross validation
trainControl <- trainControl(method="cv", number=10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(arvore_dataset_10.treino$build_successful))

trainControl_100 <- trainControl(method="cv", number=10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(arvore_dataset_100.treino$build_successful))

# Lista de algoritmos que serão utilizados
algorithmList <- c('rpart', 'knn', 'nb')

# Cria os modelos da lista
models <- caretList(build_successful~., data=arvore_dataset_10.treino, trControl=trainControl, methodList=algorithmList)
models_100 <- caretList(build_successful~., data=arvore_dataset_100.treino, trControl=trainControl_100, methodList=algorithmList)

# Resultado (precisamos passar um list de modelos treinados com Train)
results <- resamples(models)

splom(results)

stackControl <- trainControl(method="cv", number=10, classProbs=TRUE, savePredictions=TRUE)
stack.rf <- caretStack(models, method="nb", metric="Accuracy", trControl=stackControl)
stack.nb_100 <- caretStack(models_100, method="nb", metric="Accuracy", trControl=stackControl)
stack.knn <- caretStack(models, method="knn", metric="Accuracy", trControl=stackControl)

predictionsRP <- predict(stack.rpart, newdata = as.data.frame(arvore_dataset_10.teste))
accuracy.meas(arvore_dataset_10.teste$build_successful, predictionsRP)
roc.curve(arvore_dataset_10.teste$build_successful, predictionsRP)

predictionsKNN <- predict(stack.knn, newdata = as.data.frame(arvore_dataset_10.teste))
accuracy.meas(arvore_dataset_10.teste$build_successful, predictionsKNN)
roc.curve(arvore_dataset_10.teste$build_successful, predictionsKNN)

predictionsNB <- predict(stack.rf, newdata = as.data.frame(arvore_dataset_10.teste))
accuracy.meas(arvore_dataset_10.teste$build_successful, predictionsNB)
roc.curve(arvore_dataset_10.teste$build_successful, predictionsNB)

predictionsNB_100 <- predict(stack.nb_100, newdata = as.data.frame(arvore_dataset_100.teste))
accuracy.meas(arvore_dataset_100.teste$build_successful, predictionsNB_100)
roc.curve(arvore_dataset_100.teste$build_successful, predictionsNB_100)

# # BAGGING 

parametro_controle <- trainControl(method="repeatedcv", number=10, repeats=3)

set.seed(100)
bagging <- train(build_successful~., data=arvore_dataset_100, method="treebag", metric="Accuracy", trControl=parametro_controle)
bagging_rf <- train(build_successful~., data=arvore_dataset_100, method="rf", metric="Accuracy", trControl=parametro_controle)

# # BOOSTING

boosting_c50 <- train(build_successful~., data=arvore_dataset_100, method="C5.0", metric="Accuracy", trControl=parametro_controle)

# # Curva ROC de Bagging e Boosting

predictions_boosting <- predict(boosting_c50, newdata = as.data.frame(arvore_dataset_100.teste))
accuracy.meas(arvore_dataset_100.teste$build_successful, predictions_boosting)
roc.curve(arvore_dataset_100.teste$build_successful, predictions_boosting)

predictions_bagging <- predict(bagging, newdata = as.data.frame(arvore_dataset_100.teste))
accuracy.meas(arvore_dataset_100.teste$build_successful, predictions_bagging)
roc.curve(arvore_dataset_100.teste$build_successful, predictions_bagging)
