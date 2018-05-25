# k-fold cross validation
trainControl <- trainControl(method="cv", number=10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(arvore_dataset_10.treino$build_successful))

# Lista de algoritmos que serão utilizados
algorithmList <- c('rpart', 'knn', 'nb')

# Cria os modelos da lista
models <- caretList(build_successful~., data=arvore_dataset_10.treino, trControl=trainControl, methodList=algorithmList)

# Resultado (precisamos passar um list de modelos treinados com Train)
results <- resamples(models)

splom(results)

stackControl <- trainControl(method="cv", number=10, classProbs=TRUE, savePredictions=TRUE)
stack.rf <- caretStack(models, method="nb", metric="Accuracy", trControl=stackControl)
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
