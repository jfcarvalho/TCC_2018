# k-fold cross validation
trainControl <- trainControl(method="cv", number=10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(arvore_dataset_2.treino$build_successful))

# Lista de algoritmos que serão utilizados
algorithmList <- c('rpart', 'knn', 'nb')

# Cria os modelos da lista
models <- caretList(build_successful~., data=arvore_dataset_2.treino, trControl=trainControl, methodList=algorithmList)

# Resultado (precisamos passar um list de modelos treinados com Train)
results <- resamples(models)

stackControl <- trainControl(method="cv", number=10, classProbs=TRUE, savePredictions=TRUE)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)