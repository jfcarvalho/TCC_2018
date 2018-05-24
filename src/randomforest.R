rf_dataset_1000 <- nb_dataset_1000
rf_dataset_500 <- nb_dataset_500
rf_dataset_100 <- nb_dataset_100
rf_dataset_50 <- nb_dataset_50

trainIndex_rf_1000 <- createDataPartition(rf_dataset_1000$build_successful, p=0.70, list=FALSE)
rf_1000.treino <- rf_dataset_1000[ trainIndex_rf_1000,]
rf_1000.teste <- rf_dataset_1000[-trainIndex_rf_1000,]
rf.model <- randomForest(build_successful~., data=rf_1000.treino)

trainIndex_rf_100 <- createDataPartition(rf_dataset_100$build_successful, p=0.70, list=FALSE)
rf_100.treino <- rf_dataset_100[ trainIndex_rf_100,]
rf_100.teste <- rf_dataset_100[-trainIndex_rf_100,]
rf.model_100 <- randomForest(build_successful~., data=rf_100.treino)

trainIndex_rf_50 <- createDataPartition(rf_dataset_50$build_successful, p=0.70, list=FALSE)
rf_50.treino <- rf_dataset_50[ trainIndex_rf_50,]
rf_50.teste <- rf_dataset_50[-trainIndex_rf_50,]
rf.model_50 <- randomForest(build_successful~., data=rf_50.treino)
