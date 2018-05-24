arvore_dataset_1000 <- amostra_travis_1000
arvore_dataset_500 <- amostra_travis_500
arvore_dataset_50 <- amostra_travis_50
arvore_dataset_10 <- amostra_travis_10
arvore_dataset_5 <- amostra_travis_5
arvore_dataset_2 <- amostra_travis_2

trainIndex_100 <- createDataPartition(arvore_dataset_100$build_successful, p=0.80, list=FALSE)
arvore_dataset_100.treino <- arvore_dataset_100[ trainIndex_100,]
arvore_dataset_100.teste <- arvore_dataset_100[-trainIndex_100,]

nb_dataset_100.treino <- nb_dataset_100[trainIndex_100,]
nb_dataset_100.teste <- nb_dataset_100[-trainIndex_100,]

trainIndex_500 <- createDataPartition(arvore_dataset_500$build_successful, p=0.80, list=FALSE)
arvore_dataset_500.treino <- arvore_dataset_500[ trainIndex_500,]
arvore_dataset_500.teste <- arvore_dataset_500[-trainIndex_500,]

trainIndex_1000 <- createDataPartition(arvore_dataset_1000$build_successful, p=0.80, list=FALSE)
arvore_dataset_1000.treino <- arvore_dataset_1000[ trainIndex_1000,]
arvore_dataset_1000.teste <- arvore_dataset_1000[-trainIndex_1000,]

trainIndex_50 <- createDataPartition(arvore_dataset_50$build_successful, p=0.80, list=FALSE)
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

tree_500 = rpart(build_successful ~ ., data = arvore_dataset_500.treino, control = rpart.control(cp=0.00149092), method="class")
predictions_arvore_500 <- predict(tree_500, newdata = arvore_dataset_500.teste)
confusionMatrix(ifelse(predictions_arvore_500[,1] > 0.5, "false.", "true."), arvore_dataset_500.teste$build_successful)

tree_1000 = rpart(build_successful ~ ., data = arvore_dataset_1000.treino, control = rpart.control(cp=0.00149092), method="class")
predictions_arvore_1000 <- predict(tree_1000, newdata = arvore_dataset_1000.teste)
confusionMatrix(ifelse(predictions_arvore_1000[,1] > 0.5, "false.", "true."), arvore_dataset_1000.teste$build_successful)

tree_1000 = rpart(build_successful ~ ., data = arvore_dataset_1000.treino, control = rpart.control(cp=0.00149092), method="class")
predictions_arvore_1000 <- predict(tree_1000, newdata = arvore_dataset_1000.teste)
confusionMatrix(ifelse(predictions_arvore_1000[,1] > 0.5, "false.", "true."), arvore_dataset_1000.teste$build_successful)

tree_50 = rpart(build_successful ~ ., data = arvore_dataset_50.treino, control = rpart.control(cp=0.00149092), method="class")
predictions_arvore_50 <- predict(tree_50, newdata = arvore_dataset_50.teste)
confusionMatrix(ifelse(predictions_arvore_50[,1] > 0.5, "false.", "true."), arvore_dataset_50.teste$build_successful)


predictionsTeste_100 <- predict(tree_100, newdata = arvore_dataset_100.teste)
confusionMatrix(ifelse(predictionsTeste_100[,1] > 0.5, "false.", "true."), arvore_dataset_100.teste$build_successful)
