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

model_nb_100 <- NaiveBayes(build_successful~., data=nb_dataset_100.treino)
predictions_100 <- predict(model_nb_100, nb_dataset_100.teste[,-19])
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