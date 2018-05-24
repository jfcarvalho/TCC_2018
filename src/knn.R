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


dataset_knn_transformado100 <- scale(knn_dataset_100[,-19])
dataset_knn_transformado1000 <- scale(knn_dataset_1000[,c(1:18)])
dataset_knn_transformado500 <- scale(knn_dataset_500[,c(1:18)])
dataset_knn_transformado50 <- scale(knn_dataset_50[,c(1:18)])


build_status_1000 <- knn_dataset_1000[,19]
build_status_500 <- knn_dataset_500[,19]
build_status_100 <- knn_dataset_100[,19]
build_status_50 <- knn_dataset_50[,19]

dataset_knn_transformado1000 <- cbind(dataset_knn_transformado1000,build_status_1000)
dataset_knn_transformado1000 <- as.data.frame(dataset_knn_transformado1000)
dataset_knn_transformado500 <- cbind(dataset_knn_transformado500,build_status_500)
dataset_knn_transformado500 <- as.data.frame(dataset_knn_transformado500)

dataset_knn_transformado100 <- cbind(dataset_knn_transformado100,build_status_100)
dataset_knn_transformado100 <- as.data.frame(dataset_knn_transformado100)

dataset_knn_transformado50 <- cbind(dataset_knn_transformado50,build_status_50)
dataset_knn_transformado50 <- as.data.frame(dataset_knn_transformado50)

var(dataset_knn_transformado100)
var(knn_dataset_1000)


trainIndex_knn_100 <- createDataPartition(dataset_knn_transformado100$build_successful, p=0.50, list=FALSE)
knn_100.treino <- dataset_knn_transformado100[ trainIndex_knn_100,]
knn_100.teste <- dataset_knn_transformado100[-trainIndex_knn_100,]
predicted.build_100 <- knn(knn_100.treino[,-19], knn_100.teste[,-19], knn_100.treino[,19], 5)
confusionMatrix(predicted.build_100, knn_100.teste$build_successful)


trainIndex_knn_1000 <- createDataPartition(dataset_knn_transformado1000$build_successful, p=0.50, list=FALSE)
knn_1000.treino <- dataset_knn_transformado1000[ trainIndex_knn_1000,]
knn_1000.teste <- dataset_knn_transformado1000[-trainIndex_knn_1000,]
knn_status_1000.treino <- knn_dataset_1000[trainIndex_knn_1000,19]
knn_status_1000.teste <- knn_dataset_1000[-trainIndex_knn_1000,19]
predicted.build_1000 <- knn(knn_1000.treino[,-19], knn_1000.teste[,-19], knn_1000.treino[,19], 5)
confusionMatrix(predicted.build_1000, knn_1000.teste$build_successful)

trainIndex_knn_500 <- createDataPartition(dataset_knn_transformado500$build_successful, p=0.50, list=FALSE)
knn_500.treino <- dataset_knn_transformado500[ trainIndex_knn_500,]
knn_500.teste <- dataset_knn_transformado500[-trainIndex_knn_500,]
predicted.build_500 <- knn(knn_500.treino[,-19], knn_500.teste[,-19], knn_500.treino[,19], 5)
confusionMatrix(predicted.build_500, knn_500.teste$build_successful)

trainIndex_knn_50 <- createDataPartition(dataset_knn_transformado50$build_successful, p=0.50, list=FALSE)
knn_50.treino <- dataset_knn_transformado50[ trainIndex_knn_50,]
knn_50.teste <- dataset_knn_transformado50[-trainIndex_knn_50,]
predicted.build_50 <- knn(knn_50.treino[,-19], knn_50.teste[,-19], knn_50.treino[,19], 5)
confusionMatrix(predicted.build_50, knn_50.teste$build_successful)