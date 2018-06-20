boruta_100 <- Boruta(build_successful~., data = arvore_dataset_100, doTrace = 2)
print(boruta_100)

boruta_500 <- Boruta(build_successful~., data = arvore_dataset_500, doTrace = 2)
print(boruta_500)


plot(treinador_boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(treinador_boruta$ImpHistory),function(i)treinador_boruta$ImpHistory[is.finite(treinador_boruta$ImpHistory[,i]),i])
names(lz) <- colnames(treinador_boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(treinador_boruta$ImpHistory), cex.axis = 0.7)
