boruta_100 <- Boruta(build_successful~., data = arvore_dataset_100, doTrace = 2)
print(boruta_100)

plot(boruta_100, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta_100$ImpHistory),function(i)boruta_100$ImpHistory[is.finite(boruta_100$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_100$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(boruta_100$ImpHistory), cex.axis = 0.7)