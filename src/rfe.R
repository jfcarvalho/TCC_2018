# Atribuindo variavel de controle e aplicando função rfeControl

rfeControl <- rfeControl(functions=rfFuncs, method="cv", number=10)

#Alicando RFE

# arvore_dataset_100 <- as.data.frame(arvore_dataset_100)
amostra_travis_5 <- as.data.frame(amostra_travis_5)
rfe_projetos_5 <- rfe(amostra_travis_5[,-22], amostra_travis_5[,22], sizes=c(1:21), rfeControl=rfeControl)
predictors(rfe_projetos)

plot(rfe_projetos_5, type=c("g", "o"))
plot(rfe_projetos, type=c("g", "o"))

treinador_boruta <- Boruta(build_successful~., data = arvore_dataset_500, doTrace = 2,maxRuns=12)
print(treinador_boruta)


plot(treinador_boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(treinador_boruta$ImpHistory),function(i)treinador_boruta$ImpHistory[is.finite(treinador_boruta$ImpHistory[,i]),i])
names(lz) <- colnames(treinador_boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(treinador_boruta$ImpHistory), cex.axis = 0.7)
plot(treinador_boruta, x)
