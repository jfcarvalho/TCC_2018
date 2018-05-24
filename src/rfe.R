# Atribuindo variavel de controle e aplicando função rfeControl

rfeControl <- rfeControl(functions=rfFuncs, method="cv", number=10)

#Alicando RFE

# arvore_dataset_100 <- as.data.frame(arvore_dataset_100)
amostra_travis_5 <- as.data.frame(amostra_travis_5)
rfe_projetos_5 <- rfe(amostra_travis_5[,-22], amostra_travis_5[,22], sizes=c(1:21), rfeControl=rfeControl)
predictors(rfe_projetos_5)

plot(rfe_projetos_5, type=c("g", "o"))
plot(rfe_projetos, type=c("g", "o"))
