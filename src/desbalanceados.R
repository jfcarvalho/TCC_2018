trainIndex_desbalanceado_java <- createDataPartition(projetos_80m_java_false[[5]]$build_successful, p=0.80, list=FALSE)
projeto_desbalanceado_java.treino <- projetos_80m_java_false[[5]][trainIndex_desbalanceado_java,]
projeto_desbalanceado_java.teste <- projetos_80m_java_false[[5]][-trainIndex_desbalanceado_java,]

trainIndex_desbalanceado_ruby <- createDataPartition(projetos_80m_ruby_false[[7]]$build_successful, p=0.80, list=FALSE)
projeto_desbalanceado_ruby.treino <- projetos_80m_ruby_false[[7]][trainIndex_desbalanceado_ruby,]
projeto_desbalanceado_ruby.teste <- projetos_80m_ruby_false[[7]][-trainIndex_desbalanceado_ruby,]

under_java <- ovun.sample(build_successful ~ ., data = projeto_desbalanceado_java.treino, method = "under", N = 538, seed = 1)$data
under_ruby <- ovun.sample(build_successful ~ ., data = projeto_desbalanceado_ruby.treino, method = "under", N = 150, seed = 1)$data
over_java <- ovun.sample(build_successful ~ ., data = projeto_desbalanceado_java.treino, method = "over", N = 5044, seed = 1)$data
over_ruby <- ovun.sample(build_successful ~ ., data = projeto_desbalanceado_ruby.treino, method = "over", N = 1160, seed = 1)$data

trainIndex_proj_1 <- createDataPartition(projeto_1$build_successful, p=0.80, list=FALSE)
projeto_1.treino <- projeto_1[trainIndex_proj_1,]
projeto_1.teste <- projeto_1[-trainIndex_proj_1,]


table(under_java$build_successful)
table(under_ruby$build_successful)
table(over_ruby$build_successful)

tree.under <- rpart(build_successful ~ ., data = under_java)
pred.tree.under <- predict(tree.under, newdata = projeto_desbalanceado_java.teste)

tree.under <- rpart(build_successful ~ ., data = under_java)
pred.tree.under <- predict(tree.under, newdata = projeto_1.teste)
confusionMatrix(ifelse(pred.tree.under[,1] > 0.5, "false.", "true."), projeto_1.teste$build_successful, positive="true.")

tree_ruby.under <- rpart(build_successful ~ ., data = under_ruby)
pred.tree_ruby.under <- predict(tree_ruby.under, newdata = projeto_desbalanceado_ruby.teste)

tree_ruby.over <- rpart(build_successful ~ ., data = over_ruby)
pred.tree_ruby.over <- predict(tree_ruby.over, newdata = projeto_desbalanceado_ruby.teste)


tree_java.over <- rpart(build_successful ~ ., data = over_java)
pred.tree_java.over <- predict(tree_java.over, newdata = projeto_desbalanceado_java.treino)

roc.curve(projeto_desbalanceado_java.teste$build_successful, pred.tree_java.over[,2])
accuracy.meas(projeto_desbalanceado_java.teste$build_successful, pred.tree_java.over[,2])

roc.curve(projeto_desbalanceado_java.teste$build_successful, pred.tree.under[,2])
accuracy.meas(projeto_desbalanceado_java.teste$build_successful, pred.tree.under[,2])


roc.curve(projeto_desbalanceado_ruby.teste$build_successful, pred.tree_ruby.under[,2])
accuracy.meas(projeto_desbalanceado_ruby.teste$build_successful, pred.tree_ruby.under[,2])

roc.curve(projeto_desbalanceado_ruby.teste$build_successful, pred.tree_ruby.over[,2])
accuracy.meas(projeto_desbalanceado_ruby.teste$build_successful, pred.tree_ruby.over[,2])

confusionMatrix(ifelse(pred.tree.under[,1] > 0.5, "false.", "true."), projeto_desbalanceado_java.teste$build_successful, positive="true.")
confusionMatrix(ifelse(pred.tree_ruby.under[,1] > 0.5, "false.", "true."), projeto_desbalanceado_ruby.teste$build_successful, positive="true.")
confusionMatrix(ifelse(pred.tree_ruby.over[,1] > 0.5, "false.", "true."), projeto_desbalanceado_ruby.teste$build_successful, positive="true.")
confusionMatrix(ifelse(pred.tree_java.over[,1] > 0.5, "false.", "true."), projeto_desbalanceado_java.teste$build_successful, positive="true.")

rfe_debalanceado_ruby <- rfe(under_ruby[,-19], under_ruby[,19], sizes=c(1:18), rfeControl=rfeControl)
predictors(rfe_debalanceado_ruby)
plot(rfe_debalanceado_ruby, type=c("g", "o"))

rfe_debalanceado_java <- rfe(under_java[,-19], under_java[,19], sizes=c(1:18), rfeControl=rfeControl)
predictors(rfe_debalanceado_java)
plot(rfe_debalanceado_ruby, rfe_debalanceado_java, type=c("g", "o"))

rfe_debalanceado_ruby_over <- rfe(over_ruby[,-19], over_ruby[,19], sizes=c(1:18), rfeControl=rfeControl)
predictors(rfe_debalanceado_ruby_over)
plot(rfe_debalanceado_ruby_over, type=c("g", "o"))

rfe_debalanceado_java <- rfe(over_java[,-19], over_java[,19], sizes=c(1:18), rfeControl=rfeControl)
predictors(rfe_debalanceado_java)
plot(rfe_debalanceado_ruby, rfe_debalanceado_java, type=c("g", "o"))

######################## ROSE / SMOTE

trainIndex_rose_java <- createDataPartition(projetos_80m_java_false[[5]]$build_successful, p=0.80, list=FALSE)
projeto_rose_java.treino <- projetos_80m_java_false[[5]][trainIndex_rose_java,]
projeto_rose_java.teste <- projetos_80m_java_false[[5]][-trainIndex_rose_java,]

java.rose.treino <- projeto_rose_java.treino
java.rose.teste <- projeto_rose_java.teste

java_smote <- projetos_80m_java_false[[5]]

java.rose <- ROSE(build_successful~ ., data = java.rose.treino, seed = 1)$data


tree_java.rose <- rpart(build_successful ~ ., data = java.rose)
pred.tree_java.rose <- predict(tree_java.rose, newdata = java.rose.treino)

confusionMatrix(ifelse(pred.tree_java.rose[,1] > 0.5, "false.", "true."), java.rose.teste$build_successful, positive="true.")

#java.smote <- SMOTE(build_successful~ ., data = java.rose.treino, seed = 1)$data
java.smote <- SMOTE(build_successful~ ., data=as.data.frame(projeto_1), perc.over = 100, perc.under=200)

trainIndex_smote_java <- createDataPartition(java.smote$build_successful, p=0.80, list=FALSE)
projeto_smote_java.treino <- java.smote[trainIndex_smote_java,]
projeto_smote_java.teste <- java.smote[-trainIndex_smote_java,]

tree_java.smote <- rpart(build_successful ~ ., data = java.smote)
pred.tree_java.smote <- predict(tree_java.smote, newdata = projeto_smote_java.teste)
confusionMatrix(ifelse(pred.tree_java.smote[,1] > 0.5, "false.", "true."), projeto_smote_java.teste$build_successful, positive="true.")

roc.curve(projeto_smote_java.teste$build_successful, pred.tree_java.smote[,2])
accuracy.meas(projeto_smote_java.teste$build_successful, pred.tree_java.smote[,2])


#####################################

ruby.rose.dataset <- projetos_80m_ruby_false[[8]]

ruby.rose <- ROSE(build_successful~ ., data = rose_ruby, seed = 1)$data
ruby.smote <- SMOTE(build_successful~ ., data=as.data.frame(rose_ruby), perc.over = 100, perc.under=200)

trainIndex_rose_ruby <- createDataPartition(ruby.rose$build_successful, p=0.80, list=FALSE)
ruby.rose.treino <- ruby.rose[trainIndex_rose_ruby,]
ruby.rose.teste <- ruby.rose[-trainIndex_rose_java,]

tree_ruby.rose <- rpart(build_successful ~ ., data = ruby.rose.treino)
pred.tree_rose.ruby <- predict(tree_ruby.rose, newdata = ruby.rose.teste)
confusionMatrix(ifelse(pred.tree_rose.ruby[,1] > 0.5, "false.", "true."), ruby.rose.teste$build_successful, positive="true.")

roc.curve(ruby.rose.teste$build_successful, pred.tree_rose.ruby[,2])
accuracy.meas(ruby.rose.teste$build_successful, pred.tree_rose.ruby[,2])


trainIndex_smote_ruby <- createDataPartition(ruby.smote$build_successful, p=0.80, list=FALSE)
ruby.smote.treino <- ruby.smote[trainIndex_smote_ruby,]
ruby.smote.teste <- ruby.smote[-trainIndex_smote_ruby,]

tree_ruby.smote <- rpart(build_successful ~ ., data = ruby.smote.treino)
pred.tree_smote.ruby <- predict(tree_ruby.smote, newdata = ruby.smote.teste)
confusionMatrix(ifelse(pred.tree_smote.ruby[,1] > 0.5, "false.", "true."), ruby.smote.teste$build_successful, positive="true.")

roc.curve(ruby.smote.teste$build_successful, pred.tree_smote.ruby[,2])
accuracy.meas(ruby.smote.teste$build_successful, pred.tree_smote.ruby[,2])

####################################################################

under_completo_100 <- ovun.sample(build_successful ~ ., data = amostra_travis_100, method = "under", N = 87126, seed = 1)$data
under_completo_500 <- ovun.sample(build_successful ~ ., data = amostra_travis_500, method = "under", N = 210230, seed = 1)$data
under_completo_50 <- ovun.sample(build_successful ~ ., data = amostra_travis_50, method = "under", N = 62032, seed = 1)$data

over_completo_500 <- ovun.sample(build_successful ~ ., data = amostra_travis_500, method = "over", N = 400000, seed = 1)$data
over_completo_100 <- ovun.sample(build_successful ~ ., data = amostra_travis_100, method = "over", N = 200000, seed = 1)$data
over_completo_50 <- ovun.sample(build_successful ~ ., data = amostra_travis_50, method = "over", N = 120000, seed = 1)$data

smote_500 <- SMOTE(build_successful~ ., data=as.data.frame(amostra_travis_500), perc.over = 100, perc.under=200)
smote_100 <- SMOTE(build_successful~ ., data=as.data.frame(amostra_travis_100), perc.over = 100, perc.under=200)
smote_50 <- SMOTE(build_successful~ ., data=as.data.frame(amostra_travis_50), perc.over = 100, perc.under=200)

rose_500 <- ROSE(build_successful~ ., data = amostra_travis_500, seed = 1)$data
rose_100 <- ROSE(build_successful~ ., data = amostra_travis_100, seed = 1)$data
rose_50 <- ROSE(build_successful~ ., data = amostra_travis_50, seed = 1)$data
