trainIndex_desbalanceado_java <- createDataPartition(projetos_80m_java_false[[5]]$build_successful, p=0.80, list=FALSE)
projeto_desbalanceado_java.treino <- projetos_80m_java_false[[5]][trainIndex_desbalanceado_java,]
projeto_desbalanceado_java.teste <- projetos_80m_java_false[[5]][-trainIndex_desbalanceado_java,]

trainIndex_desbalanceado_ruby <- createDataPartition(projetos_80m_ruby_false[[7]]$build_successful, p=0.80, list=FALSE)
projeto_desbalanceado_ruby.treino <- projetos_80m_ruby_false[[7]][trainIndex_desbalanceado_ruby,]
projeto_desbalanceado_ruby.teste <- projetos_80m_ruby_false[[7]][-trainIndex_desbalanceado_ruby,]

under_java <- ovun.sample(build_successful ~ ., data = projeto_desbalanceado_java.treino, method = "under", N = 538, seed = 1)$data
under_ruby <- ovun.sample(build_successful ~ ., data = projeto_desbalanceado_ruby.treino, method = "under", N = 150, seed = 1)$data

table(under_java$build_successful)
table(under_ruby$build_successful)

tree.under <- rpart(build_successful ~ ., data = under_java)
pred.tree.under <- predict(tree.under, newdata = projeto_desbalanceado_java.teste)

tree_ruby.under <- rpart(build_successful ~ ., data = under_ruby)
pred.tree_ruby.under <- predict(tree_ruby.under, newdata = projeto_desbalanceado_ruby.teste)

roc.curve(projeto_desbalanceado_java.teste$build_successful, pred.tree.under[,2])
accuracy.meas(projeto_desbalanceado_java.teste$build_successful, pred.tree.under[,2])

roc.curve(projeto_desbalanceado_ruby.teste$build_successful, pred.tree_ruby.under[,2])
accuracy.meas(projeto_desbalanceado_ruby.teste$build_successful, pred.tree_ruby.under[,2])


confusionMatrix(ifelse(pred.tree.under[,1] > 0.5, "false.", "true."), projeto_desbalanceado_java.teste$build_successful, positive="true.")
confusionMatrix(ifelse(pred.tree_ruby.under[,1] > 0.5, "false.", "true."), projeto_desbalanceado_ruby.teste$build_successful, positive="true.")
