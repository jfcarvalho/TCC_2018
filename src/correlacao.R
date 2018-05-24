# Retirando features categoricas

travis_corr_1000 <- amostra_travis_1000
travis_corr_500 <- amostra_travis_500
travis_corr_100 <- amostra_travis_100
travis_corr_50 <- amostra_travis_50
travis_corr_10 <- amostra_travis_10
travis_corr_5 <- amostra_travis_5

travis_corr_1000$gh_by_core_team_member <- NULL
travis_corr_1000$gh_lang <- NULL
travis_corr_1000$gh_is_pr <- NULL
travis_corr_1000$gh_project_name <- NULL
travis_corr_1000$tr_build_id <- NULL

travis_corr_500$gh_by_core_team_member <- NULL
travis_corr_500$gh_lang <- NULL
travis_corr_500$gh_is_pr <- NULL
travis_corr_500$gh_project_name <- NULL
travis_corr_500$tr_build_id <- NULL

travis_corr_100$gh_by_core_team_member <- NULL
travis_corr_100$gh_lang <- NULL
travis_corr_100$gh_is_pr <- NULL
travis_corr_100$gh_project_name <- NULL
travis_corr_100$tr_build_id <- NULL

travis_corr_50$gh_by_core_team_member <- NULL
travis_corr_50$gh_lang <- NULL
travis_corr_50$gh_is_pr <- NULL
travis_corr_50$gh_project_name <- NULL
travis_corr_50$tr_build_id <- NULL

travis_corr_10$gh_by_core_team_member <- NULL
travis_corr_10$gh_lang <- NULL
travis_corr_10$gh_is_pr <- NULL
travis_corr_10$gh_project_name <- NULL
travis_corr_10$tr_build_id <- NULL

travis_corr_5$gh_by_core_team_member <- NULL
travis_corr_5$gh_lang <- NULL
travis_corr_5$gh_is_pr <- NULL
travis_corr_5$gh_project_name <- NULL
travis_corr_5$tr_build_id <- NULL

# Correlação entre variáveis

cores <- colorRampPalette(c("yellow", "gray", "black"))

correlacao_1000 <- cor(travis_corr_1000[,c(1:18)])
corrplot(correlacao, order="AOE", method="circle", col=cores(20), tl.srt=45, tl.cex=0.35, tl.col="black")
corrplot(correlacao, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.55)

correlacao_500 <- cor(travis_corr_500[,c(1:18)])

corrplot(correlacao_500, order="AOE", method="circle", col=cores(20), tl.srt=45, tl.cex=0.75, tl.col="black")
corrplot(correlacao_500, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.55)

correlacao_100 <- cor(travis_corr_100[,c(1:18)])

corrplot(correlacao_100, order="AOE", method="circle", col=cores(20), tl.srt=45, tl.cex=0.75, tl.col="black")
corrplot(correlacao_100, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.55)

correlacao_50 <- cor(travis_corr_50[,c(1:18)])
corrplot(correlacao_50, order="AOE", method="circle", col=cores(20), tl.srt=45, tl.cex=0.75, tl.col="black")
corrplot(correlacao_50, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.55)

correlacao_10 <- cor(travis_corr_10[,c(1:18)])
corrplot(correlacao_10, order="AOE", method="circle", col=cores(20), tl.srt=45, tl.cex=0.75, tl.col="black")
corrplot(correlacao_10, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.55)

correlacao_5 <- cor(travis_corr_5[,c(1:18)])
corrplot(correlacao_5, order="AOE", method="circle", col=cores(20), tl.srt=45, tl.cex=0.35, tl.col="black")
corrplot(correlacao_5, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.55)
