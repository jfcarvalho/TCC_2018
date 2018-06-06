library(dplyr)

projetos_java <- travis_selecionado %>% filter(gh_lang == "java")
projetos_javascript <- travis_selecionado %>% filter(gh_lang == "javascript")
projetos_ruby <- travis_selecionado %>% filter(gh_lang == "ruby")

projetos_java_falso <- projetos_java %>% filter(build_successful == "false.")
projetos_java_true <- projetos_java %>% filter(build_successful == "true.")

projetos_ruby_falso <- projetos_ruby %>% filter(build_successful == "false.")
projetos_ruby_true <- projetos_ruby %>% filter(build_successful == "true.")

projetos_javascript_falso <- projetos_javascript %>% filter(build_successful == "false.")
projetos_javascript_true <- projetos_javascript %>% filter(build_successful == "true.")

ggplot(travis_selecionado, aes(gh_lang, gh_team_size, fill = gh_lang)) + geom_boxplot() 

projetos_split_java <- split(projetos_java, projetos_java$gh_project_name)
projetos_split_ruby <- split(projetos_ruby, projetos_ruby$gh_project_name)
projetos_split_javascript <- split(projetos_javascript, projetos_javascript$gh_project_name)

projetos_80m_java_false <- list()
projetos_80m_ruby_false <- list()
projetos_80m_javascript_false <- list()

j <- 1

for(i in 1:length(projetos_split_java))
{
  if(prop.table(table(projetos_split_java[[i]]$build_successful))[[1]] > 0.8)
  {
    projetos_80m_java_false[[j]] <- projetos_split_java[[i]]
    j <- j+1
  }
}

j <- 1
for(i in 1:length(projetos_split_ruby))
{
  if(prop.table(table(projetos_split_ruby[[i]]$build_successful))[[1]] > 0.8)
  {
    projetos_80m_ruby_false[[j]] <- projetos_split_ruby[[i]]  
    j <- j+1
    
  }
}

j <- 1

for(i in 1:length(projetos_split_javascript))
{
  if(prop.table(table(projetos_split_javascript[[i]]$build_successful))[[1]] > 0.8)
  {
    projetos_80m_javascript_false[[j]] <- projetos_split_javascript[[i]]  
    j <- j+1
    }
}
