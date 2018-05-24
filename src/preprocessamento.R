travistorrent_8_2_2017 <- read_csv("C:/Users/nessk/Desktop/TravisTorrent/travistorrent_8_2_2017.csv")

#Retirando variáveis que não servirão para fazer predições

travis_selecionado <- travistorrent_8_2_2017

travis_selecionado$tr_status <- NULL
travis_selecionado$tr_jobs <- NULL
travis_selecionado$tr_build_number <- NULL
travis_selecionado$tr_job_id <- NULL
travis_selecionado$tr_log_lan <- NULL
travis_selecionado$tr_log_status <- NULL
travis_selecionado$tr_log_setup_time <- NULL
travis_selecionado$tr_log_analyzer <- NULL
travis_selecionado$tr_log_frameworks <- NULL
travis_selecionado$git_trigger_commit <- NULL
travis_selecionado$tr_virtual_merged_into <- NULL
travis_selecionado$tr_original_commit <- NULL
travis_selecionado$gh_description_complexity <- NULL
travis_selecionado$gh_pushed_at <- NULL
travis_selecionado$gh_build_started_at <- NULL
travis_selecionado$gh_pull_req_num <- NULL
travis_selecionado$git_merged_with <- NULL
travis_selecionado$git_branch <- NULL
travis_selecionado$gh_commits_in_push <- NULL
travis_selecionado$git_prev_commit_resolution_status <- NULL
travis_selecionado$git_prev_built_commit <- NULL
travis_selecionado$tr_prev_build <- NULL
travis_selecionado$gh_first_commit_created_at <- NULL
travis_selecionado$git_all_built_commits <- NULL
travis_selecionado$git_trigger_commit <- NULL
travis_selecionado$tr_virtual_merged_into <- NULL
travis_selecionado$tr_original_commit <- NULL
travis_selecionado$gh_description_complexity <- NULL
travis_selecionado$gh_pushed_at <- NULL
travis_selecionado$gh_pr_created_at <- NULL
travis_selecionado$gh_pull_req_num <- NULL
travis_selecionado$git_merged_with <- NULL
travis_selecionado$tr_duration <- NULL
travis_selecionado$tr_log_bool_tests_ran <- NULL
travis_selecionado$tr_log_bool_tests_failed <- NULL
travis_selecionado$tr_log_num_tests_ok <- NULL
travis_selecionado$tr_log_num_tests_failed <- NULL
travis_selecionado$tr_log_num_tests_run <- NULL
travis_selecionado$tr_log_num_tests_skipped <- NULL
travis_selecionado$tr_log_tests_failed <- NULL
travis_selecionado$tr_log_testduration <- NULL
travis_selecionado$tr_log_buildduration <- NULL
travis_selecionado$gh_by_core_team_member <- NULL

travis_selecionado$gh_num_commits_in_push <- as.numeric(travis_selecionado$gh_num_commits_in_push)  
travis_selecionado$gh_num_issue_comments <- as.numeric(travis_selecionado$gh_num_issue_comments)
travis_selecionado$gh_num_pr_comments <- as.numeric(travis_selecionado$gh_num_pr_comments)

# Retirando duplicação do dataset.

travis_selecionado <- travis_selecionado[!duplicated(travis_selecionado$tr_build_id),]

#Pegado projetos aleatórios, número inicial de 500

projetos_aleatorios_1000 <- travis_selecionado %>%
  select(gh_project_name, gh_lang) %>%
  sample_n(1000) %>%
  unique()

projetos_aleatorios_500 <- travis_selecionado %>%
  select(gh_project_name, gh_lang) %>%
  sample_n(500) %>%
  unique()

projetos_aleatorios_100 <- travis_selecionado %>%
  select(gh_project_name, gh_lang) %>%
  sample_n(100) %>%
  unique()

projetos_aleatorios_50 <- travis_selecionado %>%
  select(gh_project_name, gh_lang) %>%
  sample_n(50) %>%
  unique()

projetos_aleatorios_10 <- travis_selecionado %>%
  select(gh_project_name, gh_lang) %>%
  sample_n(10) %>%
  unique()

projetos_aleatorios_5 <- travis_selecionado %>%
  select(gh_project_name, gh_lang) %>%
  sample_n(5) %>%
  unique()

projetos_aleatorios_2 <- travis_selecionado %>%
  select(gh_project_name, gh_lang) %>%
  sample_n(2) %>%
  unique()




#Pegando amostra completa dos projetos selecionados aleatoriamente
amostra_travis_1000 <- travis_selecionado %>%
  filter(gh_project_name %in% projetos_aleatorios_1000$gh_project_name)

amostra_travis_500 <- travis_selecionado %>%
  filter(gh_project_name %in% projetos_aleatorios_500$gh_project_name)

amostra_travis_100 <- travis_selecionado %>%
  filter(gh_project_name %in% projetos_aleatorios_100$gh_project_name)

amostra_travis_50 <- travis_selecionado %>%
  filter(gh_project_name %in% projetos_aleatorios_50$gh_project_name)

amostra_travis_10 <- travis_selecionado %>%
  filter(gh_project_name %in% projetos_aleatorios_10$gh_project_name)

amostra_travis_2 <- travis_selecionado %>%
  filter(gh_project_name %in% projetos_aleatorios_2$gh_project_name)
