# save(loocv_DFA_1trend_PS, loocv_DFA_2trends_PS, trends, trends_by_sp, FacLoad, HMM_model_DFA_1trend_looic, HMM_model_DFA_2trends_looic,
#      rotate_DFA_2trends_trends_mean, sds,
#      file='output/BDFA_model/BDFA_results.Rdata')

librarian::shelf(grateful)

cite_packages(include.RStudio=T, cite.tidyverse=T,
              out.format = "Rmd",
              out.file = "rpackages",
              bib.file = "rpackages-refs",
              out.dir = file.path(getwd(), "analysis"))

wflow_build(c("analysis/Reproduce.Rmd",
              "analysis/index.Rmd",
              "analysis/license.Rmd",
              "analysis/rpackages.Rmd"))

wflow_publish("analysis/*Rmd", "Start my new project")

wflow_status()

wflow_view()

# wflow_use_github("palmaraz")

wflow_git_push(dry_run = TRUE)

