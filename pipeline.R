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

# wflow_git_push(dry_run = TRUE)

