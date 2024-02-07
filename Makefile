.PHONY: analyze compile all

.DEFAULT_GOAL := help

analyze: ## 1) Analyze all data
	@echo 'This code reproduces the manuscript, including'
	@echo 'all the analyses and figures'
	@echo 'Begin the analyses...'
	@Rscript MAKEFILE.R
	@echo '... end analyses'

compile: ## 2) Compile manuscript
	@echo 'Compile manuscript...'
	@cd manuscript/; find . -maxdepth 1 -name '*.tex' -exec pdflatex --interaction=batchmode {} \;
	@cd manuscript/; find . -maxdepth 1 -name '*.bcf' -exec biber --validate-datamodel {} \;
	@cd manuscript/; find . -maxdepth 1 -name '*.tex' -exec pdflatex --interaction=batchmode {} \;
	@cd manuscript/; rm *.log *.aux *.out *.bcf *.toc *.run.xml *.bbl *.blg *.lot *.lof
	@echo 'Open manuscript...'
	@cd manuscript/; find . -maxdepth 1 -name '*.pdf' -exec xdg-open {} \;
	@echo '...finished!'

all: ## Reproduce the whole project
	@echo 'This code reproduces the manuscript, including'
	@echo 'all the analyses and figures'
	@echo 'Begin the analyses...'
	@Rscript MAKEFILE.R
	@echo '... end analyses'
	@echo 'Compile manuscript...'
	@cd manuscript/; find . -maxdepth 1 -name '*.tex' -exec pdflatex --interaction=batchmode {} \;
	@cd manuscript/; find . -maxdepth 1 -name '*.bcf' -exec biber --validate-datamodel {} \;
	@cd manuscript/; find . -maxdepth 1 -name '*.tex' -exec pdflatex --interaction=batchmode {} \;
	@cd manuscript/; rm *.log *.aux *.out *.bcf *.toc *.run.xml *.bbl *.blg *.lot *.lof
	@echo 'Open manuscript...'
	@cd manuscript/; find . -maxdepth 1 -name '*.pdf' -exec xdg-open {} \;
	@echo '...finished!'

.PHONY: help

# https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
