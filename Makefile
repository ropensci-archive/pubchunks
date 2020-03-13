PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

doc:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

readme: README.Rmd
	${RSCRIPT} -e 'library(methods); knitr::knit("README.Rmd")'

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

check_windows:
	${RSCRIPT} -e "devtools::check_win_devel(); devtools::check_win_release()"

test:
	${RSCRIPT} -e "devtools::test()"
