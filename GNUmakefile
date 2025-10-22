PKGSRC  := $(shell basename $(CURDIR))
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
TGZ     := $(PKGNAME)_$(PKGVERS).tar.gz
WINBIN  := $(PKGNAME)_$(PKGVERS).zip
RBIN ?= $(shell dirname "`which R`")

all: install

README.md: README.rmd
	Rscript -e "rmarkdown::render('README.rmd', output_format = 'github_document', output_options = list(html_preview = FALSE))"

pkgfiles = DESCRIPTION \
	.Rbuildignore \
	DESCRIPTION \
	NAMESPACE \
	NEWS.md \
	README.md \
	R/* \
	inst/examples/*.R \
	_pkgdown.yml \
	tests/testthat.R \
	tests/testthat/*

roxy:
	$(RBIN)/Rscript -e "roxygen2::roxygenize(roclets = c('rd', 'collate', 'namespace'))"

$(TGZ): $(pkgfiles)
	"$(RBIN)/R" CMD build . 2>&1 | tee log/build.log

pd: roxy
	"$(RBIN)/Rscript" -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"

pd_all: roxy
	"$(RBIN)/Rscript" -e "pkgdown::build_site(run_dont_run = TRUE)"

build: roxy $(TGZ)

test: build
	"$(RBIN)/Rscript" -e 'library(devtools); devtools::test()' 2>&1 | tee log/test.log
	sed -i -e "s/\r.*\r//" log/test.log

quickcheck: build
	_R_CHECK_CRAN_INCOMING_REMOTE_=false "$(RBIN)/R" CMD check $(TGZ) --no-tests

check: roxy build
	_R_CHECK_CRAN_INCOMING_REMOTE_=false "$(RBIN)/R" CMD check --as-cran --no-tests $(TGZ) 2>&1 | tee log/check.log

install: build
	"$(RBIN)/R" CMD INSTALL --no-multiarch $(TGZ)

winbuilder: build
	date
	@echo "Uploading to R-release on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-release/
	@echo "Uploading to R-devel on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-devel/

coverage:
	"$(RBIN)/Rscript" -e "covr::report(file = 'docs/coverage/coverage.html')"

.PHONEY: roxy pd test quickcheck check install winbuilder coverage
