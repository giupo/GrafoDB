# Makefile for generating R packages.
# 2011 Andrew Redd
# 2014 Giuseppe Acito

PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

R_FILES := $(wildcard R/*.[R|r])
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(SRC_FILES)

.PHONY: tarball clean CHANGELOG.md

tarball: $(PKG_NAME)_$(PKG_VERSION).tar.gz 
$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	R CMD build .


check: tarball
	Rscript -e 'devtools::check(error_on="error")'

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla CMD INSTALL --build $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

NAMESPACE: $(R_FILES) $(SRC_FILES)
	@Rscript -e "require(methods); devtools::document()"

DOCS: NAMESPACE

clean:
	-rm -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f src/*.o src/*.so

.PHONY: list

list:
	@echo "R files:"
	@echo $(R_FILES)
	@echo "Source files:"
	@echo $(SRC_FILES)

test:
	@Rscript -e 'require(methods); devtools::test()'

autotest:
	@Rscript autotest.r

# so:     deps
so:
	@Rscript --vanilla -e 'devtools::compile_dll()'

coverage:
	@Rscript -e 'covr::package_coverage(path="."")'

codecov:
	@Rscript -e 'covr::codecov(path=".")'

CHANGELOG.md:
	@gitchangelog | grep -v "git-svn-id" > CHANGELOG.md

changelog: CHANGELOG.md
