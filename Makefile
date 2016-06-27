# Makefile for generating R packages.
# 2011 Andrew Redd
# 2014 Giuseppe Acito

PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

R_FILES := $(wildcard R/*.[R|r])
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(SRC_FILES)

.PHONY: tarball install check clean build

tarball: $(PKG_NAME)_$(PKG_VERSION).tar.gz 
$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	R CMD build .

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL --build $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

NAMESPACE: $(R_FILES) $(SRC_FILES)
	Rscript -e "library(roxygen2);roxygenize('.')"

DOCS: 
	Rscript -e "devtools::document()"

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

autotest:
	Rscript autotest.r

# so:     deps
so:
	Rscript --vanilla -e 'devtools::compile_dll()'

# deps: deps-jsoncpp deps-dist


#deps-dist:
#	mkdir -p ext/include
#	mkdir -p ext/libs
#	cp -R ext/jsoncpp/include/json ext/include/
#	cp  ext/jsoncpp/build/src/lib_json/libjsoncpp.a ext/libs

#deps-jsoncpp:
#	cd ext/jsoncpp && mkdir -p build
#	cd ext/jsoncpp/build && cmake ..
#	cd ext/jsoncpp/build && make jsoncpp_lib_static
