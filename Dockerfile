FROM rocker/rstudio:4.0.4

RUN apt-get -qq update && apt-get -yqq upgrade
RUN apt-get -yqq install cmake wget libxml2-dev libcurl4-gnutls-dev postgresql-client libssl-dev libpq-dev

RUN wget https://github.com/libgit2/libgit2/releases/download/v1.1.0/libgit2-1.1.0.tar.gz && \
	tar xf libgit2-1.1.0.tar.gz && \
	cd libgit2-1.1.0 && \
 	mkdir build && cd build && cmake .. && cmake --build . && make install

# install R deps
RUN R -e 'install.packages(c("devtools", "igraph", "hash", "jsonlite", "stringr", "formatR", "DBI", "stats", "foreach", "iterators", "gdata", "rredis", "R.utils", "Rcpp", "RCurl", "zoo", "futile.logger", "stringi", "rstudioapi", "testthat", "roxygen2", "covr", "tis", "sourcetools", "mockery", "tempdisagg", "RPostgreSQL", "RSQLite", "progress"), repos="https://cran.rstudio.com")' && \
	R -e 'devtools::install_github("giupo/rutils")' && \
	R -e 'devtools::install_github("giupo/rprogressbar")' && \
	R -e 'devtools::install_github("giupo/rdataset")'

# install GrafoDB
RUN mkdir /home/GrafoDB
COPY . /home/GrafoDB
WORKDIR /home/GrafoDB
RUN make install

ENV ROOT=TRUE
# run rstudio
