FROM rocker/rstudio

RUN apt-get -qq update && apt-get -yqq upgrade
RUN apt-get -yqq install libxml2-dev libgit2-dev libcurl4-gnutls-dev postgresql-client libssl-dev libpq-dev

# install R deps
RUN R -e 'install.packages(c("devtools", "igraph", "hash", "RJSONIO", "stringr", "formatR", "DBI", "stats", "foreach", "iterators", "gdata", "rredis", "R.utils", "Rcpp", "RCurl", "zoo", "futile.logger", "crayon", "stringi", "rstudioapi", "testthat", "roxygen2", "covr", "tis", "sourcetools", "mockery", "tempdisagg", "RPostgreSQL", "RSQLite"), repos="https://cran.rstudio.com")'

RUN R -e 'devtools::install_github("giupo/rutils")'
RUN R -e 'devtools::install_github("giupo/rprogressbar")'
RUN R -e 'devtools::install_github("giupo/rdataset")'

# install GrafoDB
RUN mkdir /home/GrafoDB
COPY . /home/GrafoDB
WORKDIR /home/GrafoDB
RUN rm -rf packrat/lib*
RUN make install

ENV ROOT=TRUE
# run rstudio
CMD ["/init"]
