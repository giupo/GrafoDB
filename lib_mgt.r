is_win = .Platform$OS.type == "windows"
if(is_win) setInternet2()
.repos = "http://cran.mirror.garr.it/mirrors/CRAN/"

if(!require(devtools)) {
  Sys.setenv(http_proxy=Sys.getenv("http_proxy"))
  Sys.setenv(https_proxy=Sys.getenv("https_proxy"))
  install.packages(c("devtools", "roxygen2"), dependencies=TRUE, repos=.repos)
  library(devtools)
}

package <- as.package(".")
dep <- unlist(strsplit(package$depends, ",|\n"))
sugg <- unlist(strsplit(package$suggests, ",|\n"))
dep <- c(dep, sugg)

.required_pkgs <- dep[which(dep != "")]

for (pkg in .required_pkgs) {
  if(pkg != "rcf") {
    tryCatch({
      library(pkg, character.only=TRUE)
    }, error = function(err) {
      if(is_win) {
        install.packages(pkg, repos=.repos, dependencies=TRUE)
      } else {
        install.packages(
          pkg, repos=.repos, dependencies=TRUE, method="wget")
      }
    })
  }
}
