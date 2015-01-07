library(testthat)
library(devtools)
# options(env="test")

load_all()

.praise <- c(
  "You rock!",
  "You are a coding rockstar!",
  "Keep up the good work.",
  ":)",
  "Woot!",
  "Way to go!",
  "Nice code."
)


GrowlReporter <- setRefClass(
  "GrowlReporter", contains = "Reporter",
  fields = list(
    "failures" = "list",
    "failed_count" = "integer",
    "passed_count" = "integer",
    "success_icon" = "character",
    "failed_icon" = "character",
    "show_praise" = "logical"),
  
  methods = list(
    initialize = function(...) {
      failures <<- list()
      passed_count <<- 0L
      failed_count <<- 0L
      success_icon <<- "http://jetpackweb.com/blog/wp-content/uploads/2009/09/pass.png"
      failed_icon <<- "http://jetpackweb.com/blog/wp-content/uploads/2009/09/fail.png"
      
      show_praise <<- TRUE
      callSuper(...)
    },
    
    add_result = function(result) {
      if (result$passed) {
        passed_count <<- passed_count + 1L
      } else {
        failed_count <<- failed_count + 1L
        result$test <- if (is.null(test)) "(unknown)" else test
        failures[[failed_count]] <<- result
      }
    },
    
    end_reporter = function() {      
      growl <- function(m, icon) {
        m <- gsub("\'","''", m)
        cmd <- if(is.windows()) {
          paste('growlnotify /silent:true /:R /t:testthat /i:',icon,' "',m,'"',  sep='')
        } else if(is.darwin()) {
          paste('/usr/local/bin/terminal-notifier -title testthat -message \'',m,
                '\' --appIcon', icon)
        } else {
          paste('notify-send  "testthat" "',m,'" -i ', icon)
        }
        
        invisible(suppressWarnings(
          system(cmd, intern=TRUE)))
      }
      icon <- success_icon
      report_text <- paste(passed_count, "tests passed")
      if(failed_count == 1) {
        x <- failures[[1]]
        ref <- x$srcref
        location <- if ( is.null(ref) ) {
          ''
        } else {
          paste0('(@', attr(ref, 'srcfile')$filename, '#', ref[1], ')')
        }
        type <- ifelse(sapply(failures, "[[", "error"), "Error", "Failure")
        tests <- vapply(failures, "[[", "test", FUN.VALUE = character(1))
        msg <- vapply(failures, "[[", "failure_msg", FUN.VALUE = character(1))
        icon <- failed_icon
        report_text <- paste(type, tests, msg, location)
      }
      
      if(failed_count > 1) {
        icon <- failed_icon
        report_text <- paste0(report_text, ", ", failed_count, " failed tests")
      }
           
      if(failed_count == 0 && passed_count > 10 && show_praise) {
        report_text <- paste0(report_text, ", ", sample(.praise, 1))
      }

      .self$clean()
      growl(report_text, icon)
    },

    clean = function() {
      failed_count <<- 0L
      passed_count <<- 0L
      failures <<- list()
    }
  )
)

reporters <- MultiReporter()
reporters$reporters <- list(SummaryReporter(), GrowlReporter())
auto_test("R", "tests/testthat/", reporter=reporters)
