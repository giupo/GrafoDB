#!/usr/bin/env Rscript

library(testthat)

#' Test reporter: summary of errors in jUnit XML format.
#'
#' This reporter includes detailed results about each test and summaries,
#' written to a file (or stdout) in jUnit XML format. This can be read by
#' the Jenkins Continuous Integration System to report on a dashboard etc.
#' Requires the XML package.
#'
#' This works for \code{\link{expect_that}} but not for the wrappers like
#' \code{\link{expect_equal}} etc.
#'
#' To fit into the jUnit structure, context() becomes the \code{<testsuite>}
#' name as well as the base of the \code{<testcase> classname}. The
#' test_that() name becomes the rest of the \code{<testcase> classname}.
#' The deparsed expect_that() call becomes the \code{<testcase>} name.
#' On failure, the message goes into the \code{<failure>} node message
#' argument (first line only) and into its text content (full message).
#'
#' Execution time and some other details are also recorded.
#'
#' References for the jUnit XML format:
#' http://ant.1045680.n5.nabble.com/schema-for-junit-xml-output-td1375274.html
#' http://windyroad.org/dl/Open%20Source/JUnit.xsd
#'
#' Output drawn from the SummaryReporter is gprinted to the standard
#' error stream during execution.
#'
#' @export
#' @exportClass JUnitReporter
#' @aliases JUnitReporter-class
#' @examples
#' if (require("XML")) {
#' test_package("testthat", reporter = JUnitReporter$new(file = "testjunit.xml"))
#' }
#' @keywords debugging

JUnitReporter <- setRefClass("JUnitReporter", contains = "Reporter",
  fields = list(
    "file" = "character",
    "results" = "list",
    "timer" = "ANY"),

  methods = list(
    initialize = function(file = "", ...) {
      if (!require("XML", quietly = TRUE)) {
        stop("Please install the XML package", call. = FALSE)
      }
      callSuper(...)
      file <<- file
      results <<- list()
    },

    start_reporter = function() {
      callSuper()
      results <<- list()
      timer <<- proc.time()
      context <<- "(ungrouped)"
    },

    start_context = function(desc) {
      callSuper(desc)
      cat(desc, ": ", file = stderr())
    },

    end_context = function() {
      callSuper()
      cat("\n", file = stderr())
    },

    add_result = function(result) {
      if (result$passed) {
        cat(colourise(".", fg = "light green"), file = stderr())
      } else {
        failed <<- TRUE
        if (result$error) {
          cat(colourise("F", fg = "red"), file = stderr())
        } else {
          cat(colourise("E", fg = "red"), file = stderr())
        }
      }
      result$time <- round((proc.time() - timer)[["elapsed"]], 2)
      timer <<- proc.time()
      result$test <- if (is.null(test) || test == "") "(unnamed)" else test
      result$call <- if (is.null(result$call)) "(unexpected)" else result$call
      results[[context]] <<- append(results[[context]], list(result))
    },

    end_reporter = function() {
      cat("\n", file = stderr())
      xmlNodeOK <- function(name, ..., attrs = NULL) {
        ## do XML entity substitutions
        if (!is.null(attrs)) {
          attrs <- sapply(attrs, function(x) toString(xmlTextNode(x)))
        }
        xmlNode(name, ..., attrs = attrs)
      }
      classnameOK <- function(text) {
        gsub("[ \\.]", "_", text)
      }
      suites <- lapply(names(results), function(context) {
        x <- results[[context]]
        xnames <- vapply(x, "[[", "call", FUN.VALUE = character(1))
        xnames <- make.unique(xnames, sep = "_")
        for (i in seq_along(x)) {
          x[[i]]$call <- xnames[[i]]
        }
        testcases <- lapply(x, function(result) {
          failnode <- NULL
          if (!result$passed) {
            failnode <-
              xmlNodeOK("failure", attrs =
                        c(type = ifelse(result$error, "error", "failure"),
                          message = gsub("\n.*", "", result$message)),
                        .children = list(xmlTextNode(result$message)))
          }
          xmlNodeOK("testcase", attrs =
                    c(classname = paste(classnameOK(context),
                          classnameOK(result$test), sep = "."),
                      name = result$call,
                      time = result$time),
                    .children = if (!result$passed) list(failnode))
        })
        ispass <- sapply(x, "[[", "passed")
        iserr <- sapply(x, "[[", "error")
        tests <- sapply(x, "[[", "test")
        xmlNodeOK("testsuite", attrs =
                  c(tests = length(x),
                    failures = sum(!ispass & !iserr),
                    errors = sum(iserr),
                    name = context,
                    time = sum(sapply(x, "[[", "time")),
                    timestamp = toString(Sys.time()),
                    hostname = Sys.info()[["nodename"]]),
                  .children = testcases)
      })
      cat(toString(xmlNode("testsuites", .children = suites)),
          file = file)
    }
  )
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
        invisible(suppressWarnings(
          system(paste('growlnotify /silent:true /:R /t:testthat /i:',icon,' "',m,'"',  sep=''),
                 intern=TRUE)))
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

test_check("GrafoDB", reporter=reporters)
