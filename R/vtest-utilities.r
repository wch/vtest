# Utility functions for the vtest "object"

init_vtest <- NULL
set_vtest_pkg <- NULL
get_vtest_pkg <- NULL
get_vtest_dir <- NULL
get_vtest_resultdir <- NULL
get_vtest_imagedir <- NULL
get_vtest_htmldir <- NULL
get_vtest_diffdir <- NULL

get_vtest_lasttest_dir <- NULL
get_vtest_lasttest_resultset_file <- NULL

get_vcontext <- NULL
set_vcontext <- NULL
get_vtest_resultset <- NULL
get_vtest_commits_file <- NULL
get_vtest_resultsets_file <- NULL
append_vtest_resultset <- NULL

local({
  pkg <- NULL       # The package object
  testdir <- NULL   # The dir of the test scripts(usually package/visual_test/)
  resultdir <- NULL # Where the database files are saved
  imagedir <- NULL  # Where the image files are saved

  context <- NULL   # The context of a set of tests (usually in one script)
  context_count <- NULL # Keep count of tests, within this context
  resultset <- NULL  # Information about each test in a context

  # These are used by the top-level vtest function
  get_vtest_pkg <<- function() pkg
  get_vtest_dir <<- function() testdir
  get_vtest_resultdir <<- function() resultdir
  get_vtest_commits_file    <<- function() file.path(resultdir, "commits.csv")
  get_vtest_resultsets_file <<- function() file.path(resultdir, "resultsets.csv")
  get_vtest_imagedir <<- function() file.path(resultdir, "images")
  get_vtest_htmldir  <<- function() file.path(resultdir, "html")
  get_vtest_diffdir  <<- function() file.path(resultdir, "diff")

  get_vtest_lasttest_dir <<- function() file.path(tempdir(), "lasttest")
  get_vtest_lasttest_resultset_file <<- function() file.path(tempdir(), "lasttest", "resultset.csv")

  # These are used by each test script
  get_vcontext <<- function() context
  set_vcontext <<- function(value) {
    context <<- value
    context_count <<- 0
  }

  init_vtest <<- function(pkg, testdir = NULL, resultdir = NULL) {

    # Close context, if open
    if (!is.null(get_vcontext())) set_vcontext(NULL)

    # Create a zero-row data frame to hold resultset
    cols <- c("context", "desc", "type", "width", "height", "dpi", "err",
              "hash", "order")
    resultset <- setNames(data.frame(t(rep(NA, length(cols)))), cols)
    parent <- parent.env(environment())  # This is where the variables are
    parent$resultset <- resultset[-1, ]
  }

  set_vtest_pkg <<- function(pkg) {
    parent <- parent.env(environment())  # This is where the variables are
    pkg <- as.package(pkg)
    parent$pkg <- pkg

    if (is.null(resultdir))  {
      # If packaage dir is mypath/ggplot2, default result dir is mypath/ggplot2-vtest
      p <- strsplit(pkg$path, "/")[[1]]
      parent$resultdir <-
        paste(c(p[-length(p)], paste(pkg$package, "vtest", sep="-")), collapse="/")
    } else {
      parent$resultdir <- resultdir
    }

    if (!file.exists(get_vtest_lasttest_dir()))
      dir.create(get_vtest_lasttest_dir())
    
    if (is.null(testdir))  parent$testdir <- file.path(pkg$path, "visual_test")
    else                   parent$testdir <- testdir
  }

  get_vtest_resultset <<- function() resultset

  # Add information about a single test
  append_vtest_resultset <<- function(context, desc, type, width, height, dpi, err, hash, order) {
    # Check that context + description aren't already used
    if (sum(context == resultset$context & desc == resultset$desc) != 0)
      stop(contest, ":\"", desc, "\" cannot be added to resultset because it is already present.")

    context_count <<- context_count + 1

    resultset <<- rbind(resultset,
      data.frame(context = context, desc = desc, type = type, width = width,
        height = height, dpi = dpi, err = err, hash = hash,
        order = context_count, stringsAsFactors = FALSE))
  }

})
