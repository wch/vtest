# Global information about visual tests, stored in vt,
# and utility functions for the vt "object"

# This list holds information about current tests
vt <- new.env(hash = TRUE, parent = emptyenv())

#' Initialize the environment for vtest to run
#'
#' @param pkg  A package object, or path to the package
#' @param testdir  The directory with the test scripts. If \code{NULL},
#'   it will be set to packagedir/visual_test/.
#' @param resultdir  The directory that results are stored in. If \code{NULL},
#'   it will be set to packagedir/visual_test/vtest/.
#'
#' @export
init_vtest <- function(pkg = NULL, testdir = NULL, resultdir = NULL) {

  # Close context, if open
  if (!is.null(get_vcontext())) set_vcontext(NULL)

  # Reset vt to starting state
  vt$pkg <- NULL       # The package object that's being tested
  vt$testdir <- NULL   # The dir of the test scripts(usually package/visual_test/)
  vt$resultdir <- NULL # Where the database files are saved
  vt$resultset <- NULL # Information about each test

  vt$pkg <- as.package(pkg)

  # The directory where visual test scripts are stored
  if (is.null(testdir))
    vt$testdir <- file.path(vt$pkg$path, "visual_test")
  else
    vt$testdir <- testdir

  # If packaage dir is mypath/ggplot2, default result dir is mypath/ggplot2/visual_test/vtest
  if (is.null(vt$resultdir))
    vt$resultdir <- file.path(vt$pkg$path, "visual_test", "vtest")
  else
    vt$resultdir <- resultdir


  # Make directories for storing results
  if (!file.exists(get_vtest_resultdir())) {
    if (!confirm(paste(get_vtest_resultdir(), "does not exist! Create? (y/n) ")))
      stop("Cannot continue without creating directory for results")
    dir.create(get_vtest_resultdir(), recursive = TRUE, showWarnings = FALSE)

    cat(paste("html", "diff", "lasttest", "pngcache", "", sep = "\n"),
      file = file.path(get_vtest_resultdir(), ".gitignore"))
  }

  if (!file.exists(get_vtest_imagedir()))
    dir.create(get_vtest_imagedir())

  if (!file.exists(get_vtest_lasttest_dir()))
    dir.create(get_vtest_lasttest_dir())

  # Create a zero-row data frame to hold resultset
  vt$resultset <- empty_resultset()

  invisible()
}


reset_lasttest <- function() {
  unlink(dir(get_vtest_lasttest_dir(), full.names = TRUE))
}

get_vtest_pkg <- function() vt$pkg
get_vtest_testdir <- function() vt$testdir
get_vtest_resultdir <- function() vt$resultdir
get_vtest_imagedir <- function() file.path(vt$resultdir, "images")
get_vtest_htmldir <- function() file.path(vt$resultdir, "html")
get_vtest_diffdir <- function() file.path(vt$resultdir, "diff")

get_vtest_commits_file    <- function() file.path(vt$resultdir, "commits.csv")
get_vtest_resultsets_file <- function() file.path(vt$resultdir, "resultsets.csv")

get_vtest_lasttest_dir            <- function() file.path(vt$resultdir, "lasttest")
get_vtest_lasttest_resultset_file <- function() file.path(vt$resultdir, "lasttest", "resultset.csv")

get_vtest_resultset <- function() vt$resultset


# Add information about a single test
append_vtest_resultset <- function(context, desc, type, width, height, dpi, err, hash, order) {
  # Check that context + description aren't already used
  if (sum(context == vt$resultset$context  &  desc == vt$resultset$desc) != 0)
    stop(context, ":\"", desc, "\" cannot be added to resultset because it is already present.")

  vt$resultset <- rbind(vt$resultset,
    data.frame(context = context, desc = desc, type = type, width = width,
      height = height, dpi = dpi, err = err, hash = hash,
      order = order, source = vt$.file_parsing, stringsAsFactors = FALSE))
}


# Make sure that a package is loaded for vtest.
assert_vtest_pkg_loaded <- function() {
  if (is.null(get_vtest_pkg()))
    stop("No package is currently set for vtest. Run init_vtest('pkg_dir').")
}
