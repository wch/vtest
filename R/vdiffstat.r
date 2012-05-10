#' Find test results that changed between ref1 and ref2
#'
#' This function is used for comparing the test results for two different
#' commits of the package that was tested.
#'
#' @param ref1 a git commit ref to compare (usually this should be the older
#'  ref). If \code{"recent"}, then use the most recent commit with a saved
#'  resultset.
#' @param ref2 a git commit ref to compare (usually this should be the newer
#'  ref). The empty string \code{""} refers to the last-run tests.
#' @param pkg package object or path.
#' @param filter a regular expression; result pages will be generated only
#'   only for test contexts that match this pattern.
#' @param all if \code{TRUE}, show only Added, Deleted, and Changed test items.
#'   If \code{FALSE}, also show Unchanged test items.
#'
#' @return a data frame with information about test results that changed.
#'
#' @seealso \code{\link{vdiff_webpage}}
#' @export
vdiffstat <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "", all = FALSE) {
  init_vtest(pkg)

  if (ref1 == "recent") {
    recent <- most_recent_vtest()
    message("Comparing to most recent commit with saved resultset: ", recent$commit, ".")
    ref1 <- recent$commit
  }

  # Get the resultset data for ref1 and ref2
  if (ref1 == "") {
    ref1text <- "last local test"
    commit1 <- ""
    ti1 <- load_lastresultset()
  } else {
    ref1text <- ref1
    commit1 <- git_find_commit_hash(get_vtest_pkg()$path, ref1)
    ti1 <- load_resultsets(commit = commit1)
  }
  if (nrow(ti1) == 0) {
    stop("No resultset found for ref ", ref1, ", commit ", git_abbrev_hash(commit1), "\n",
      "Run recent_vtest() to see a list of recent commits with test results.")
  }

  if (ref2 == "") {
    ref2text <- "last local test"
    commit2 <- ""
    ti2 <- load_lastresultset()
  } else {
    ref2text <- ref2
    commit2 <- git_find_commit_hash(get_vtest_pkg()$path, ref2)
    ti2 <- load_resultsets(commit = commit2)
  }
  if (nrow(ti2) == 0) {
    stop("No resultset found for ref ", ref2, ", commit ", git_abbrev_hash(commit2), "\n",
      "Run recent_vtest() to see a list of recent commits with test results.")
  }

  # Keep just a few columns
  ti1 <- ti1[c("context", "desc", "order", "hash", "err")]
  ti2 <- ti2[c("context", "desc", "order", "hash", "err")]

  # Merge together and check for changes
  td <- merge(ti1, ti2, by=c("context", "desc"), suffixes = c("1", "2"), all = TRUE)
  td$status <- "U"                                      # Default Unchanged
  td$status[ is.na(td$hash1) & !is.na(td$hash2)] <- "A" # Added
  td$status[!is.na(td$hash1) &  is.na(td$hash2)] <- "D" # Deleted
  td$status[td$hash1 != td$hash2] <- "C"                # Changed
  td$status[td$err1  != td$err2]  <- "C"                # Changed (if error status changed)


  td$status <- factor(td$status, levels = c("U", "A", "D", "C")) 

  # Sort by order in ref1, then ref2
  td <- arrange(td, order1, order2)

  # Change order of columns to be prettier
  td <- td[c("context", "desc", "status", "hash1", "hash2", "err1", "err2", "order1", "order2")]

  # Pull out only the rows where context matches the filter
  td <- td[match_filter_idx(td$context, filter), ]

  if (!all)
    td <- td[td$status != "U", ]

  return(arrange(td, context, order1, order2))
}
