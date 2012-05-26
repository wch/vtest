# Functions that interact with both the git repo and the test result database


#' Returns a data frame of recent commits and their associated resultset_hash.
#'
#' For commits without a resultset, the resultset_hash is \code{NA}.
#'
#' @param start  The commit to start searching backward from
#' @param dir  The directory with the git repository
#' @param n  Maximum number of commits to search
#' @param main_branch  Don't show commits on branches that were merged in.
#' @export
recent_vtest <- function(dir = ".", start = "", n = 20, main_branch = TRUE) {
  prev <- git_prev_commits(dir = dir, n = n, start = start, main_branch = main_branch)
  prev_commits <- data.frame(idx = seq_along(prev), commit = prev)

  c_results <- load_commits_table()
  prev_commits <- merge(prev_commits, c_results, all.x = TRUE)
  prev_commits <- prev_commits[order(prev_commits$idx), ]
  prev_commits$idx <- NULL
  rownames(prev_commits) <- NULL
  prev_commits$commit <- git_abbrev_hash(prev_commits$commit)

  prev_commits
}


#' Returns a one-row data frame of the most recent commit that has a
#' resultset hash.
#'
#' @param start  The commit to start searching backward from
#' @param dir  The directory with the git repository
#' @param n  Maximum number of commits to search
#' @param main_branch  Don't show commits on branches that were merged in.
#' @export
most_recent_vtest <- function(dir = ".", start = "", main_branch = TRUE) {
  commits <- recent_vtest(n = NULL)
  idx <- min(which(!is.na(commits$resultset_hash)))
  commits[idx, ]
}
