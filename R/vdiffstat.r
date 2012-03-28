# Find files modified between ref1 and ref2
# If ref2 is "", we treat it to mean the last test results.
# * all: if FALSE, don't return Unchanged; if true TRUE, return Unchanged
#     (in addition to everything else)
#' @export
vdiffstat <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "", all = FALSE) {
  set_vtest_pkg(pkg)

  # Get the resultset data for ref1 and ref2
  if (ref1 == "") {
    ref1text <- "last local test"
    commit1 <- ""
    ti1 <- load_lastresultset()
  } else {
    ref1text <- ref1
    commit1 <- git_find_commit_hash(get_vtest_pkg()$path, ref1)
    ti1 <- load_resultset(commit = commit1)
  }
  if (nrow(ti1) == 0) {
    stop("No resultset found for ref ", ref1, ", commit ", substr(commit1, 1, 8),
      "\n", nearest_commit_with_resultset(commit1, 25), "\n",
      "Run recent_commits_resultsets(\"", ref1, "\") to see a list of recent commits and their resultsets.")
  }

  if (ref2 == "") {
    ref2text <- "last local test"
    commit2 <- ""
    ti2 <- load_lastresultset()
  } else {
    ref2text <- ref2
    commit2 <- git_find_commit_hash(get_vtest_pkg()$path, ref2)
    ti2 <- load_resultset(commit = commit2)
  }
  if (nrow(ti2) == 0) {
    stop("No resultset found for ref ", ref2, ", commit ", substr(commit2, 1, 6),
      "\n", nearest_commit_with_resultset(commit2, 25), "\n",
      "Run recent_commits_resultsets(\"", ref2, "\") to see a list of recent commits and their resultsets.")
  }

  # Keep just a few columns
  ti1 <- ti1[c("context", "desc", "order", "hash")]
  ti2 <- ti2[c("context", "desc", "order", "hash")]

  # Merge together and check for changes
  td <- merge(ti1, ti2, by=c("context", "desc"), suffixes = c("1", "2"), all = TRUE)
  td$status <- "U"                                      # Default Unchanged
  td$status[ is.na(td$hash1) & !is.na(td$hash2)] <- "A" # Added
  td$status[!is.na(td$hash1) &  is.na(td$hash2)] <- "D" # Deleted
  td$status[td$hash1 != td$hash2] <- "C"                # Changed

  td$status <- factor(td$status, levels = c("U", "A", "D", "C")) 

  # Sort by order in ref1, then ref2
  td <- arrange(td, order1, order2)

  # Change order of columns to be prettier
  td <- td[c("context", "desc", "status", "hash1", "hash2", "order1", "order2")]

  # Pull out only the rows where context matches the filter
  td <- td[match_filter_idx(td$context, filter), ]

  if (!all)
    td <- td[td$status != "U", ]

  return(arrange(td, context, order1, order2))
}


#' This function is only to be called from vdiffstat
#'
#' @param start  The commit to start searching backward from
#' @param n  Maximum number of commits to search
nearest_commit_with_resultset <- function(start = "", n = 20) {
  pcommits <- git_prev_commits(dir = get_vtest_pkg()$path, n = n, start = start)
  c_results <- load_commits_table()

  matchidx <- which(pcommits %in% c_results$commit)
  if (length(matchidx) == 0) {
    return(str_c("No commit with a resultset found within last ", n,
      " commits"))

  } else {
    matchidx <- min(matchidx)   # If we did this with an empty matchidx, it would complain
    return(str_c("Most recent commit with a resultset: ",
      substr(pcommits[matchidx], 1, 6), ", found ", matchidx-1,
      " commits previous.\n"))
  }
}


#' Returns a data frame of recent commits and their associated resultset_hash.
#'
#' For commits without a resultset, the resultset_hash is NA.
#'
#' @param start  The commit to start searching backward from
#' @param n  Maximum number of commits to search
#' @export
recent_commits_resultsets <- function(start = "", n = 20) {
  prev <- git_prev_commits(dir = get_vtest_pkg()$path, n = n, start = start)
  prev_commits <- data.frame(idx = seq_along(prev), commit = prev)

  c_results <- load_commits_table()
  prev_commits <- merge(prev_commits, c_results, all.x = TRUE)
  prev_commits <- prev_commits[order(prev_commits$idx), ]
  prev_commits$idx <- NULL
  rownames(prev_commits) <- NULL
  prev_commits$commit <- substr(prev_commits$commit, 1, 6)  # Short git hash

  prev_commits
}
