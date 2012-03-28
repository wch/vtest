# Get the resultset table for a given commit or resultset_hash
load_resultset <- function(commit = NULL, resultset_hash = NULL) {
  if (is.null(commit) && is.null(resultset_hash))
    stop("Must specify either commit or resultset_hash.")
  else if (!is.null(commit) && !is.null(resultset_hash))
    stop("Must specify one of commit or resultset_hash, not both.")

  resultsets <- read.csv(get_vtest_resultsets_file(), stringsAsFactors = FALSE)

  if (!is.null(commit)) {
    commits <- read.csv(get_vtest_commits_file(), stringsAsFactors = FALSE)
    resultset_hash <- commits$resultset_hash[commits$commit == commit]

    if (length(resultset_hash) > 1)
      stop("More than one resultset_hash found for commit ", commit)
  }

  return(resultsets[resultsets$resultset_hash == resultset_hash, ])
}


# Get the resultset table for the last test run
load_lastresultset <- function() {
  return(read.csv(get_vtest_lasttest_resultset_file(), stringsAsFactors = FALSE))
}


# Get a hash of a resultset table
hash_resultset <- function(t) {
  # Reset the row names so it hashes like the original
  rownames(t) <- NULL
  # Sort by context and then order
  t <- arrange(t, context, order)

  # Make sure number columns are treated as num instead of int (for consistent hashing)
  numcols <- sapply(t, is.numeric)
  t[numcols] <- lapply(t[numcols], as.numeric)

  digest(t)
}

#' Load the commit-resultset_hash table
load_commits_table <- function() {
  read.csv(get_vtest_commits_file(), stringsAsFactors = FALSE)
}
