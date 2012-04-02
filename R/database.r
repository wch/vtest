# Functions that interact with the test result database

# Get the resultset table for a given commit or resultset_hash
load_resultsets <- function(resultset_hash = NULL, commit = NULL) {
  if (!is.null(commit) && !is.null(resultset_hash))
    stop("Cannot specify both commit and resultset_hash.")


  if (file.exists(get_vtest_resultsets_file()))
    resultsets <- read.csv(get_vtest_resultsets_file(), stringsAsFactors = FALSE)
  else
    resultsets <- cbind(resultset_hash = character(), empty_resultset())

  if (is.null(commit) && is.null(resultset_hash))
    return(resultsets)

  if (!is.null(commit)) {
    commits <- read.csv(get_vtest_commits_file(), stringsAsFactors = FALSE)
    resultset_hash <- commits$resultset_hash[commits$commit == commit]

    if (length(resultset_hash) == 0)
      stop("No resultset_hash found for commit ", commit)
    else if (length(resultset_hash) > 1)
      stop("More than one resultset_hash found for commit ", commit)
  }

  return(resultsets[resultsets$resultset_hash == resultset_hash, , drop = FALSE])
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
  if (file.exists(get_vtest_commits_file()))
    read.csv(get_vtest_commits_file(), stringsAsFactors = FALSE)
  else
    data.frame(commit = character(), resultset_hash = character())
}


# Create a zero-row data frame to hold resultset
empty_resultset <- function() {
  data.frame(
    context = character(),
    desc    = character(),
    type    = character(),
    width   = numeric(),
    height  = numeric(),
    dpi     = numeric(),
    err     = character(),
    hash    = character(),
    order   = numeric(),
    stringsAsFactors = FALSE
  )
}