# Find the current git commit hash of a directory, given a commit ref
git_find_commit_hash <- function(dir = ".", ref = "HEAD") {
  ret <- systemCall("git", c("--git-dir", file.path(dir, ".git"), "rev-parse", ref))
  ret$output <- gsub("\\n$", "", ret$output)  # Remove trailing \n

  if (ret$status == 0  && nchar(ret$output) == 40)
    return(ret$output)
  else
    stop("Error finding current git commit hash of repo at ", dir, ":", ret$output)
}


# Check if the state of the git working tree is clean or dirty
git_check_clean <- function(dir = ".") {
  ret <- systemCall("git",
    c("--git-dir", file.path(dir, ".git"), "--work-tree", dir, "diff", "--shortstat"))
  ret$output <- gsub("\\n$", "", ret$output)  # Remove trailing \n

  if (ret$status == 0) {
    if (length(ret$output) == 0)
      return(TRUE)
    else
      return(FALSE)
  } else {
    stop("Error checking git working tree clean/dity status of ", dir, ":", ret$output)
  }
}


#' Return the last n commit hashes of a git repository
#'
#' @param dir  directory containing the git repository
#' @param n  number of commit hashes to return
#' @param start  the commit to search backward from
#'
#' @value a character vector of commit hashes, or an empty vector if there
#'   was a problem executing the git command.
git_prev_commits <- function(dir = ".", n = 20, start = "") {
  ret <- systemCall("git", c("--git-dir", file.path(dir, ".git"), "log",
    "--format='%H'", str_c("-", n), start))

  if (ret$status == 0) {
    return(strsplit(ret$output, "\n")[[1]])
  } else {
    warning(ret$output)
    return(character())
  }
}
