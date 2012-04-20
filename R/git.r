# Functions that interact with the git repository

# Find the current git commit hash of a directory, given a commit ref
git_find_commit_hash <- function(dir = ".", ref = "HEAD") {
  ret <- system2("git", c("--git-dir", file.path(dir, ".git"), "rev-parse", ref),
    stdout = TRUE, stderr = TRUE)

  if (!is.null(attr(ret, "status")) || nchar(ret) != 40)
    stop("Error finding current git commit hash of repo at ", dir, ":", ret)

  ret <- gsub("\\n$", "", ret)  # Remove trailing \n
  return(ret)
}


# Check if the state of the git working tree is clean or dirty
git_check_clean <- function(dir = ".") {
  ret <- system2("git",
    c("--git-dir", file.path(dir, ".git"), "--work-tree", dir, "diff", "--shortstat"),
    stdout = TRUE, stderr = TRUE)

  if (!is.null(attr(ret, "status")))
    stop("Error checking git working tree clean/dity status of ", dir, ":", ret)

  ret <- gsub("\\n$", "", ret)  # Remove trailing \n

  if (length(ret) == 0)
    return(TRUE)
  else
    return(FALSE)
}


#' Return the last n commit hashes of a git repository
#'
#' @param dir  directory containing the git repository.
#' @param n  number of commit hashes to return. If \code{NULL} return all commits.
#' @param start  the commit to search backward from.
#' @param all  if \code{TRUE} return all commits including ones not on current
#'   branch (like \code{git log --all}).
#' @param main_branch  Don't show commits on branches that were merged in.
#'   This probably won't work correctly with \code{all=TRUE}.
#'
#' @return a character vector of commit hashes, or an empty vector if there
#'   was a problem executing the git command.
git_prev_commits <- function(dir = ".", n = 20, start = "", all = FALSE,
  main_branch = TRUE) {

  args <- c("--git-dir", file.path(dir, ".git"), "log", "--format='%H'")

  if (!is.null(n))  args <- c(args, str_c("-", n))
  if (all)          args <- c(args, "--all")
  if (main_branch)  args <- c(args, "--first-parent")

  ret <- system2("git", c(args, start), stdout = TRUE, stderr = TRUE)

  if (!is.null(attr(ret, "status")))
    stop("Error finding git history of ", dir, ":", ret)

  return(ret)
}

#' Return an abbreviated version of a hash
git_abbrev_hash <- function(hash = "") {
  substr(hash, 1, 7)
}
