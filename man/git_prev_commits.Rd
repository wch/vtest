\name{git_prev_commits}
\alias{git_prev_commits}
\title{Return the last n commit hashes of a git repository}
\usage{
  git_prev_commits(dir = ".", n = 20, start = "",
    all = FALSE)
}
\arguments{
  \item{dir}{directory containing the git repository.}

  \item{n}{number of commit hashes to return. If
  \code{NULL} return all commits.}

  \item{start}{the commit to search backward from.}

  \item{all}{if \code{TRUE} return all commits including
  ones not on current branch (like \code{git log --all}).}
}
\value{
  a character vector of commit hashes, or an empty vector
  if there was a problem executing the git command.
}
\description{
  Return the last n commit hashes of a git repository
}

