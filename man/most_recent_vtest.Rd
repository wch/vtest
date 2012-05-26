\name{most_recent_vtest}
\alias{most_recent_vtest}
\title{Returns a one-row data frame of the most recent commit that has a
resultset hash.}
\usage{
  most_recent_vtest(dir = ".", start = "",
    main_branch = TRUE)
}
\arguments{
  \item{start}{The commit to start searching backward from}

  \item{dir}{The directory with the git repository}

  \item{n}{Maximum number of commits to search}

  \item{main_branch}{Don't show commits on branches that
  were merged in.}
}
\description{
  Returns a one-row data frame of the most recent commit
  that has a resultset hash.
}

