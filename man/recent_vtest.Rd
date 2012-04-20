\name{recent_vtest}
\alias{recent_vtest}
\title{Returns a data frame of recent commits and their associated resultset_hash.}
\usage{
  recent_vtest(dir = ".", start = "", n = 20)
}
\arguments{
  \item{start}{The commit to start searching backward from}

  \item{dir}{The directory with the git repository}

  \item{n}{Maximum number of commits to search}
}
\description{
  For commits without a resultset, the resultset_hash is
  \code{NA}.
}

