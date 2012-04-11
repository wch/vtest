\name{init_vtest}
\alias{init_vtest}
\title{Initialize the environment for vtest to run}
\usage{
  init_vtest(pkg = NULL, testdir = NULL, resultdir = NULL)
}
\arguments{
  \item{pkg}{A package object, or path to the package}

  \item{testdir}{The directory with the test scripts. If
  \code{NULL}, it will be set to packagedir/visual_test/.}

  \item{resultdir}{The directory that results are stored
  in. If \code{NULL}, it will be set to
  packagedir/visual_test/vtest/.}
}
\description{
  Initialize the environment for vtest to run
}

