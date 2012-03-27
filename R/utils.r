# =============================================================
# Utility functions
# =============================================================

# For a character vector x, return the *values* that have matches in
# character vector 'filter'
match_filter <- function(x, filter) {
  x[match_filter_idx(x, filter)]
}

# For a character vector x, return the *indices* that have matches in
# character vector 'filter'
match_filter_idx <- function(x, filter) {
  # Get indices of 'x' that have matches in 'filter'
  idx <- unlist(lapply(filter, grep, x))
  unique(idx)
}


# Call system2, but capture both the exit code and the stdout+stderr
# Supposedly in the next version of R, system2 will return this information.
# This function also will go to `rundir` before running the command, then return
# to the starting dir.
# There are some tricks used to capture the stdout+stderr.
# I tried this but it didn't work:
# http://stackoverflow.com/questions/7014081/capture-both-exit-status-and-output-from-a-system-call-in-r
systemCall <- function(commands, args = character(), stdin = "", input = NULL,
                env = character(), wait = TRUE, rundir = NULL) {
  output <- ""  # Need to set variable in case there's no output
  status <- 0
  warn <- NULL

  # Save current dir and go to rundir before running the command
  if (!is.null(rundir)) {
    startdir <- getwd()
    setwd(rundir)
  }

  # For some reason we need to save the stdout/stderr to a file to properly
  # capture it. (The stackoverflow answer doesn't work for me)
  tempfile <- tempfile("systemCall")

  status <- system2(commands, args, stdout = tempfile, stderr = tempfile,
                    stdin, input, env, wait)

  # Read in the output from stdin/stderr
  tempfile_fd <- file(tempfile, "r")
  output <- readChar(tempfile_fd, 1048576)  # Max 1MB from stdout/stderr
  close(tempfile_fd)
  unlink(tempfile)

  # Return to the starting directory
  if (!is.null(rundir))  setwd(startdir)

  return(list(status = status, output = output))
}


# Print a prompt and ask for confirmation
confirm <- function(prompt = "", confirm = "y", ignoreLF = TRUE, ignorecase = TRUE) {
  resp <- readline(prompt)
  while (ignoreLF && resp == "")
    resp <- readline(prompt)

  if (resp == confirm)
    return(TRUE)
  else if (ignorecase && tolower(resp) == tolower(confirm))
    return(TRUE)
  else
    return(FALSE)
}


# Adapted from staticdocs
copy_css <- function(base_path) {
  css <- file.path(inst_path(), "css")
  file.copy(dir(css, full.names = TRUE), base_path, recursive = TRUE)
}


# Borrowed this from staticdocs
inst_path <- function() {
  srcref <- attr(vtest, "srcref")

  if (is.null(srcref)) {
    # Probably in package
    system.file(package = "vtest")
  } else {
    # Probably in development
    file.path(dirname(dirname(attr(srcref, "srcfile")$filename)),
      "inst")
  }
}