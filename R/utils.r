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