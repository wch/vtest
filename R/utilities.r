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

# For a PDF, modify the CreationDate and ModDate (lines 5 and 6)
# so that the files are exactly the same, regardless of date + time they
# were created. The output must be written to a different file.
zero_pdf_date <- function(infile = NULL, outfile = NULL) {
  if (is.null(infile) || is.null(outfile))
    stop("Can't operate on NULL infile or outfile")

  infile_fd <- file(infile, "r")
  pdftext <- readLines(infile_fd)
  close(infile_fd)

  if (!grepl("^/CreationDate ", pdftext[5]) || !grepl("^/ModDate ", pdftext[6]))
    stop("Unexpected structure of PDF file. CreationDate or ModDate not found in right place in ",
         infile)

  pdftext[5] <- "/CreationDate (D:00000000000000)"
  pdftext[6] <- "/ModDate (D:00000000000000)"

  outfile_fd <- file(outfile, "w")
  writeLines(pdftext, outfile_fd)
  close(outfile_fd)
}

# This converts files to PNG, making use of a temporary PNG cache.
# It assumes the file name is a cryptographic hash of the contents --
# that is, each different filename has unique content, and vice versa.
convert_png_cached <- function(filenames, indir = NULL, outdir = NULL, method = "ghostscript") {
  if (length(filenames) == 0) return()
  if (any(dirname(filenames) != "."))
    stop("filenames must not have a leading path")

  cachedir <- file.path(get_vtest_resultdir(), "pngcache")
  if (!file.exists(cachedir))  dir.create(cachedir)
  allcached <- sub("\\.png$", "", dir(cachedir))

  cached   <- filenames[ (filenames %in% allcached)]
  uncached <- filenames[!(filenames %in% allcached)]

  message(length(filenames), " PNG files requested. ", length(cached), " already in PNG cache.")
  convert_png(uncached, indir, cachedir, method)

  file.copy(file.path(cachedir, paste(filenames, ".png", sep = "")), outdir)
}


# Generate the PNG images for a directory
convert_png <- function(filenames, indir = NULL, outdir = NULL, method = "ghostscript") {
  if (length(filenames) == 0) return()
  infiles <- unique(filenames)  # Remove duplicates
  outfiles <- paste(infiles, ".png", sep="")

  # Prepend paths if specified; otherwise assume that 'filenames' has a full path
  if (!is.null(indir))   infiles  <- file.path(indir, infiles)
  if (!is.null(outdir))  outfiles <- file.path(outdir, outfiles)

  message("Converting ", length(infiles), " files to PNG, using method ", method)

  # Convert multiple PNGs by building a command string like this:
  # convert \( a.pdf -write a.png +delete \) \( b.pdf -write b.png +delete \) null:

  if (method == "ghostscript") {
    for (i in seq_along(infiles)) {
      system2("gs", c("-dNOPAUSE", "-dBATCH", "-sDEVICE=png16m", "-r72",
        "-dTextAlphaBits=4", "-dGraphicsAlphaBits=4",
        paste("-sOutputFile=", outfiles[i], sep=""), infiles[i]), stdout = TRUE)
    }

  } else if (method == "imagemagick") {
    args <- NULL
    for (i in seq_along(infiles)) {
      args <- c(args, "\\(", infiles[i], "-density", "72x72", "-write", outfiles[i],
               "+delete", "\\)")
    }

    # Need the these "null:" to suppress convert warnings, for some reason
    args <- c(args, "null:", "null:")

    system2("convert", args)

  } else {
    stop("Unknown method.")
  }
}


# Compare png files
compare_png <- function(files1, files2, filesout) {
  if (length(files1) == 0) return()
  message("Comparing ", length(files1), " pairs of images")

  # Not sure how to build a single command line string to compare (as was done
  #   with convert in convert_pdf2png), so do them individually.
  for (i in seq_along(files1)) {
    system2("compare", c("-dissimilarity-threshold", "1", files1[i], files2[i], filesout[i]))
  }
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

# Find the current git commit hash of a directory (must be top level of repo)
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
