# Set the context of the visual tests
set_vtest_pkg <- NULL
get_vtest_pkg <- NULL
set_vtest_path <- NULL
get_vtest_path <- NULL
set_vtest_resultdir <- NULL
get_vtest_resultdir <- NULL
set_vtest_imagedir <- NULL
get_vtest_imagedir <- NULL

get_vcontext <- NULL
set_vcontext <- NULL
init_vresultset <- NULL
get_vresultset <- NULL
append_vresultset <- NULL

local({
  pkg <- NULL       # The package object
  testpath <- NULL  # The path to the test (usually package/visual_test/)
  resultdir <- NULL # Where the database files are saved
  imagedir <- NULL  # Where the image files are saved

  context <- NULL   # The context of a set of tests (usually in one script)
  context_count <- NULL # Keep count of tests, within this context
  resultset <- NULL  # Information about each test in a context

  # These are used by the top-level vtest function
  set_vtest_pkg <<- function(value) pkg <<- value
  get_vtest_pkg <<- function() pkg
  set_vtest_path <<- function (value) testpath <<- value
  get_vtest_path <<- function() testpath
  set_vtest_resultdir <<- function (value) resultdir <<- value
  get_vtest_resultdir <<- function() resultdir
  set_vtest_imagedir <<- function (value) imagedir <<- value
  get_vtest_imagedir <<- function() imagedir

  # These are used by each test script
  get_vcontext <<- function() context
  set_vcontext <<- function(value) {
    context <<- value
    context_count <<- 0
  }

  # Create a zero-row data frame to hold resultset
  init_vresultset <<- function() {
    cols <- c("context", "desc", "type", "width", "height", "dpi", "err",
              "hash", "order")
    resultset <<- setNames(data.frame(t(rep(NA, length(cols)))), cols)
    resultset <<- resultset[-1, ]
  }

  get_vresultset <<- function() resultset

  # Add information about a single test
  append_vresultset <<- function(context, desc, type, width, height, dpi, err, hash, order) {
    # Check that context + description aren't already used
    if (sum(context == resultset$context & desc == resultset$desc) != 0)
      stop(contest, ":\"", desc, "\" cannot be added to resultset because it is already present.")

    context_count <<- context_count + 1

    resultset <<- rbind(resultset,
      data.frame(context = context, desc = desc, type = type, width = width,
        height = height, dpi = dpi, err = err, hash = hash,
        order = context_count, stringsAsFactors = FALSE))
  }

})


# Run visual tests
#' @export
vtest <- function(pkg = NULL, filter = "", resultdir = NULL, showhelp = TRUE) {
  pkg <- as.package(pkg)
  load_all(pkg)

  set_vtest_pkg(pkg)

  test_path <- file.path(pkg$path, "visual_test")
  if (!file.exists(test_path)) 
    return()

  set_vtest_path(test_path)

  if (is.null(resultdir))
    resultdir <- find_default_resultdir()

  imagedir <- file.path(resultdir, "images")

  set_vtest_resultdir(resultdir)
  set_vtest_imagedir(imagedir)

  if (showhelp)
    message("Saving test results to directory ", resultdir)

  if (!file.exists(resultdir)) {
    resp <- readline(paste(resultdir, "does not exist! Create? (y/n) "))
    if (tolower(resp) != "y")
      return(invisible())

    dir.create(resultdir, recursive = TRUE, showWarnings = FALSE)
    dir.create(imagedir, recursive = TRUE, showWarnings = FALSE)
  } else if (!file.exists(imagedir)) {
    resp <- readline(paste(imagedir, "does not exist! Create? (y/n) "))
    if (tolower(resp) != "y")
      return(invisible())
    dir.create(imagedir, recursive = TRUE, showWarnings = FALSE)
  }

  init_vresultset()

  # Run the test scripts
  files <- dir(test_path, full.names = TRUE, include.dirs = FALSE)
  files <- files[grepl("\\.[rR]$", files)]
  files <- match_filter(files, filter)
  files <- files[order(files)]
  lapply(files, source)


#  f_quote <- ifelse(is.null(filter), '', paste('filter="', filter, '"', sep = ""))
#  if (showhelp) {
#    message("\nRun vtest_webpage(", f_quote, ") to generate web pages for viewing tests.\n",
#      "Run vdiffstat(", f_quote, ") to see what files have changed.\n",
#      "Run vdiff_webpage(", f_quote,
#      ") to generate web pages comparing results to another commit in the git repository.\n",
#      "If you have added new tests, remember to add the output files to the git repository.\n",
#      "(Hide this message with showhelp=FALSE.)")
#  }


  # ============ Hash resultset and save to last_resultset.csv ===========

  # If running the full battery of tests, then we can hash the entire test set
  # and compare it to the test set table
  resultset_hash <- hash_resultset(get_vresultset())

  # Always save results to last_resultset.csv
  message("Saving test results to last_resultset.csv")
  write.csv(cbind(resultset_hash, get_vresultset()),
    file.path(resultdir, "last_resultset.csv"), row.names = FALSE)

  # TODO: turn this into function
  # ============ Check hash of testset results ===========

  commit <- git_find_commit_hash(pkg$path)
  clean_repo <- git_check_clean(pkg$path)

  # Assume that we'll write the commit data; if certain things happen, set to FALSE
  write_commitdata <- TRUE
  # Assume that we'll write the resultset data; if certain things happen, set to FALSE
  write_resultset <- TRUE

  if (filter != "") {
    write_commitdata <- FALSE
    write_resultset <- FALSE
    message("Did not run the entire set of tests, so the results can't be added to the database.")
  }

  message("Hash for vtest results is ", resultset_hash)
  message(pkg$package, " is at commit ", commit)
  if (clean_repo) {
    message("Working tree state is clean, so results can be added to database.")
  } else {
    message("Working tree state is dirty, so results cannot be added to database.")
    write_commitdata <- FALSE
    write_resultset   <- FALSE
  }

  # Read existing commit test results
  if (file.exists(file.path(resultdir, "commits.csv")))
    commitdata <- read.csv(file.path(resultdir, "commits.csv"))
  else
    commitdata <- data.frame()

  commitmatch <- commitdata$commit == commit
  if (any(commitmatch)) {
    message("Previous results for commit ", substr(commit, 1, 6), " found: ",
      paste(commitdata$resultset_hash[commitmatch], collapse = ", "))

    if (sum(commitmatch) > 1)
      stop("More than one matching commit in database. This indicates a problem with the database.")

    if (commitdata$resultset_hash == resultset_hash) {
      message("Old and current results match! Good.")
      write_commitdata <- FALSE
    } else {
      message("Old and current results do not match! This may be because of changes to R, or to other packages.")
      if (write_commitdata) {
        reply <- readline("Replace old test result data with new test result data? (y/n) ")
        if (tolower(reply) != "y")
          write_commitdata <- FALSE
        else {
          commitdata <- commitdata[-commitmatch, ]
          commitdata <- rbind(commitdata, data.frame(commit = commit,
                                                     resultset_hash = resultset_hash))
        }
      }
    }
  } else {
    commitdata <- rbind(commitdata, data.frame(commit = commit,
                                               resultset_hash = resultset_hash))

    reply <- readline("Results are new. Would you like to add them to the database? (y/n) ")
    if (tolower(reply) != "y") {
      write_commitdata <- FALSE
      write_resultset <- FALSE
    }
  }

  if (write_commitdata) {
    message("Writing result hash to commit database.")
    write.csv(commitdata, file.path(resultdir, "commits.csv"), row.names = FALSE)
  }

  # TODO: turn this into function
  # ============== Add to the resultset table ======================

  # Read existing test results
  if (file.exists(file.path(resultdir, "resultsets.csv")))
    resultsets <- read.csv(file.path(resultdir, "resultsets.csv"), stringsAsFactors = FALSE)
  else
    resultsets <- data.frame(resultset_hash = character())

  # Get the old results that match the current resultset hash (if present)
  # It would be nice to be able to use:
  #   subset(resultsets, resultset_hash == resultset_hash, select = -resultset_hash)
  # but this case is very problematic for subset because of re-used var name and
  # because when there are no matches, subset returns a 1-row NA-filled data frame.
  resultset_match <- resultsets[resultsets$resultset_hash == resultset_hash, , drop = FALSE]
  resultset_match <- resultset_match[!(names(resultset_match) %in% "resultset_hash")]

  if (nrow(resultset_match) > 0) {
    message("Existing results found for resultset hash ", resultset_hash)
    message("Checking existing result hash just to make sure... ", appendLF = FALSE)
    resultset_match_hash <- hash_resultset(resultset_match)
    if (resultset_match_hash != resultset_hash)
      stop("Re-hashing old resultset results in a different hash value: ",
           resultset_match_hash,
           "\nThis indicates a problem with the resultset database.")

    message("Hash matches!")
    message("No need to add new resultset to database.")
  } else {
    message("No existing results found for resultset hash ", resultset_hash)
    if (write_resultset) {
      message("Adding new resultset to database.")

      resultsets <- rbind(resultsets, cbind(resultset_hash, get_vresultset()))
      write.csv(resultsets, file.path(resultdir, "resultsets.csv"), row.names = FALSE)
    }
  }
}


# Start a visual test context
#' @export
vcontext <- function(context) {
  if (!is.null(get_vcontext()))
    stop("Can't open new context while current context is still open. Use end_vcontext().")

  set_vcontext(context)
  message(context, appendLF = FALSE)
}


# Finish a visual test context.
#' @export
end_vcontext <- function() {
  if(is.null(get_vcontext())) {
    message("No open vcontext to end.")
    return(invisible())
  }

  set_vcontext(NULL)  # Reset the context
  message("")         # Print a newline
}


# Save an individual test to file, and record information using append_vresultset
# This presently only works with pdf; other file types will fail
# * desc: a short description of the test
# * filename: output filename (not including extension, like ".pdf"). If NULL, use MD5
#     hash of `desc` as the filename.
# * width: width in inches
# * height: height in inches
# * dpi: pixels per inch (OK, it really should be ppi)
# * device: string with name of output device. Only "pdf" is supported now.
# * err: error status. ok, warn, or error
# * hash: a hash of the file contents
#' @export
save_vtest <- function(desc = NULL, width = 4, height = 4, dpi = 72, device = "pdf") {
  if (is.null(get_vcontext()))     stop("Must have active vcontext")
  if (is.null(desc) || desc == "") stop("desc must not be empty")

  if (device == "pdf")  dpi <- NA
  else                  stop('Only "pdf" device supported at this time')

  err <- "ok"  # Use this to track if there's a warning or error when using ggsave

  # Save the pdf to a temporary file
  temppdf <- tempfile("vtest")
  tryCatch({ ggsave(temppdf, width = width, height = height, dpi = dpi,
               device = match.fun(device), compress = FALSE) },
           warning = function(w) { err <<- "warn"; warning(w) },
           error   = function(e) { err <<- "error"; warning(e) })

  # Zero out the dates and write modified PDF file to the output dir
  cleanpdf <- tempfile("vtest_cleaned")
  zero_pdf_date(temppdf, cleanpdf)

  unlink(temppdf)  # Remove the file in the temp dir

  # Get a hash of the file contents
  filehash <- digest(cleanpdf, file = TRUE)
  if (!file.exists(file.path(get_vtest_imagedir(), filehash)))
    file.rename(cleanpdf, file.path(get_vtest_imagedir(), filehash))

  # Append the info for this test in the vis_info list
  append_vresultset(context = get_vcontext(), desc = desc,
    type = device, width = width, height = height, dpi = dpi,
    err = err, hash = filehash)

  message(".", appendLF = FALSE)
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

