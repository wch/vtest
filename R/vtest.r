# Set the context of the visual tests

init_vtest <- NULL
set_vtest_pkg <- NULL
get_vtest_pkg <- NULL
get_vtest_dir <- NULL
get_vtest_resultdir <- NULL
get_vtest_imagedir <- NULL
get_vtest_htmldir <- NULL
get_vtest_diffdir <- NULL

get_vtest_lasttest_dir <- NULL
get_vtest_lasttest_resultset_file <- NULL

get_vcontext <- NULL
set_vcontext <- NULL
get_vtest_resultset <- NULL
get_vtest_commits_file <- NULL
get_vtest_resultsets_file <- NULL
append_vtest_resultset <- NULL

local({
  pkg <- NULL       # The package object
  testdir <- NULL   # The dir of the test scripts(usually package/visual_test/)
  resultdir <- NULL # Where the database files are saved
  imagedir <- NULL  # Where the image files are saved

  context <- NULL   # The context of a set of tests (usually in one script)
  context_count <- NULL # Keep count of tests, within this context
  resultset <- NULL  # Information about each test in a context

  # These are used by the top-level vtest function
  get_vtest_pkg <<- function() pkg
  get_vtest_dir <<- function() testdir
  get_vtest_resultdir <<- function() resultdir
  get_vtest_commits_file    <<- function() file.path(resultdir, "commits.csv")
  get_vtest_resultsets_file <<- function() file.path(resultdir, "resultsets.csv")
  get_vtest_imagedir <<- function() file.path(resultdir, "images")
  get_vtest_htmldir  <<- function() file.path(resultdir, "html")
  get_vtest_diffdir  <<- function() file.path(resultdir, "diff")

  get_vtest_lasttest_dir <<- function() file.path(resultdir, "lasttest")
  get_vtest_lasttest_resultset_file <<- function() file.path(resultdir, "lasttest", "resultset.csv")

  # These are used by each test script
  get_vcontext <<- function() context
  set_vcontext <<- function(value) {
    context <<- value
    context_count <<- 0
  }

  init_vtest <<- function(pkg, testdir = NULL, resultdir = NULL) {

    # Close context, if open
    if (!is.null(get_vcontext())) set_vcontext(NULL)

    # Create a zero-row data frame to hold resultset
    cols <- c("context", "desc", "type", "width", "height", "dpi", "err",
              "hash", "order")
    resultset <- setNames(data.frame(t(rep(NA, length(cols)))), cols)
    parent <- parent.env(environment())  # This is where the variables are
    parent$resultset <- resultset[-1, ]
  }

  set_vtest_pkg <<- function(pkg) {
    parent <- parent.env(environment())  # This is where the variables are
    pkg <- as.package(pkg)
    parent$pkg <- pkg

    if (is.null(resultdir))  {
      # If packaage dir is mypath/ggplot2, default result dir is mypath/ggplot2-vtest
      p <- strsplit(pkg$path, "/")[[1]]
      parent$resultdir <-
        paste(c(p[-length(p)], paste(pkg$package, "vtest", sep="-")), collapse="/")
    } else {
      parent$resultdir <- resultdir
    }
    
    if (is.null(testdir))  parent$testdir <- file.path(pkg$path, "visual_test")
    else                   parent$testdir <- testdir
  }

  get_vtest_resultset <<- function() resultset

  # Add information about a single test
  append_vtest_resultset <<- function(context, desc, type, width, height, dpi, err, hash, order) {
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
# TODO: allow set testdir
#' @export
vtest <- function(pkg = NULL, filter = "", showhelp = TRUE) {
  pkg <- as.package(pkg)
  load_all(pkg)

  set_vtest_pkg(pkg)
  init_vtest(pkg)

  if (!file.exists(get_vtest_dir()))
    stop("Visual test script directory does not exist", get_vtest_dir())

  if (showhelp)
    message("Saving test results to directory ", get_vtest_resultdir())

  if (!file.exists(get_vtest_resultdir())) {
    resp <- readline(paste(get_vtest_resultdir(), "does not exist! Create? (y/n) "))
    if (tolower(resp) != "y")
      return(invisible())
    dir.create(get_vtest_resultdir(), recursive = TRUE, showWarnings = FALSE)
  }

  if (!file.exists(get_vtest_imagedir())) {
    resp <- readline(paste(get_vtest_imagedir(), "does not exist! Create? (y/n) "))
    if (tolower(resp) != "y")
      return(invisible())
    dir.create(get_vtest_imagedir(), recursive = TRUE, showWarnings = FALSE)
  }

  if (!file.exists(get_vtest_lasttest_dir()))
    dir.create(get_vtest_lasttest_dir())


  # Run the test scripts
  files <- dir(get_vtest_dir(), full.names = TRUE, include.dirs = FALSE)
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

  resultset_hash <- hash_resultset(get_vtest_resultset())

  # Always save results to last_resultset.csv
  message("Saving result set to lasttest/resultset.csv")
  write.csv(cbind(resultset_hash, get_vtest_resultset()),
            get_vtest_lasttest_resultset_file(), row.names = FALSE)

  if (filter == "")
    save_last_resultset()
  else
    message("Did not run the entire set of tests, so the results can't be added to the database.")

    save_last_resultset()
}


save_last_resultset <- function(prompt = TRUE) {

  # ============ Add to commit tatble ===========

  resultset_hash <- hash_resultset(get_vtest_resultset())
  message("Hash for resultset is ", resultset_hash)

  # Find current commit of package
  commit <- git_find_commit_hash(get_vtest_pkg()$path)
  message("Package ", get_vtest_pkg()$package, " is at commit ", substr(commit, 1, 8))

  # Check for clean working tree
  if (!git_check_clean(get_vtest_pkg()$path)) {
    message("Working tree state is dirty, so results cannot be added to database.")
    return(invisible())
  }
  message("Working tree state is clean, so results can be added to database.")

  # Read existing commit table
  if (file.exists(get_vtest_commits_file()))
    commitdata <- read.csv(get_vtest_commits_file(), stringsAsFactors = FALSE)
  else
    commitdata <- data.frame()

  commitmatch <- commitdata[commitdata$commit == commit, ]
  if (nrow(commitmatch) > 1) {
    stop("More than one matching commit in commit table. This indicates a problem with the database.")

  } else if (nrow(commitmatch) == 1) {
    message("Found existing test results for commit ", substr(commit, 1, 8), ": ",
      paste(commitmatch$resultset_hash, collapse = ", "))

    if (commitmatch$resultset_hash == resultset_hash) {
      message("For this commit, old and current results match! Good.")
      return(invisible())
    } else {
      message("For this commit, old and current results do not match! This may be because of changes to R, or to other packages.")
      if (prompt) {
        reply <- readline("  Replace old results with new results in the commit table? (y/n) ")
        if (tolower(reply) != "y")
          return(invisible())
      }

      commitdata <- rbind(commitdata[commitdata$commit != commit, ],
                      data.frame(commit = commit, resultset_hash = resultset_hash))
    }
  } else {
    if (prompt) {
      reply <- readline("  Results are new. Add them to the commit table? (y/n) ")
      if (tolower(reply) != "y")
        return(invisible())
    }

    commitdata <- rbind(commitdata,
                    data.frame(commit = commit, resultset_hash = resultset_hash))
  }

  # ============== Add to the resultset table ======================

  # Assume that we'll write the resultset data; if certain things happen, set to FALSE
  write_resultset <- TRUE

  # Read existing test results
  if (file.exists(get_vtest_resultsets_file()))
    resultsets <- read.csv(get_vtest_resultsets_file(), stringsAsFactors = FALSE)
  else
    resultsets <- data.frame(resultset_hash = character())

  message("Checking if this resultset is already in resultsets table...")

  # Get the old results that match the current resultset hash (if present)
  # It would be nice to be able to use:
  #   subset(resultsets, resultset_hash == resultset_hash, select = -resultset_hash)
  # but this case is problematic for subset because of re-used var name and
  # because when there are no matches, subset returns a 1-row NA-filled data frame.
  resultset_match <- resultsets[resultsets$resultset_hash == resultset_hash, , drop = FALSE]
  resultset_match <- resultset_match[!(names(resultset_match) %in% "resultset_hash")]

  if (nrow(resultset_match) > 0) {
    message("Found existing resultset with matching hash: ", resultset_hash)
    message("Recalculating existing resultset hash just to make sure... ", appendLF = FALSE)
    resultset_match_hash <- hash_resultset(resultset_match)
    if (resultset_match_hash != resultset_hash)
      stop("Re-hashing old resultset in a different hash value: ",
           resultset_match_hash,
           "\nThis indicates a problem with the resultsets database.")

    message("Hash matches! No need to add this resultset to database.")

  } else {
    message("Did not find existing resultset with matching hash: ", resultset_hash)
    if (prompt) {
      reply <- readline("  Add this resultset to the resultsets table? (y/n) ")
      if (tolower(reply) != "y")
        return(invisible())
    }
    resultsets <- rbind(resultsets, cbind(resultset_hash, get_vtest_resultset()))
  }

  # If we made it this far, write to the commit and resultsets tables
  message("Adding new commit information to commit table.")
  write.csv(commitdata, get_vtest_commits_file(), row.names = FALSE)

  message("Adding new resultset to resultsets table.")
  write.csv(resultsets, get_vtest_resultsets_file(), row.names = FALSE)

  # ============ Copy any new image files over ============
  lasttest_files <- get_vtest_resultset()$hash
  lasttest_files <- lasttest_files[!(lasttest_files %in% list.files(get_vtest_imagedir()))]

  if (length(lasttest_files) > 0) {
    message("Copying ", length(lasttest_files), " new image files to images directory.")
    file.copy(file.path(get_vtest_lasttest_dir(), lasttest_files),
              get_vtest_imagedir())
  }
  return(invisible())
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


# Save an individual test to file, and record information using append_vtest_resultset
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

  # Rename file to hash and move to lasttest_dir
  if (!file.exists(file.path(get_vtest_lasttest_dir(), filehash)))
    file.rename(cleanpdf, file.path(get_vtest_lasttest_dir(), filehash))

  # Append the info for this test in the vis_info list
  append_vtest_resultset(context = get_vcontext(), desc = desc,
    type = device, width = width, height = height, dpi = dpi,
    err = err, hash = filehash)

  message(".", appendLF = FALSE)
}
