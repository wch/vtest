# Run visual tests
#' @export
vtest <- function(pkg = NULL, filter = "", showhelp = TRUE) {
  pkg <- as.package(pkg)
  load_all(pkg)

  set_vtest_pkg(pkg)
  init_vtest(pkg)

  if (!file.exists(get_vtest_dir()))
    stop("Visual test script directory does not exist", get_vtest_dir())

  if (showhelp)
    message("Using test results directory ", get_vtest_resultdir())

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


  resultset_hash <- hash_resultset(get_vtest_resultset())

  # Always save results to last_resultset.csv
  message("Saving resultset to ", get_vtest_lasttest_resultset_file())
  write.csv(cbind(resultset_hash, get_vtest_resultset()),
            get_vtest_lasttest_resultset_file(), row.names = FALSE)

  if (filter == "")
    save_last_resultset()
  else
    message("Did not run the entire set of tests, so the results can't be added to the database.")

  if (showhelp) {
    message("
Run vtest_webpage() to generate web pages for viewing tests.
Run vdiffstat() to see what files have changed.
Run vdiff_webpage() to generate web pages comparing results to those for another
commit in the ", get_vtest_pkg()$package, " repository. (Hide this message with showhelp=FALSE.)")
  }


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
      message("For this commit, old and current resulsets match! Good.")
      return(invisible())
    } else {
      message("For this commit, old and current resultsets do not match!\n  This may be because of changes to R, or to other packages.")
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
  # TODO: find a better way to handle warnings. Right now, if there's a warning,
  # it stops execution of ggsave and the file isn't saved. We need to record
  # that there's a warning (in 'err') and then re-run the ggsave so that the
  # file actually gets written. This is an ugly way to do it. Is it possible
  # to record the warning AND let it finish running?
  tryCatch({ ggsave(temppdf, width = width, height = height, dpi = dpi,
              device = match.fun(device), compress = FALSE) },
           warning = function(w) {
             err <<- "warn"
             ggsave(temppdf, width = width, height = height, dpi = dpi,
               device = match.fun(device), compress = FALSE) },
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
  else
    unlink(cleanpdf)

  # Append the info for this test in the vis_info list
  append_vtest_resultset(context = get_vcontext(), desc = desc,
    type = device, width = width, height = height, dpi = dpi,
    err = err, hash = filehash)

  message(".", appendLF = FALSE)
}
