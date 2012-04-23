#' vtest
#'
#' @name vtest
#' @docType package
#' @aliases vtest package-vtest
#' @import devtools plyr digest reshape2 stringr
NULL

#' Run visual tests
#'
#' @param pkg a path to the package, or a package object
#' @param filter only tests with file names matching this
#'   regular expression will be executed.
#' @param showhelp if \code{TRUE}, print helpful messages.
#'
#' @export
vtest <- function(pkg = NULL, filter = "", showhelp = TRUE) {
  pkg <- as.package(pkg)
  load_all(pkg, reset = TRUE)

  init_vtest(pkg)
  reset_lasttest()

  if (!file.exists(get_vtest_testdir()))
    stop("Visual test script directory does not exist", get_vtest_testdir())

  if (showhelp)
    message("Using test results directory ", get_vtest_resultdir())

  # Run the test scripts
  files <- dir(get_vtest_testdir(), full.names = TRUE, include.dirs = FALSE)
  files <- files[grepl("\\.[rR]$", files)]
  files <- match_filter(files, filter)
  files <- files[order(files)]
  lapply(files, function(f) {vt$.file_parsing <- f; source(f)})

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


#' Saves the last resultset to the database, if possible
#'
#' @export
save_last_resultset <- function(prompt = TRUE) {

  last_resultset <- load_lastresultset()
  # Drop the hash column
  last_resultset <- last_resultset[!(names(last_resultset) %in% "resultset_hash")]

  # ============ Add to commit tatble ===========

  resultset_hash <- hash_resultset(last_resultset)
  message("Hash for resultset is ", resultset_hash)

  # Find current commit of package
  commit <- git_find_commit_hash(get_vtest_pkg()$path)
  message("Package ", get_vtest_pkg()$package, " is at commit ", git_abbrev_hash(commit))

  # Check for clean working tree
  if (!git_check_clean(get_vtest_pkg()$path)) {
    message("Working tree state is dirty, so results cannot be added to database.")
    return(invisible())
  }
  message("Working tree state is clean, so results can be added to database.")

  # Read existing commit table
  commitdata <- load_commits_table()

  # Assume this commit is new unles otherwise
  commit_status <- "new"

  commitmatch <- commitdata[commitdata$commit == commit, ]
  if (nrow(commitmatch) > 1) {
    stop("More than one matching commit in commit table. This indicates a problem with the database.")

  } else if (nrow(commitmatch) == 1) {
    message("Found existing test results for commit ", git_abbrev_hash(commit), ": ",
      paste(commitmatch$resultset_hash, collapse = ", "))

    if (commitmatch$resultset_hash == resultset_hash) {
      message(" Old and current resulsets match! No need to add to database.")
      return(invisible())
    } else {
      message("  For this commit, old and current resultsets do not match!\n  This may be because of changes to R, or to other packages.")
      if (prompt) {
        if (confirm("Replace old results with new results in the commit table? (y/n) "))
          commit_status <- "replace"
        else
          return(invisible())
      }

      commitdata <- rbind(commitdata[commitdata$commit != commit, ],
                      data.frame(commit = commit, resultset_hash = resultset_hash))
    }
  } else {
    message("  No existing resultset found for this commit. It can be added to commit table.")

    commitdata <- rbind(commitdata,
                    data.frame(commit = commit, resultset_hash = resultset_hash))
  }

  # ============== Add to the resultset table ======================

  # Assume that resultset is new unless found otherwise
  resultset_status <- "new"

  # Read existing test results
  resultsets <- load_resultsets()

  message("Checking if this resultset is already in resultsets table...")

  # Get the old results that match the current resultset hash (if present)
  resultset_match <- extract_resultset(load_resultsets(), resultset_hash, drop_hash = TRUE) 

  if (nrow(resultset_match) > 0) {
    message("Found existing resultset with matching hash: ", resultset_hash)
    message("  Recalculating existing resultset hash just to make sure... ", appendLF = FALSE)
    resultset_match_hash <- hash_resultset(resultset_match)
    if (resultset_match_hash != resultset_hash)
      stop("  Re-hashing old resultset in a different hash value: ",
           resultset_match_hash,
           "\nThis indicates a problem with the resultsets database.")

    resultset_status <- "old"
    message("  Hash matches! No need to add this resultset to database.")

  } else {
    message("  Did not find existing resultset with this hash. It can be added to the resultsets table.")

    resultsets <- rbind(resultsets, cbind(resultset_hash, last_resultset))
  }


  if(prompt) {
    message("You can save the resultset to the database now. If you choose not to save ",
        "now, you can save it later by running save_last_resultset().")
    if (commit_status == "replace" && resultset_status == "new")
      resp <- readline("Replace commit and add new resultset to database? (y/n) ")
    else if (commit_status == "replace" && resultset_status == "old")
      resp <- readline("Replace commit in database? (y/n) ")
    else if (commit_status == "new" && resultset_status == "new")
      resp <- readline("Add new commit and new resultset to database? (y/n) ")
    else if (commit_status == "new" && resultset_status == "old")
      resp <- readline("Add new commit to database? (y/n) ")
    else
      stop("Unexpected status combination.")

    if (tolower(resp) != "y") {
      message("Changes to database not saved. If you decide later to save the results, run save_last_resultset().")
      return(invisible())
    }
  }

  # TODO: modularize this part
  # If we made it this far, write to the commit and resultsets tables
  message("Adding commit to commit table.")
  write.csv(commitdata, get_vtest_commits_file(), row.names = FALSE)

  message("Adding resultset to resultsets table.")
  write.csv(resultsets, get_vtest_resultsets_file(), row.names = FALSE)

  # ============ Copy any new image files over ============
  lasttest_files <- load_lastresultset()$hash
  lasttest_files <- lasttest_files[!(lasttest_files %in% list.files(get_vtest_imagedir()))]

  if (length(lasttest_files) > 0) {
    message("Copying ", length(lasttest_files), " new image files to images directory.")
    file.copy(file.path(get_vtest_lasttest_dir(), lasttest_files),
              get_vtest_imagedir())
  }
  return(invisible())
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

  # Run the test and mark if warning or error
  withCallingHandlers({ ggsave2(temppdf, plot = last_plot(),
              width = width, height = height, dpi = dpi,
              device = match.fun(device), compress = FALSE) },
          warning = function(w) { err <<- "warn" },
          error   = function(e) { err <<- "error" })

  # Zero out the dates/producer and write modified PDF file to the output dir
  cleanpdf <- tempfile("vtest_cleaned")
  zero_pdf_info(temppdf, cleanpdf)

  unlink(temppdf)  # Remove the file in the temp dir

  # Get a hash of the file contents
  filehash <- digest(cleanpdf, file = TRUE)

  # Rename file to hash and move to lasttest_dir
  if (!file.exists(file.path(get_vtest_lasttest_dir(), filehash)))
    file.rename(cleanpdf, file.path(get_vtest_lasttest_dir(), filehash))
  else
    unlink(cleanpdf)

  inc_vcontext_count()

  # Append the info for this test to the resultset
  append_vtest_resultset(context = get_vcontext(), desc = desc,
    type = device, width = width, height = height, dpi = dpi,
    err = err, hash = filehash, order = get_vcontext_count())

  message(".", appendLF = FALSE)
}
