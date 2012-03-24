# Set the context of the visual tests
set_vtest_pkg <- NULL
get_vtest_pkg <- NULL
set_vtest_path <- NULL
get_vtest_path <- NULL
set_vtest_outdir <- NULL
get_vtest_outdir <- NULL

get_vcontext <- NULL
set_vcontext <- NULL
init_vtestinfo <- NULL
get_vtestinfo <- NULL
append_vtestinfo <- NULL

local({
  pkg <- NULL      # The package object
  testpath <- NULL # The path to the test (usually package/visual_test/)
  outdir <- NULL   # Where the output files are saved

  context <- NULL  # The context of a set of tests (usually in one script)
  testinfo <- NULL # Information about each test in a context

  # These are used by the top-level vtest function
  set_vtest_pkg <<- function(value) pkg <<- value
  get_vtest_pkg <<- function() pkg
  set_vtest_path <<- function (value) testpath <<- value
  get_vtest_path <<- function() testpath
  set_vtest_outdir <<- function (value) outdir <<- value
  get_vtest_outdir <<- function() outdir

  # These are used by each test script
  get_vcontext <<- function() context
  set_vcontext <<- function(value) {
    context <<- value
  }

  # TODO: Get rid of this...
  init_vtestinfo <<- function() testinfo <<- data.frame()

  get_vtestinfo <<- function() testinfo

  # Add information about a single test
  append_vtestinfo <<- function(value) {
    # Check that description hash isn't already used
    if (sum(value$deschash == testinfo$deschash) != 0)
      stop("Hash ", value$deschash, " cannot be added because it is already present.")

    testinfo <<- rbind(testinfo, cbind(value, data.frame(order = nrow(testinfo)+1)))
  }

})


# Run visual tests
#' @export
vtest <- function(pkg = NULL, filter = NULL, outdir = NULL, showhelp = TRUE) {
  pkg <- as.package(pkg)
  load_all(pkg)

  set_vtest_pkg(pkg)

  test_path <- file.path(pkg$path, "visual_test")
  if (!file.exists(test_path)) 
    return()

  set_vtest_path(test_path)


  if (is.null(outdir)) {
    # Default output directory would be ggplot2/../ggplot2-vtest
    p <- strsplit(pkg$path, "/")[[1]]
    outdir <- paste(c(p[-length(p)], paste(pkg$package, "vtest", sep="-")),
                collapse="/")
  }

  set_vtest_outdir(outdir)

  if (showhelp)
    message("Saving output to directory ", outdir)

  if (!file.exists(outdir)) {
    resp <- readline(paste(outdir, "does not exist! Create? (y/n) "))
    if (tolower(resp) != "y")
      return(invisible())

    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }


  init_vtestinfo()

  # Run the test scripts
  files <- dir(test_path, filter, full.names = TRUE, include.dirs = FALSE)
  files <- files[grepl("\\.[rR]$", files)]
  lapply(files, source)

  f_quote <- ifelse(is.null(filter), '', paste('filter="', filter, '"', sep = ""))
  if (showhelp) {
    message("\nRun vtest_webpage(", f_quote, ") to generate web pages for viewing tests.\n",
      "Run vdiffstat(", f_quote, ") to see what files have changed.\n",
      "Run vdiff_webpage(", f_quote,
      ") to generate web pages comparing results to another commit in the git repository.\n",
      "If you have added new tests, remember to add the output files to the git repository.\n",
      "(Hide this message with showhelp=FALSE.)")
  }


  # ============ Check hash of testset results ===========


  # If running the full battery of tests, then we can hash the entire test set
  # and compare it to the test set table
  testset_hash <- digest(get_vtestinfo())
  # TODO: Make sure the data frame is always the same before hashing it
  #  (will unimportant info like rownames alter the hash?)
  commit <- git_find_commit_hash(pkg$path)
  clean_repo <- git_check_clean(pkg$path)

  # Assume that we'll write the commit data; if certain things happen, set to FALSE
  write_commitdata <- TRUE

  message("Hash for vtest results is ", testset_hash)
  message(pkg$package, " is at commit ", commit)
  if (clean_repo) {
    message("Working tree state is clean, so results can be added to vtest database.")
  } else {
    message("Working tree state is dirty, so results cannot be added to vtest database.")
    write_commitdata <- FALSE
  }

  # Read existing commit test results
  if (file.exists(file.path(outdir, "commits.csv")))
    commitdata <- read.csv(file.path(outdir, "commits.csv"))
  else
    commitdata <- data.frame()

  commitmatch <- commitdata$commit == commit
  if (any(commitmatch)) {
    message("Previous results for commit ", substr(commit, 1, 6), " found: ",
      paste(commitdata$testset_hash[commitmatch], collapse = ", "))

    if (sum(commitmatch) > 1)
      stop("More than one matching commit in database. This indicates a problem with the database.")

    if (commitdata$testset_hash == testset_hash) {
      message("Old and current results match! Good.")
    } else {
      message("Old and current results do not match! This may be because of changes to R, or to other packages.")
      if (write_commitdata) {
        reply <- readline("Replace old test result data with new test result data? (y/n) ")
        if (tolower(reply) != "y")
          write_commitdata <- FALSE
        else
          commitdata <- commitdata[-commitmatch, ]
      }
    }
  }

  commitdata <- rbind(commitdata, data.frame(commit = commit,
                                             testset_hash = testset_hash))

  if (write_commitdata) {
    message("Writing to result hash to commit database.")
    write.csv(commitdata, file.path(outdir, "commits.csv"), row.names = FALSE)
  }
}


# Start a visual test context
#' @export
vcontext <- function(context) {
  if (!is.null(get_vcontext()))
    stop("Can't open new context while current context is still open. Use end_vcontext().")

  set_vcontext(context)
  message(context, appendLF = FALSE)

  destdir <- file.path(get_vtest_path(), context)
  unlink(dir(destdir, full.names = TRUE))
  dir.create(destdir, showWarnings = FALSE)
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


# Save an individual test to file, and record information using append_vtestinfo
# This presently only works with pdf; other file types will fail
# * desc: a short description of the test
# * filename: output filename (not including extension, like ".pdf"). If NULL, use MD5
#     hash of `desc` as the filename.
# * width: width in inches
# * height: height in inches
# * dpi: pixels per inch (OK, it really should be ppi)
# * device: string with name of output device. Only "pdf" is supported now.
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
  file.rename(cleanpdf, file.path(get_vtest_outdir(), filehash))

  # Append the info for this test in the vis_info list
  append_vtestinfo(data.frame(context = get_vcontext(), desc = desc,
    type = device, width = width, height = height, dpi = dpi,
    err = err, hash = filehash, stringsAsFactors = FALSE))

  message(".", appendLF = FALSE)
}



# =============================================================
# Functions for generating web pages to view tests
# =============================================================

# This is the function that the user calls
# * convertpng: if TRUE, convert the source PDFs files to PNG instead.
# TODO: Create overall index file?
#' @export
vtest_webpage <- function(pkg = NULL, filter = "", convertpng = TRUE) {
  pkg <- as.package(pkg)

  test_path <- file.path(pkg$path, "visual_test")
  if (!file.exists(test_path))
    return()

  # Find subdirs with testinfo.dat - these are where html files will be made
  dirs <- dirname(list.files(test_path, pattern = "testinfo.dat",
                             recursive = TRUE))

  dirs <- dirs[grepl(filter, dirs)]
  dirs <- dirs[!grepl("^diff/", dirs)]  # Ignore diff dir
  dirs <- dirs[!grepl("^html/", dirs)]  # Ignore html dir

  for(d in dirs) {
    make_vtest_webpage(file.path(test_path, d),
      outdir = file.path(test_path, "html", d), convertpng = convertpng)
  }

  # Copy the css file
  file.copy(file.path(test_path, "style.css"), file.path(test_path, "html"),
            overwrite = TRUE)
  invisible()
}


# Make a single web page (user shouldn't use this function)
# TODO: display filename if it differs from hash
make_vtest_webpage <- function(dir = NULL, outdir = NULL, convertpng = TRUE) {
  if (is.null(dir))     stop("dir cannot be  NULL")
  if (is.null(outdir))  stop("outdir cannot be  NULL")

  # Read in the information about the tests
  testinfo <- dget(file.path(dir, "testinfo.dat"))

  # Sort by order
  testinfo <- testinfo[order(testinfo$order), ]

  unlink(outdir, recursive= TRUE)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  htmlfile <- file.path(normalizePath(outdir), "index.html")
  message("Writing ", htmlfile)

  # Get the name of the subdirectory of visual tests
  vname <- strsplit(dir, "/")[[1]]
  vname <- vname[length(vname)]

  write(paste('<html><head>\n',
              '<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />',
              '<title>Visual tests: ', vname,
              '</title></head><body><h1>Visual tests: ', vname,
              '</h1>\n', sep = ""), htmlfile)

  # Write HTML code to show a single test
  item_html <- function(t, convertpng = FALSE) {
    if (convertpng) f <- sub("\\.pdf$", "\\.png", t$filename)
    else            f <- t$filename

    paste('<div class="float">\n',
          '  <div class="header">',
          '    <p class="description">', t$desc, '</p>\n',
          '    <p class="hash">', t$hash, '</p>', '</div>\n',
          '  <div class="imageset">\n',
          '    <span class="imagewrap">\n',
          '      <div class="image"><img src="', f, '"></div>\n',
          '    </span>\n',
          '  </div>\n',
          '</div>\n', sep="")
  }

  # Get the list of info about all tests, then write information about each of the items
  for (i in seq_len(nrow(testinfo))) {
    write(item_html(testinfo[i, ], convertpng), htmlfile, append = TRUE)
  }

  write('</body></html>', htmlfile, append = TRUE)

  if (convertpng)
    convert_pdf2png(testinfo$filename, dir, outdir)
  else
    file.copy(file.path(dir, testinfo$filename), outdir)

}



# =============================================================
# Functions for generating visual diff pages
# =============================================================


# Find files modified between ref1 and ref2
# If ref2 is "" (the working tree), then we don't know exactly which of the new files
# the user plans to commit. So we just assume all new files in the working tree are
# added files (marked with A).
#' @export
vdiffstat <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "", showhelp = TRUE) {
  pkg <- as.package(pkg)

  if (ref1 == "")  stop('ref1 must not be blank "" (because git doesn\'t like it)')

  ref2text <- ifelse(ref2 == "", "working tree", ref2)
  message("Comparing ", ref1, " to ", ref2text);
  if (ref2 == "")
    message("The status of Added files is a guess when using working tree.\n",
      "  (All new files are reported as Added.)")

  gitstat <- systemCall("git", c("diff", "--name-status", ref1, ref2),
                        rundir = pkg$path)

  if (gitstat$status != 0) {
    # Git failed to run for some reason. it would be nice to print the output,
    # but we can't because of issues with system2 in systemCall
    stop("git returned code ", gitstat$status, ". Make sure you use valid git commit refs.")
  } else if (length(gitstat$output) == 0) {
    # There were no changes; create an empty data frame
    changed <- data.frame(V1=character(), V2=character())
  } else {
    # There were some changes
    changed <- read.table(con <- textConnection(gitstat$output), stringsAsFactors = FALSE)
    close(con)
  }
  changed <- setNames(changed, c("status", "filename"))
  changed <- subset(changed, grepl("^visual_test/", filename))

  # Special case where ref2 is the working tree. This is a bit hacky. Add all
  # the untracked files in the visual_test dir. Because they're not committed, we
  # can't tell exactly which files *should* be compared. So copy all the untracked
  # files over.
  if (ref2 == "") {
    wfiles <- systemCall("git", c("ls-files", "--other", "--exclude-standard", "visual_test/"),
                rundir = pkg$path)

    if (length(wfiles) > 0)
      changed <- rbind(changed, data.frame(status = "A", filename = wfiles$output,
                                           stringsAsFactors = FALSE))
  }

  if (nrow(changed) == 0) return(changed)

  # use 'filter' on the second part of the path (right after visual_test/)
  cpaths <- strsplit(changed$filename,"/")
  changed[grepl(filter, sapply(cpaths, "[[", 2)), ]
}


# Make visual diff from two refs
# TODO: Create overall index file, with status
#' @export
vdiff_webpage <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "",
      convertpng = TRUE, method = "ghostscript", prompt = TRUE) {
  # TODO: message about weird color space in conversion using convert
  # TODO: print message about png option, and slow png vs safari-only pdf
  # TODO: display filename if it differs from hash

  pkg <- as.package(pkg)

  test_path <- file.path(pkg$path, "visual_test")

  # TODO: de-hard code this?
  cssfile <- file.path(test_path, "style.css")

  if (ref1 == "")  stop('ref1 must not be blank "" (because git doesn\'t like it)')

  # Check we're in top level of the repo
  if (pkg$path != systemCall("git", c("rev-parse", "--show-toplevel"),
                    rundir = pkg$path)$output)
    stop("The path of pkg must also be the top level of the git tree.")

  if (prompt) {
    resp <- readline(paste("This will unstage changes to the git index, so if you have staged any changes",
      "with 'git add' or similar commands, you will need to restage them. This will",
      "not change files in the working tree. (Use `prompt=FALSE` to disable this messsage.)",
      "Continue? (y/n) ", sep="\n"))
    if (tolower(resp) != "y")
      return(invisible())
  }

  # The directories for ref1, ref2, and the diffs
  path1 <- file.path(test_path, "diff", "1")
  path2 <- file.path(test_path, "diff", "2")
  pathd <- file.path(test_path, "diff", "diff")

  # visual_test dirs within the path1, path2, pathd
  path1_vtest <- file.path(path1, "visual_test")
  path2_vtest <- file.path(path2, "visual_test")
  pathd_vtest <- file.path(pathd, "visual_test")

  # Checkout the files for ref1
  checkout_worktree(ref1, outdir = path1, paths = "visual_test", pkgpath = pkg$path)

  # Find what changed between ref1 and ref2
  # TODO: add filtering here?
  changed <- vdiffstat(ref1, ref2, pkg, showhelp = FALSE)

  # The Modified and Added files
  ref2_changed <- subset(changed, (status =="M" | status=="A"), select = filename, drop = TRUE)

  # Check out from ref1 only the Modified and Deleted files
  checkout_worktree(ref2, outdir = path2, paths = ref2_changed, pkgpath = pkg$path)


  # Copy the CSS file over to the diff/visual_test dir
  dir.create(pathd_vtest, recursive = TRUE, showWarnings = FALSE)
  css_outfile <- file.path(pathd_vtest, basename(cssfile))
  file.copy(cssfile, css_outfile, overwrite = TRUE)

  # Find the subdirs that have testinfo.dat, and generate diff webpages for them
  testdirs <- dirname(list.files(path1_vtest,
                                 pattern = "testinfo.dat",
                                 recursive = TRUE))

  testdirs <- testdirs[grepl(filter, testdirs)]

  # Make diff pages for each of these directories
  for (t in testdirs) {
    # Just the changed files in this directory
    cfiles <- subset(changed, grepl(paste("visual_test/", t, "/", sep = ""), filename))
    # Strip off the leading path part of the filename
    cfiles$filename <- sub(paste("visual_test/", t, "/", sep = ""), "",
                           cfiles$filename, fixed = TRUE)
    make_vdiff_webpage(cfiles, name = t,
      file.path(path1_vtest, t), file.path(path2_vtest, t), file.path(pathd_vtest, t),
      cssfile = css_outfile, convertpng = convertpng, method = method,
      refname1 = ref1, refname2 = ifelse(ref2 == "", "working tree", ref2))
  }

  invisible()
}


# Make a web page with diffs between one path and another path
# This shouldn't be called by the user - users should call vdiff_webpage()
make_vdiff_webpage <- function(changed, name = "", path1, path2, pathd, cssfile,
    convertpng = FALSE, method = "ghostscript", refname1 = "", refname2 = "") {

  dir.create(pathd, recursive = TRUE, showWarnings = FALSE) # Create diff dir if needed

  # Get the information about the tests
  testinfo1 <- dget(file.path(path1, "testinfo.dat"))
  if (file.exists(file.path(path2, "testinfo.dat")))
    testinfo2 <- dget(file.path(path2, "testinfo.dat"))
  else
    testinfo2 <- testinfo1   # If testinfo2 doesn't exist, then it's unchanged from testinfo1

  # We want to merge 'changed' together with testinfo1 and testinfo2
  # This is a little tricky
  testinfo1 <- merge(changed, testinfo1, by = "filename", all.y = TRUE)
  testinfo2 <- merge(changed, testinfo2, by = "filename", all.y = TRUE)

  # order numbers can change if a test is inserted. So we have to do some acrobatics.
  mergeby <- intersect(names(testinfo1), names(testinfo2))
  mergeby <- mergeby[mergeby != "order"]
  testinfo <- merge(testinfo1, testinfo2, by = mergeby, all = TRUE)

  # In the special case where comparing a commit-ref against working dir (""),
  # added files won't have an "A" in changed$status (which is from git diff --name-status).
  # We can detect these cases, because they will be missing order.x, and manually
  # set the status to A.
  testinfo$status[is.na(testinfo$order.x) & !is.na(testinfo$order.y)] <- "A"

  testinfo <- arrange(testinfo, order.x, order.y)  # Order by ref1 and then ref2
  testinfo$order <- seq_len(nrow(testinfo))     # Assign new order (used for ordering items)

  testinfo$status[is.na(testinfo$status)] <- "U" # Set status to U for unchanged files

  # Figure out which files need to be converted to png.
  # All Modified files need to be converted (so they can be diffed)
  dfiles <- testinfo$filename[testinfo$status == "D"]  # Deleted files (in path1)
  afiles <- testinfo$filename[testinfo$status == "A"]  # Added files (in path2)
  mfiles <- testinfo$filename[testinfo$status == "M"]  # Modified files (in path1 and path2), also create a diff
  ufiles <- testinfo$filename[testinfo$status == "U"]  # Unchanged files (in path1)

  convertfiles <- c(file.path(path1, mfiles), file.path(path2, mfiles))

  # Add in the other files to convert, if convertpng==TRUE
  if (convertpng) {
    convertfiles <- c(convertfiles, file.path(path1, c(dfiles, ufiles)),
                                    file.path(path2, afiles))
  }


  outfile <- file.path(normalizePath(pathd), "index.html")
  message("Writing ", outfile)

  # Write HTML code to show a single test
  item_html <- function(t, path1, path2, pathd, convertpng) {

    # The diff file is a png. If convertpng==TRUE, so are the 1/ and 2/ files
    pngfile <- sub("\\.pdf$", ".png", t$filename)
    if (convertpng) reffile <- pngfile       # The filename in dirs 1/ and 2/
    else            reffile <- t$filename

    if (t$status == "D") {           # Deleted file
      status <- "changed"
      cell1 <- paste("<img src='", file.path(relativePath(path1, pathd), reffile), "'>", sep="")
      cell2 <- "Not present"
      celld <- "NA"
    } else if (t$status == "A") {    # Added file
      status <- "changed"
      cell1 <- "Not present"
      cell2 <- paste("<img src='", file.path(relativePath(path2, pathd), reffile), "'>", sep="")
      celld <- "NA"    
    } else if (t$status == "M") {    # Modified file
      status <- "changed"
      cell1 <- paste("<img src='", file.path(relativePath(path1, pathd), reffile), "'>", sep="")
      cell2 <- paste("<img src='", file.path(relativePath(path2, pathd), reffile), "'>", sep="")
      celld <- paste("<img src='", pngfile, "'>", sep="")
    } else if (t$status == "U") {    # Unchanged file
      status <- "unchanged"
      cell1 <- paste("<img src='", file.path(relativePath(path1, pathd), reffile), "'>", sep="")
      cell2 <- cell1
      celld <- "Identical"
    }

    paste('<div class="float"><div class="', status, '">\n',
          '  <div class="header">',
          '    <p class="description">', t$desc, '</p>\n',
          '    <p class="hash">', t$hash, '</p>', '</div>\n',
          '  <div class="imageset">\n',
          '    <span class="imagewrap">\n',
          '      <div><span class="refspec">', refname1,'</span></div>\n',
          '      <div class="image">', cell1, '</div>\n',
          '    </span>\n',
          '    <span class="imagewrap">\n',
          '      <div><span class="refspec">', refname2,'</span></div>\n',
          '      <div class="image">', cell2, '</div>\n',
          '    </span>\n',
          '    <span class="imagewrap">\n',
          '      <div>Difference</div>\n',
          '      <div class="image">', celld, '</div>\n',
          '    </span>\n',
          '  </div>\n',
          '</div></div>\n', sep="")
  }

  write(paste('<html><head>\n',
        '<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />',
        '<title>Visual tests diffs: ', name,
        '</title></head><body>\n',
        '<h1>Visual tests diffs: ', name, '</h1>\n',
        '<h2>Comparing <span class="refspec">', refname1,
        '</span> to <span class="refspec">', refname2,
        '</span></h2>\n',
        '<p class="changestatus">', nrow(testinfo), ' tests</p>\n',
        '<p class="changestatus">', sum(testinfo$status == "M"), ' changed</p>\n',
        '<p class="changestatus">', sum(testinfo$status == "A"), ' added</p>\n',
        '<p class="changestatus">', sum(testinfo$status == "D"), ' deleted</p>\n',
        sep = ""), outfile)

  # Write information about all the test items in testinfo
  for (i in seq_len(nrow(testinfo))) {
    write(item_html(testinfo[i, ], path1, path2, pathd, convertpng), outfile, append = TRUE)
  }

  write("</table></body></html>", outfile, append = TRUE)

  convert_pdf2png(convertfiles, method = method)

  mfilespng <- sub("\\.pdf$", ".png", mfiles)  # For the compared files, use png
  compare_png(file.path(path1, mfilespng),
              file.path(path2, mfilespng),
              file.path(pathd, mfilespng))

}



# =============================================================
# Utility functions
# =============================================================

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


# Generate the PNG images for a directory
convert_pdf2png <- function(filenames, indir = NULL, outdir = NULL, method = "ghostscript") {
  if (length(filenames) == 0) return()
  infiles <- filenames[grepl("\\.pdf$", filenames)]  # Keep the .pdf files only
  outfiles <- sub("\\.pdf$", ".png", infiles)

  # Prepend paths if specified; otherwise assume that 'filenames' has a full path
  if (!is.null(indir))   infiles  <- file.path(indir, infiles)
  if (!is.null(outdir))  outfiles <- file.path(outdir, outfiles)

  message("Converting ", length(infiles), " PDF files to PNG, using method ", method)

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


# A function for checking out a path (like "visual_test) from a commit ref,
#  or use "" for current state
checkout_worktree <- function(ref = "", outdir = NULL, paths = "", pkgpath = NULL) {
  if (is.null(outdir))  outdir <- file.path(tempdir(), "checkout-workdir")

  # TODO: change this - it's dangerous if someone uses "/"!
  unlink(outdir, recursive = TRUE)      # Delete existing directory
  dir.create(outdir, recursive = TRUE)  # Create the new directory

  if (ref == "") {
    # If blank ref, simply copy the files over from the working tree
    # First get the (non-dir) files only, then recurse into directories
    fullpaths <- file.path(pkgpath, paths)
    dirs <- file.info(fullpaths)$isdir
    fullpaths <- fullpaths[!dirs]  # Drop the dirs from the vector of full paths
    fullpaths <- c(fullpaths, list.files(fullpaths[dirs], recursive = TRUE,
                                         full.names = TRUE))

    # Get the paths, relative to pkgpath
    relpaths <- sapply(fullpaths, relativePath, pkgpath, USE.NAMES = FALSE)

    # Find which directories need to be created, and then create them
    newdirs <- unique(file.path(outdir, dirname(relpaths)))
    sapply(newdirs, dir.create, recursive = TRUE, showWarnings = FALSE)
    # Copy the files over
    file.copy(fullpaths, file.path(outdir, relpaths))

  } else {
    # Checkout the git ref into outdir
    if (systemCall("git", c("--work-tree", outdir, "checkout", ref, "--", paths),
                   rundir = pkgpath)$status != 0)
      stop("git checkout failed.")
    # Need to reset git index status after the checkout (so git doesn't get confused)
    systemCall("git", c("reset", "--mixed"), rundir = pkgpath)
  }
}


# Find path to d, relative to start. If `start` is NULL, use current dir
# if d is ./foo/bar and start is ./foo, then return "bar"
# if d is ./zz and start is ./foo/bar, then return "../../zz"
relativePath <- function(path, start = NULL) {
  if (is.null(start)) start <- getwd()

  # If either of these fail (with a warning), it'll give an incorrect relative
  # path, so throw an error.
  tryCatch({
    p <- strsplit(normalizePath(path,  winslash = "/"), "/")[[1]]
    s <- strsplit(normalizePath(start, winslash = "/"), "/")[[1]]
  }, warning = function(w) stop(w) )

  len <- min(length(s), length(p))
  # Find if any of these pieces are different. If so, that's the first mismatch;
  #   if not, then the next piece is the first mismatch.
  mismatches <- s[1:len] != p[1:len]
  if (any(mismatches))  lastmatch <- min(which(mismatches)) - 1
  else                  lastmatch <- len

  p <- p[-(1:lastmatch)]                            # remove everything that matches

  # Build the relative path, adding ..'s for each path level in s
  paste(c(rep("..", length(s)-lastmatch), p), collapse="/")
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
  unlink(tempfile_fd)

  # Return to the starting directory
  if (!is.null(rundir))  setwd(startdir)

  return(list(status = status, output = output))
}

# Find the current git commit hash of a directory (must be top level of repo)
git_find_commit_hash <- function(dir = ".") {
  ret <- systemCall("git", c("--git-dir", file.path(dir, ".git"), "rev-parse", "HEAD"))
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
