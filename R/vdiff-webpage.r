# =============================================================
# Functions for generating visual diff webpages
# =============================================================


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

