# =============================================================
# Functions for generating visual diff webpages
# =============================================================


# Make visual diff from two refs
# TODO: Create overall index file, with status
#' @export
vdiff_webpage <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "",
      resultdir = NULL, convertpng = TRUE, method = "ghostscript", prompt = TRUE) {
  # TODO: message about weird color space in conversion using convert
  # TODO: print message about png option, and slow png vs safari-only pdf
  # TODO: display filename if it differs from hash

  pkg <- as.package(pkg)

  if (is.null(resultdir))
    resultdir <- find_default_resultdir()

  diffdir  <- file.path(resultdir, "diff")
  imagedir <- file.path(resultdir, "images")

  if (!file.exists(diffdir))
    dir.create(diffdir, recursive = TRUE)
  else
    unlink(dir(diffdir, full.names = TRUE))

  # TODO: Copy css file?

  # Get the changes
  vdiff <- vdiffstat(ref1, ref2, pkg, filter, resultdir, all = TRUE)


  if (ref1 == "")  ref1text <- "last local test"
  else             ref1text <- ref1
  if (ref2 == "")  ref2text <- "last local test"
  else             ref2text <- ref2

  for (context in unique(vdiff$context)) {
    make_vdiff_contextpage(vdiff, context, ref1text, ref2text, diffdir, imagedir, convertpng, method = method)
  }

  invisible()
}


# Make a web page with diffs between one path and another path
# This shouldn't be called by the user - users should call vdiff_webpage()
make_vdiff_contextpage <- function(vdiff, context = NULL, ref1text = "", ref2text = "",
    diffdir = NULL, imagedir = NULL, convertpng = TRUE, method = "ghostscript") {

  if(is.null(context))  stop("Need to specify context")
  if(is.null(diffdir))  stop("Need to specify diffdir")
  if(is.null(imagedir)) stop("Need to specify imagedir")

  vdiff <- vdiff[vdiff$context == context, ]

  htmlfile <- file.path(normalizePath(diffdir), "index.html")
  message("Writing ", htmlfile)

  # Write HTML code to show a single test
  item_html <- function(t, ref1text, ref2text, convertpng) {

    fd <- paste(t$hash1, t$hash2, sep="-")  # Name of diff file

    if (convertpng) {
      f1 <- paste(t$hash1, ".png", sep = "")
      f2 <- paste(t$hash2, ".png", sep = "")
      fd <- paste(fd,      ".png", sep = "")
    } else {
      f1 <- paste(t$hash1, ".pdf", sep = "")
      f2 <- paste(t$hash2, ".pdf", sep = "")
      fd <- paste(fd,      ".pdf", sep = "")
    }

    if (t$status == "D") {           # Deleted file
      status <- "changed"
      cell1 <- paste("<img src='", f1 , "'>", sep="")
      cell2 <- "Not present"
      celld <- "NA"
    } else if (t$status == "A") {    # Added file
      status <- "changed"
      cell1 <- "Not present"
      cell2 <- paste("<img src='", f2, "'>", sep="")
      celld <- "NA"    
    } else if (t$status == "C") {    # Changed file
      status <- "changed"
      cell1 <- paste("<img src='", f1, "'>", sep="")
      cell2 <- paste("<img src='", f2, "'>", sep="")
      celld <- paste("<img src='", fd, "'>", sep="")
    } else if (t$status == "U") {    # Unchanged file
      status <- "unchanged"
      cell1 <- paste("<img src='", f1, "'>", sep="")
      cell2 <- cell1
      celld <- "Identical"
    }

    paste('<div class="float"><div class="', status, '">\n',
          '  <div class="header">',
          '    <p class="description">', t$desc, '</p>\n',
          '  </div>\n',
          '  <div class="imageset">\n',
          '    <span class="imagewrap">\n',
          '      <div><span class="refspec">', ref1text,'</span></div>\n',
          '      <div class="image">', cell1, '</div>\n',
          '      <div class="hash">', t$hash1, '</div>\n',
          '    </span>\n',
          '    <span class="imagewrap">\n',
          '      <div><span class="refspec">', ref2text,'</span></div>\n',
          '      <div class="image">', cell2, '</div>\n',
          '      <div class="hash">', t$hash2, '</div>\n',
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
        '<title>Visual tests diffs: ', context,
        '</title></head><body>\n',
        '<h1>Visual tests diffs: ', context, '</h1>\n',
        '<h2>Comparing <span class="refspec">', ref1text,
        '</span> to <span class="refspec">', ref2text,
        '</span></h2>\n',
        '<p class="changestatus">', nrow(vdiff), ' tests</p>\n',
        '<p class="changestatus">', sum(vdiff$status == "C"), ' changed</p>\n',
        '<p class="changestatus">', sum(vdiff$status == "A"), ' added</p>\n',
        '<p class="changestatus">', sum(vdiff$status == "D"), ' deleted</p>\n',
        sep = ""), htmlfile)

  # Write information about all the test items in testinfo
  for (i in seq_len(nrow(vdiff))) {
    write(item_html(vdiff[i, ], ref1text, ref2text, convertpng), htmlfile, append = TRUE)
  }

  write("</table></body></html>", htmlfile, append = TRUE)


  # Get all the rows that changed
  changed <- vdiff[vdiff$status == "C", ]

  if (convertpng) {
    # Convert all the images
    convertfiles <- unique(c(vdiff$hash1, vdiff$hash2))

  } else {
    # Convert only those images that changed (and require diff images)
    convertfiles <- unique(c(changed$hash1, vdiff$hash2))

    # Copy over the other files (to display as PDF)
    allhashes <- unique(c(vdiff$hash1, vdiff$hash2))
    file.copy(
      file.path(imagedir, allhashes),
      file.path(diffdir, paste(allhashes, ".pdf", sep="")))
  }
  convertfiles <- convertfiles[!is.na(convertfiles)] # Drop NAs

  convert_png(convertfiles, imagedir, diffdir, method = method)

  compare_png(
    file.path(diffdir, paste(changed$hash1, ".png", sep="")),
    file.path(diffdir, paste(changed$hash2, ".png", sep="")),
    file.path(diffdir, paste(changed$hash1, "-", changed$hash2, ".png", sep="")))

}
