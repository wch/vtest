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

  make_vdiff_indexpage(vdiff, ref1text, ref2text, diffdir)

  for (context in unique(vdiff$context)) {
    make_vdiff_contextpage(vdiff, context, ref1text, ref2text, diffdir, imagedir, convertpng, method = method)
  }

  invisible()
}


make_vdiff_indexpage <- function(vdiff, ref1text = "", ref2text = "", diffdir = NULL) {

  # Get context
  contexts <- unique(vdiff$context)

  htmlfile <- file.path(normalizePath(diffdir), "index.html")
  message("Writing ", htmlfile)

template <- '
<html>
<head>
<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />
<title>Visual test diffs</title>
</head>
<body>
<h1>Visual test diffs</h1>
<h2>Comparing <span class="refspec">{{ref1text}}</span> to <span class="refspec">{{ref2text}}</span></h2>

<table>
  <thead><tr>
    <th>Context</th>
    <th>Changed</th>
    <th>Added</th>
    <th>Deleted</th>
    <th>Total tests</th>
  </tr></thead>
  <tbody>
{{#vds}}
{{#value}}
    <tr>
      <td class="context"><a href="{{context}}.html"}>{{context}}</a></td>
      <td class="num">{{C}}</td>
      <td class="num">{{A}}</td>
      <td class="num">{{D}}</td>
      <td class="num">{{Total}}</td>
    </tr>
{{/value}}
{{/vds}}
  </tbody>
  <tfoot>
{{#vdtotal}}
    <tr>
      <td class="total">Total</td>
      <td class="num">{{C}}</td>
      <td class="num">{{A}}</td>
      <td class="num">{{D}}</td>
      <td class="num">{{Total}}</td>
    </tr>
{{/vdtotal}}
  </tfoot>
</table>
</body>
</html>
'

  # Get a summary count for each category
  vds <- ddply(vdiff, .(context, status), summarise, n = length(status), .drop=FALSE)
  vds <- dcast(vds, context ~ status, value.var = "n")
  vds$Total <- vds$C + vds$A + vds$D + vds$U  # Total for each context
  vds <- split(vds, 1:nrow(vds))
  vds <- iteratelist(vds)

  # Total across all contexts
  vdtotal <- ddply(vdiff, .(status), summarise, n = length(status), .drop=FALSE)
  vdtotal <- dcast(vdtotal, 1 ~ status, value.var = "n")
  vdtotal$Total <- vdtotal$C + vdtotal$A + vdtotal$D + vdtotal$U

  write(whisker.render(template), htmlfile, append = TRUE)
}


# Make a web page with diffs between one path and another path
# This shouldn't be called by the user - users should call vdiff_webpage()
make_vdiff_contextpage <- function(vdiff, context = NULL, ref1text = "", ref2text = "",
    diffdir = NULL, imagedir = NULL, convertpng = TRUE, method = "ghostscript") {

  if(is.null(context))  stop("Need to specify context")
  if(is.null(diffdir))  stop("Need to specify diffdir")
  if(is.null(imagedir)) stop("Need to specify imagedir")

  vdiff <- vdiff[vdiff$context == context, ]

  htmlfile <- file.path(normalizePath(diffdir), paste(context, ".html", sep = ""))
  message("Writing ", htmlfile)

  # Write HTML code to show a single test
  item_html <- function(t, ref1text, ref2text, convertpng) {

    fd <- paste(t$hash1, t$hash2, sep="-")  # Name of diff file

    if (convertpng) {
      f1 <- paste(t$hash1, ".png", sep = "")
      f2 <- paste(t$hash2, ".png", sep = "")
    } else {
      f1 <- paste(t$hash1, ".pdf", sep = "")
      f2 <- paste(t$hash2, ".pdf", sep = "")
    }
    fd <- paste(fd,      ".png", sep = "")   # Diff images are always png

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
    convertfiles <- unique(c(changed$hash1, changed$hash2))

    # Copy over the other files (to display as PDF)
    allhashes <- unique(c(vdiff$hash1, vdiff$hash2))
    file.copy(
      file.path(imagedir, allhashes),
      file.path(diffdir, paste(allhashes, ".pdf", sep="")))
  }
  convertfiles <- convertfiles[!is.na(convertfiles)] # Drop NAs

  convert_png(convertfiles, imagedir, diffdir, method = method)

  if(nrow(changed) > 0) {
    compare_png(
      file.path(diffdir, paste(changed$hash1, ".png", sep="")),
      file.path(diffdir, paste(changed$hash2, ".png", sep="")),
      file.path(diffdir, paste(changed$hash1, "-", changed$hash2, ".png", sep="")))
  }

}
