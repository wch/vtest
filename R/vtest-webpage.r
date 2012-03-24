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
