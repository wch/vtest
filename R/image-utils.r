#' Normalize creation information in PDF files
#'
#' For a PDF, modify the CreationDate and ModDate (lines 5 and 6)
#' so that the files are exactly the same, regardless of date + time they
#' were actually created. It also changes the Producer field (line 8) to "R 0.00.0",
#' instead of the usual version, e.g., "R 2.15.0".
#'
#' @param infile input file name
#' @param outfile output file name (must be different from \code{infile})
zero_pdf_info <- function(infile = NULL, outfile = NULL) {
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
  pdftext[8] <- "/Producer (R 0.00.0)"

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

  filenames <- unique(filenames)
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
