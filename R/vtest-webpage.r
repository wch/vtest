# =============================================================
# Functions for generating web pages to view test results
# =============================================================

#' Generate web pages for viewing vtest results.
#'
#' @param ref a git commit ref that result pages will be generated for. The
#'   empty string \code{""} refers to the last-run tests.
#' @param pkg package object or path.
#' @param filter a regular expression; result pages will be generated only
#'   only for test contexts that match this pattern.
#' @param convertpng if TRUE, convert the source PDFs files to PNG. Otherwise
#'   they are kept in PDF format, and are viewable only in some browers.
#' @param prompt ask to open web page in browser.
#'
#' @seealso \code{\link{vdiff_webpage}} for creating a web page for comparing
#'   results of tests in two different commits.
#' @export
vtest_webpage <- function(ref = "", pkg = ".", filter = "", convertpng = TRUE,
      prompt = TRUE) {
  init_vtest(pkg)

  if (!file.exists(get_vtest_htmldir()))
    dir.create(get_vtest_htmldir(), recursive = TRUE)
  else
    unlink(dir(get_vtest_htmldir(), full.names = TRUE))

  copy_css(get_vtest_htmldir())

  if (ref == "") {
    imagedir <- get_vtest_lasttest_dir()
    reftext <- "last local test"
    commit <- "NA"
    resultset <- load_lastresultset()
  } else {
    imagedir <- get_vtest_imagedir()

    reftext <- ref
    commit <- git_find_commit_hash(get_vtest_pkg()$path, ref)
    resultset <- load_resultsets(commit = commit)
    if (nrow(resultset) == 0)
      stop("No resultset found for ref ", ref, ", commit ", commit)
  }

  # Filter results
  resultset <- resultset[match_filter_idx(resultset$context, filter), ]

  indexpage <- make_vtest_indexpage(resultset, get_vtest_htmldir(), reftext, commit)

  ddply(resultset, .(context), .fun = function(ti) {
      make_vtest_contextpage(ti, get_vtest_htmldir(), imagedir, reftext, commit, convertpng)
  })

  if (prompt && confirm("Open webpage in browser? (y/n) "))
    browseURL(indexpage)

  invisible()
}


# Makes the overall index web page
make_vtest_indexpage <- function(resultset, htmldir = NULL, reftext = "", commit = "") {
  if (is.null(htmldir))  stop("Need to specify htmldir")

  # Get contexts and counts
  vts <- ddply(resultset, .(context), summarise, n = length(context),
           nwarn = sum(err=="warn"), nerror = sum(err=="error"))
  # css classes for warning and error cells
  vts$warn_class  <- ifelse(vts$nwarn > 0,  "warn", "num")
  vts$error_class <- ifelse(vts$nerror > 0, "error", "num")

  vts <- split(vts, 1:nrow(vts))
  vts <- iteratelist(vts)

  # List with data for the template
  data <- list(vts = vts, reftext = reftext, commit = commit)

  # Stuff for the Total row
  data$total      <- nrow(resultset)              # Total number of tests
  data$totalwarn  <- sum(resultset$err == "warn") # Total number of warnings
  data$totalerror <- sum(resultset$err == "error")  # Total number of errors
  data$warn_class  <- ifelse(data$totalwarn > 0, "warn", "num")
  data$error_class <- ifelse(data$totalwarn > 0, "error", "num")

  htmlfile <- file.path(normalizePath(htmldir), "index.html")
  message("Writing ", htmlfile)
  render_template('vtest-index', data, htmlfile)

  return(htmlfile)
}


# Makes the web page for a single context
make_vtest_contextpage <- function(resultset, htmldir = NULL, imagedir = NULL,
    reftext = "", commit = "", convertpng = TRUE)  {
  if (is.null(htmldir))  stop("Need to specify htmldir")
  if (is.null(imagedir)) stop("Need to specify imagedir")

  # Sort by order
  resultset <- resultset[order(resultset$order), ]

  # Get context
  context <- unique(resultset$context)
  if (length(context) != 1)
    stop("There is not exactly one context in this subset: ", context)

  # Prepare info for a single test
  item_prep <- function(t) {
    if (convertpng) file <- paste(t$hash, "png", sep=".")
    else            file <- paste(t$hash, t$type , sep=".")

    data.frame(desc = t$desc, file, hash = t$hash, err = t$err)
  }

  vtitems <- lapply(split(resultset, 1:nrow(resultset)), item_prep)
  vtitems <- iteratelist(vtitems)

  # List with data for the template
  data <- list(vtitems = vtitems, reftext = reftext, commit = commit,
    context = context)

  htmlfile <- file.path(normalizePath(htmldir), str_c(context, ".html"))
  message("Writing ", htmlfile)
  render_template('vtest-context', data, htmlfile)


  if (convertpng) {
    convert_png_cached(resultset$hash, imagedir, htmldir)
  } else {
    file.copy(file.path(imagedir, resultset$hash),
      file.path(htmldir, str_c(resultset$hash, ".pdf")))
  }

  return(htmlfile)
}
