# =============================================================
# Functions for generating visual diff webpages
# =============================================================


# Make visual diff from two refs
#' @export
vdiff_webpage <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "",
      convertpng = TRUE, method = "ghostscript", prompt = TRUE) {
  # TODO: message about weird color space in conversion using convert
  # TODO: print message about png option, and slow png vs safari-only pdf
  set_vtest_pkg(pkg)

  if (!file.exists(get_vtest_diffdir()))
    dir.create(get_vtest_diffdir(), recursive = TRUE)
  else
    unlink(dir(get_vtest_diffdir(), full.names = TRUE))

  copy_css(get_vtest_diffdir())

  # Get the changes
  vdiff <- vdiffstat(ref1, ref2, get_vtest_pkg(), filter, all = TRUE)


  if (ref1 == "") {
    ref1text <- "last local test"
    commit1 <- "NA"
  } else {
    ref1text <- ref1
    commit1 <- git_find_commit_hash(get_vtest_pkg()$path, ref1)
  }
  if (ref2 == "") {
    ref2text <- "last local test"
    commit2 <- "NA"
  } else {
    ref2text <- ref2
    commit2 <- git_find_commit_hash(get_vtest_pkg()$path, ref2)
  }

  make_vdiff_indexpage(vdiff, ref1text, ref2text, commit1, commit2, get_vtest_diffdir())

  for (context in unique(vdiff$context)) {
    make_vdiff_contextpage(vdiff, context, ref1text, ref2text, commit1, commit2,
                           get_vtest_diffdir(), get_vtest_imagedir(), convertpng, method = method)
  }

  invisible()
}


make_vdiff_indexpage <- function(vdiff, ref1text = "", ref2text = "",
    commit1 = "", commit2 = "", diffdir = NULL) {

  # Get context
  contexts <- unique(vdiff$context)

  # Get a summary count for each category
  vds <- ddply(vdiff, .(context, status), summarise, n = length(status), .drop = FALSE)
  vds <- dcast(vds, context ~ status, value.var = "n")
  vds$Total <- vds$C + vds$A + vds$D + vds$U  # Total for each context
  # css classes for warning and error cells
  vds$C_class <- ifelse(vds$C == 0, "num", "changed")
  vds$A_class <- ifelse(vds$A == 0, "num", "added")
  vds$D_class <- ifelse(vds$D == 0, "num", "deleted")

  vds <- split(vds, 1:nrow(vds))
  vds <- iteratelist(vds)

  # List with data for the template
  data <- list(vds = vds, ref1text = ref1text, ref2text = ref2text,
    commit1 = commit1, commit2 = commit2)

  # Total across all contexts
  data$Total <- nrow(vdiff)
  data$C     <- sum(vdiff$status == "C")
  data$A     <- sum(vdiff$status == "A")
  data$D     <- sum(vdiff$status == "D")
  # css classes for warning and error cells
  data$C_class <- ifelse(data$C > 0, "changed", "num")
  data$A_class <- ifelse(data$A > 0, "added", "num")
  data$D_class <- ifelse(data$D > 0, "deleted", "num")

  htmlfile <- file.path(normalizePath(diffdir), "index.html")
  message("Writing ", htmlfile)
  render_template('vdiff-index', data, htmlfile)
}


# Make a web page with diffs between one path and another path
# This shouldn't be called by the user - users should call vdiff_webpage()
#' @importFrom whisker iteratelist
make_vdiff_contextpage <- function(vdiff, context = NULL, ref1text = "", ref2text = "",
    commit1 = "", commit2 = "", diffdir = NULL, imagedir = NULL, convertpng = TRUE,
    method = "ghostscript") {

  if(is.null(context))  stop("Need to specify context")
  if(is.null(diffdir))  stop("Need to specify diffdir")
  if(is.null(imagedir)) stop("Need to specify imagedir")

  vdiff <- vdiff[vdiff$context == context, ]

  item_prep <- function(t, ref1text, ref2text, convertpng) {

    img_link <- function(name) {
      if (convertpng)  f <- paste(name, ".png", sep = "")
      else             f <- paste(name, ".pdf", sep = "")
      paste("<img src=\"", f, "\">", sep = "")
    }

    if (t$status == "D") {           # Deleted file
      status <- "deleted"
      cell1  <- img_link(t$hash1)
      cell2  <- "Not present"
      celld  <- "NA"
    } else if (t$status == "A") {    # Added file
      status <- "added"
      cell1  <- "Not present"
      cell2  <- img_link(t$hash2)
      celld  <- "NA"
    } else if (t$status == "C") {    # Changed file
      status <- "changed"
      cell1  <- img_link(t$hash1)
      cell2  <- img_link(t$hash2)
      # Diff file is always png
      celld  <- paste("<img src=\"", t$hash1, "-", t$hash2, ".png", "\">", sep="") 
    } else if (t$status == "U") {    # Unchanged file
      status <- "unchanged"
      cell1  <- img_link(t$hash1)
      cell2  <- img_link(t$hash2)
      celld  <- "Identical"
    }

    data.frame(ref1text, ref2text, desc = t$desc, status,
      hash1 = t$hash1, hash2 = t$hash2, cell1, cell2, celld)
  }


  vstat <- list(C = sum(vdiff$status == "C"),
                A = sum(vdiff$status == "A"),
                D = sum(vdiff$status == "D"),
                U = sum(vdiff$status == "U"),
                Total = nrow(vdiff))

  # css classes for warning and error cells
  vstat$C_class <- ifelse(vstat$C == 0, "num", "changed")
  vstat$A_class <- ifelse(vstat$A == 0, "num", "added")
  vstat$D_class <- ifelse(vstat$D == 0, "num", "deleted")


  vditems <- lapply(split(vdiff, 1:nrow(vdiff)), item_prep, ref1text, ref2text, convertpng)
  vditems <- iteratelist(vditems)

  # List with data for the template
  data <- list(vstat = vstat, vditems = vditems, context = context,
    ref1text = ref1text, ref2text = ref2text,
    commit1 = commit1, commit2 = commit2)

  htmlfile <- file.path(normalizePath(diffdir), paste(context, ".html", sep = ""))
  message("Writing ", htmlfile)
  render_template('vdiff-context', data, htmlfile)

  # ========= PNG convert and compare ==========

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

  convert_png_cached(convertfiles, imagedir, diffdir, method = method)

  if(nrow(changed) > 0) {
    compare_png(
      file.path(diffdir, paste(changed$hash1, ".png", sep="")),
      file.path(diffdir, paste(changed$hash2, ".png", sep="")),
      file.path(diffdir, paste(changed$hash1, "-", changed$hash2, ".png", sep="")))
  }

}
