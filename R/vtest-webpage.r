# =============================================================
# Functions for generating web pages to view tests
# =============================================================

# This is the function that the user calls
# * outdir: the output directory
# * convertpng: if TRUE, convert the source PDFs files to PNG instead.
#' @export
vtest_webpage <- function(ref = "", pkg = NULL, resultdir = NULL, filter = "",
    convertpng = TRUE) {
  pkg <- as.package(pkg)

  if (is.null(resultdir))
    resultdir <- find_default_resultdir()

  htmldir  <- file.path(resultdir, "html")
  imagedir <- file.path(resultdir, "images")

  if (!file.exists(htmldir))
    dir.create(htmldir, recursive = TRUE)
  else
    unlink(dir(htmldir, full.names = TRUE))

  if (ref == "") {
    reftext <- "last local test"
    testinfo <- read.csv(file.path(resultdir, "lasttest.csv"), stringsAsFactors = FALSE)
  } else {
    reftext <- ref
    refh <- git_find_commit_hash(pkg$path, ref)
    testinfo <- get_testinfo(commit = refh, resultdir = resultdir)
  }

  # Filter results
  testinfo <- testinfo[match_filter_idx(testinfo$context, filter), ]

  make_vtest_indexpage(testinfo, htmldir, reftext)

  ddply(testinfo, .(context), .fun = function(ti) {
      make_vtest_contextpage(ti, htmldir, imagedir, reftext, convertpng)
  })

  invisible()
}


# Makes the overall index web page
make_vtest_indexpage <- function(testinfo, htmldir = NULL, reftext = "") {

  template <- '
<html><head>
<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />
<title>Visual tests</title></head>
<body>
<h1>Visual tests</h1>

<table>
  <thead><tr>
    <th>Context</th>
    <th>Tests</th>
  </tr></thead>
{{#vts}}
{{#value}}
    <tr>
      <td class="context"><a href="{{context}}.html">{{context}}</a></td>
      <td class="num">{{n}}</td>
    </tr>
{{/value}}
{{/vts}}
  </tbody>
  <tfoot>
    <tr>
      <td class="total">Total</td>
      <td class="num">{{total}}</td>
    </tr>
  </tfoot>
</table>
</body>
</html>
'

  # Get contexts and counts
  vts <- ddply(testinfo, .(context), summarise, n = length(context))
  vts <- split(vts, 1:nrow(vts))
  vts <- iteratelist(vts)
  total <- nrow(testinfo)  # Total number of tests

  whisker.render(template)

  htmlfile <- file.path(normalizePath(htmldir), "index.html")
  message("Writing ", htmlfile)
  write(whisker.render(template), htmlfile)
}


# Makes the web page for a single context
make_vtest_contextpage <- function(testinfo, htmldir = NULL, imagedir = NULL,
    reftext = "", convertpng = TRUE)  {
  if (is.null(htmldir))  stop("Need to specify htmldir")
  if (is.null(imagedir)) stop("Need to specify imagedir")

  # Sort by order
  testinfo <- testinfo[order(testinfo$order), ]

  # Get context
  context <- unique(testinfo$context)
  if (length(context) != 1)
    stop("There is not exactly one context in this subset: ", context)

  htmlfile <- file.path(normalizePath(htmldir), paste(context, "html", sep="."))
  message("Writing ", htmlfile)

  template <- '
<html><head>
<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />
<title>Visual tests: {{context}}</title></head>
<body>
<h1>Visual tests: {{context}}</h1>
<h2>Results for <span class="refspec">{{reftext}}</span></h2>

{{#vtitems}}
{{#value}}
<div class="float">
  <div class="header">
    <p class="description">{{desc}}</p>
  </div>
  <div class="imageset">
    <span class="imagewrap">
      <div class="image"><img src="{{file}}"></div>
      <div class="hash">{{hash}}</div>
    </span>
  </div>
</div>
{{/value}}
{{/vtitems}}

</body>
</html>
'

  # Prepare info for a single test
  item_prep <- function(t) {
    if (convertpng) file <- paste(t$hash, "png", sep=".")
    else            file <- paste(t$hash, t$type , sep=".")

    data.frame(desc = t$desc, file, hash = t$hash)
  }

  vtitems <- lapply(split(testinfo, 1:nrow(testinfo)), item_prep)
  vtitems <- iteratelist(vtitems)

  write(whisker.render(template), htmlfile)


  if (convertpng) {
    convert_png(testinfo$hash, imagedir, htmldir)
  } else {
    file.copy(file.path(imagedir, testinfo$hash),
      file.path(htmldir, paste(testinfo$hash, ".pdf", sep="")))
  }

  return(htmlfile)
}
