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
    commit <- "NA"
    resultset <- get_lastresultset(resultdir)
  } else {
    reftext <- ref
    commit <- git_find_commit_hash(pkg$path, ref)
    resultset <- get_resultset(commit, resultdir)
  }

  # Filter results
  resultset <- resultset[match_filter_idx(resultset$context, filter), ]

  make_vtest_indexpage(resultset, htmldir, reftext, commit)

  ddply(resultset, .(context), .fun = function(ti) {
      make_vtest_contextpage(ti, htmldir, imagedir, reftext, commit, convertpng)
  })

  invisible()
}


# Makes the overall index web page
make_vtest_indexpage <- function(resultset, htmldir = NULL, reftext = "", commit = "") {

  template <- '
<html><head>
<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />
<title>Visual tests</title></head>
<body>
<h1>Visual tests</h1>
<h2>Results for <span class="refspec">{{reftext}}</span></h2>
<p>Commit: <span class="smallrefspec">{{commit}}</span></p>

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
  vts <- ddply(resultset, .(context), summarise, n = length(context))
  vts <- split(vts, 1:nrow(vts))
  vts <- iteratelist(vts)
  total <- nrow(resultset)  # Total number of tests

  whisker.render(template)

  htmlfile <- file.path(normalizePath(htmldir), "index.html")
  message("Writing ", htmlfile)
  write(whisker.render(template), htmlfile)
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

  htmlfile <- file.path(normalizePath(htmldir), paste(context, "html", sep="."))
  message("Writing ", htmlfile)

  template <- '
<html><head>
<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />
<title>Visual tests: {{context}}</title></head>
<body>
<h1>Visual tests: {{context}}</h1>
<h2>Results for <span class="refspec">{{reftext}}</span></h2>
<p>Commit: <span class="smallrefspec">{{commit}}</span></p>

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

  vtitems <- lapply(split(resultset, 1:nrow(resultset)), item_prep)
  vtitems <- iteratelist(vtitems)

  write(whisker.render(template), htmlfile)


  if (convertpng) {
    convert_png(resultset$hash, imagedir, htmldir)
  } else {
    file.copy(file.path(imagedir, resultset$hash),
      file.path(htmldir, paste(resultset$hash, ".pdf", sep="")))
  }

  return(htmlfile)
}
