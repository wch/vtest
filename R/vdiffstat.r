# Find files modified between ref1 and ref2
# If ref2 is "", we treat it to mean the last test results.
# * showall: if FALSE, don't return Unchanged; if true TRUE, return Unchanged
#     (in addition to everything else)
#' @export
vdiffstat <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "",
                      resultdir = NULL, all = FALSE) {
  pkg <- as.package(pkg)

  if (is.null(resultdir))
    resultdir <- find_default_resultdir()

  # Get the testinfo data for ref1 and ref2
  if (ref1 == "") {
    ref1text <- "last local test"
    ref1h <- ""
    ti1 <- get_lasttestinfo(resultdir = resultdir)
  } else {
    ref1text <- ref1
    ref1h <- git_find_commit_hash(pkg$path, ref1)
    ti1 <- get_testinfo(commit = ref1h, resultdir = resultdir)
  }

  if (ref2 == "") {
    ref2text <- "last local test"
    ref2h <- ""
    ti2 <- get_lasttestinfo(resultdir = resultdir)
  } else {
    ref2text <- ref2
    ref2h <- git_find_commit_hash(pkg$path, ref2)
    ti2 <- get_testinfo(commit = ref2h, resultdir = resultdir)
  }

  # Keep just a few columns
  ti1 <- ti1[c("context", "desc", "order", "hash")]
  ti2 <- ti2[c("context", "desc", "order", "hash")]

  # Merge together and check for changes
  td <- merge(ti1, ti2, by=c("context", "desc"), suffixes = c("1", "2"), all = TRUE)
  td$status <- "U"                                      # Default Unchanged
  td$status[ is.na(td$hash1) & !is.na(td$hash2)] <- "A" # Added
  td$status[!is.na(td$hash1) &  is.na(td$hash2)] <- "D" # Deleted
  td$status[td$hash1 != td$hash2] <- "C"                # Changed

  # Sort by order in ref1, then ref2
  td <- arrange(td, order1, order2)

  # Change order of columns to be prettier
  td <- td[c("context", "desc", "status", "hash1", "hash2", "order1", "order2")]

  # Pull out only the rows where context matches the filter
  td <- td[match_filter_idx(td$context, filter), ]

  if (!all)
    td <- td[td$status != "U", ]

  return(td)
}
