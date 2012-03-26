# Find files modified between ref1 and ref2
# If ref2 is "", we treat it to mean the last test results.
# * all: if FALSE, don't return Unchanged; if true TRUE, return Unchanged
#     (in addition to everything else)
#' @export
vdiffstat <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "", all = FALSE) {
  set_vtest_pkg(pkg)

  # Get the resultset data for ref1 and ref2
  if (ref1 == "") {
    ref1text <- "last local test"
    ref1h <- ""
    ti1 <- load_lastresultset()
  } else {
    ref1text <- ref1
    ref1h <- git_find_commit_hash(get_vtest_pkg()$path, ref1)
    ti1 <- load_resultset(commit = ref1h)
  }

  if (ref2 == "") {
    ref2text <- "last local test"
    ref2h <- ""
    ti2 <- load_lastresultset()
  } else {
    ref2text <- ref2
    ref2h <- git_find_commit_hash(get_vtest_pkg()$path, ref2)
    ti2 <- load_resultset(commit = ref2h)
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

  td$status <- factor(td$status, levels = c("U", "A", "D", "C")) 

  # Sort by order in ref1, then ref2
  td <- arrange(td, order1, order2)

  # Change order of columns to be prettier
  td <- td[c("context", "desc", "status", "hash1", "hash2", "order1", "order2")]

  # Pull out only the rows where context matches the filter
  td <- td[match_filter_idx(td$context, filter), ]

  if (!all)
    td <- td[td$status != "U", ]

  return(arrange(td, context, order1, order2))
}
