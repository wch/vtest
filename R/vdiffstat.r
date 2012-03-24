
# Find files modified between ref1 and ref2
# If ref2 is "" (the working tree), then we don't know exactly which of the new files
# the user plans to commit. So we just assume all new files in the working tree are
# added files (marked with A).
#' @export
vdiffstat <- function(ref1 = "HEAD", ref2 = "", pkg = NULL, filter = "", showhelp = TRUE) {
  pkg <- as.package(pkg)

  if (ref1 == "")  stop('ref1 must not be blank "" (because git doesn\'t like it)')

  ref2text <- ifelse(ref2 == "", "working tree", ref2)
  message("Comparing ", ref1, " to ", ref2text);
  if (ref2 == "")
    message("The status of Added files is a guess when using working tree.\n",
      "  (All new files are reported as Added.)")

  gitstat <- systemCall("git", c("diff", "--name-status", ref1, ref2),
                        rundir = pkg$path)

  if (gitstat$status != 0) {
    # Git failed to run for some reason. it would be nice to print the output,
    # but we can't because of issues with system2 in systemCall
    stop("git returned code ", gitstat$status, ". Make sure you use valid git commit refs.")
  } else if (length(gitstat$output) == 0) {
    # There were no changes; create an empty data frame
    changed <- data.frame(V1=character(), V2=character())
  } else {
    # There were some changes
    changed <- read.table(con <- textConnection(gitstat$output), stringsAsFactors = FALSE)
    close(con)
  }
  changed <- setNames(changed, c("status", "filename"))
  changed <- subset(changed, grepl("^visual_test/", filename))

  # Special case where ref2 is the working tree. This is a bit hacky. Add all
  # the untracked files in the visual_test dir. Because they're not committed, we
  # can't tell exactly which files *should* be compared. So copy all the untracked
  # files over.
  if (ref2 == "") {
    wfiles <- systemCall("git", c("ls-files", "--other", "--exclude-standard", "visual_test/"),
                rundir = pkg$path)

    if (length(wfiles) > 0)
      changed <- rbind(changed, data.frame(status = "A", filename = wfiles$output,
                                           stringsAsFactors = FALSE))
  }

  if (nrow(changed) == 0) return(changed)

  # use 'filter' on the second part of the path (right after visual_test/)
  cpaths <- strsplit(changed$filename,"/")
  changed[grepl(filter, sapply(cpaths, "[[", 2)), ]
}

