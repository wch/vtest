#' Check the integrity of the vtest database
#'
#' @export
check_vtest_db <- function() {
  checkdb_multiple_commits()
  checkdb_resultset_hashes()
  checkdb_all_commits_in_resultsets()
  checkdb_all_resultsets_in_commits()

  checkdb_all_commits_in_git_history()
  
  checkdb_no_missing_images()
  checkdb_no_extra_images()

  checkdb_image_hashes_match()
}


checkdb_multiple_commits <- function() {
  message("Checking that commits don't have multiple entries in commit table... ",
    appendLF = FALSE)

  commits <- load_commits_table()
  dup <- duplicated(commits$commit)
  if (any(dup)) {
    dc <- commits$commit[dup]
    dc <- git_abbrev_hash(dc)
    message("Multiple entries for ", paste(dc, collapse = ", "))
  } else {
    message("OK (", nrow(commits), " unique commits).")
  }
}


checkdb_resultset_hashes <- function() {
  message("Checking that resultset hashes are correct... ", appendLF = FALSE)

  res <- load_resultsets()
  badcount <- 0

  for (hash in unique(res$resultset_hash)) {
    r <- extract_resultset(res, hash, drop_hash = TRUE)
    newhash <- hash_resultset(r)
    if (newhash != hash) {
      message("Hash mismatch for resultset ", hash)
      badcount <- badcount + 1
    }
  }

  if (badcount != 0)
    message(badcount, " hash mismatches.")
  else
    message("OK (", length(unique(res$resultset_hash)), " resultset hashes checked).")
}


checkdb_all_commits_in_resultsets <- function() {
  message("Checking that all resultset hashes in commit table are also in resultset table... ",
    appendLF = FALSE)

  commits <- load_commits_table()
  res <- load_resultsets()

  ch <- unique(commits$resultset_hash)
  rh <- unique(res$resultset_hash)

  matches <- ch %in% rh
  if (!all(matches)) {
    message("The following resultset hashes are in the commit table but not the resultsets table: ",
      paste(ch[!matches], collapse = ", "))
  } else {
    message("OK (", length(ch), " resultset hashes checked).")
  }
}


checkdb_all_resultsets_in_commits <- function() {
  message("Checking that all resultset hashes in resultset table are also in commit table... ",
    appendLF = FALSE)

  commits <- load_commits_table()
  res <- load_resultsets()

  ch <- unique(commits$resultset_hash)
  rh <- unique(res$resultset_hash)

  matches <- rh %in% ch
  if (!all(matches)) {
    message("The following resultset hashes are in the resultsets table but not the commit table: ",
      paste(rh[!matches], collapse = ", "))
  } else {
    message("OK (", length(rh), " resultset hashes checked).")
  }
}


checkdb_all_commits_in_git_history <- function() {
  message("Checking that all commits in commits table are also in git history... ",
    appendLF = FALSE)

  commits <- load_commits_table()
  cc <- unique(commits$commit)
  gc <- git_prev_commits(n = NULL, all = TRUE)

  matches <- cc %in% gc
  if (!all(matches)) {
    message("The following commits are in the commit table but not the git history: ",
      paste(cc[!matches], collapse = ", "),
      "\n  This may be because some commits were removed by rebasing.")
  } else {
    message("OK (", length(cc), " commits checked).")
  }
}


checkdb_no_missing_images <- function() {
  message("Checking that all result image hashes referenced in resultsets table have files in images/... ",
    appendLF = FALSE)

  res <- load_resultsets()
  imagerefs <- unique(res$hash)
  imagefiles <- dir(get_vtest_imagedir())

  matches <- imagerefs %in% imagefiles
  if (!all(matches)) {
    message("The following image hashes are in the resultsets table but don't have corresponding files in images/: ",
      paste(imagerefs[!matches], collapse = ", "))
  } else {
    message("OK (", length(imagerefs), " image hashes checked).")
  }
}


checkdb_no_extra_images <- function() {
  message("Checking that all files in images/ have matching image hash entries in resultsets table... ",
    appendLF = FALSE)

  res <- load_resultsets()
  imagerefs <- unique(res$hash)
  imagefiles <- dir(get_vtest_imagedir())

  matches <- imagefiles %in% imagerefs
  if (!all(matches)) {
    message("The following image hashes have files in images/ but don't have corresponding entries in the resultsets table: ",
      paste(imagefiles[!matches], collapse = ", "))
  } else {
    message("OK (", length(imagefiles), " files).")
  }
}


checkdb_image_hashes_match <- function() {
  message("Checking that all images have correct hashes... ", appendLF = FALSE)
  imagefiles <- dir(get_vtest_imagedir())
  
  badcount <- 0
  for (i in imagefiles) {
    hash <- digest(file.path(get_vtest_imagedir(), i), file = TRUE)
    if (i != hash) {
      message("Hash mismatch for image ", i, ". Calculated hash is ", hash)
      badcount <- badcount + 1
    }
  }

  if (badcount != 0)
    message(badcount, " hash mismatches.")
  else
    message("OK (", length(imagefiles), " images checked).")
}