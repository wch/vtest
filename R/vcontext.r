
# An environment to hold information about a context
vc <- new.env(hash = TRUE, parent = emptyenv())

vc$context <- NULL       # The context of a set of tests (usually in one script)
vc$context_count <- NULL # Keep count of tests, within this context

# These are used by each test script
get_vcontext <- function() vc$context
set_vcontext <- function(value) {
  vc$context <- value
  vc$context_count <- 0
}

get_vcontext_count <- function() vc$context_count
inc_vcontext_count <- function(value) vc$context_count <- vc$context_count + 1


#' Start a visual test context
#' @param context name of the context
#' @export
vcontext <- function(context) {
  if (!is.null(get_vcontext()))
    stop("Can't open new context while current context is still open. Use end_vcontext().")

  set_vcontext(context)
  message(context, appendLF = FALSE)
}


#' Finish a visual test context.
#' @export
end_vcontext <- function() {
  if(is.null(get_vcontext())) {
    message("No open vcontext to end.")
    return(invisible())
  }

  set_vcontext(NULL)  # Reset the context
  message("")         # Print a newline
}
