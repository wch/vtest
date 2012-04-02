get_vcontext <- NULL
set_vcontext <- NULL
get_vcontext_count <- NULL
inc_vcontext_count <- NULL


local({
  context <- NULL       # The context of a set of tests (usually in one script)
  context_count <- NULL # Keep count of tests, within this context

  # These are used by each test script
  get_vcontext <<- function() context
  set_vcontext <<- function(value) {
    context <<- value
    context_count <<- 0
  }

  get_vcontext_count <<- function() context_count
  inc_vcontext_count <<- function(value) context_count <<- context_count + 1

})


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
