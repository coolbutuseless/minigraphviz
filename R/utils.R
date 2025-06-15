#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Idea borrowed from \code{ggplot2::theme()}
#'
#' Use of this function negates having to define default values for all the
#' arguments in the parent function
#'
#' @param ... all arguments from call to parent function
#'
#' @return list with only arguments used in the call
#' @importFrom utils modifyList
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
find_args <- function (...) {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))
  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, function(x) {identical(x, quote(expr=))}, logical(1))]
  args <- modifyList(vals, list(..., ... = NULL))
  
  args
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Assert a list object has elements which are all named
#' 
#' @param ll list
#' @return None
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assert_named <- function(ll) {
  if (length(ll) == 0) return()
  nms <- names(ll)
  stopifnot(
    !is.null(nms) &&  !anyNA(nms) && !any(nms == "")
  )
}



assert_dot <- function(d) {
  stopifnot(inherits(d, 'dot'))
}
