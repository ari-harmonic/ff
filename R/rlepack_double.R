#' Rlepack for doubles
#'
#' Rlepack for doubles
#'
#' @return \code{NULL}
#'
#' @rdname rlepack
#' @method rlepack double
#' @export
rlepack.double = function (x, pack = TRUE, ...)
{
  #stopifnot(is.integer(x))
  n <- length(x)
  if (n > 1) {
    if (pack)
      r <- .Call("int_rle", (diff(x)), PACKAGE = "bit")
    else r <- NULL
    structure(list(first = x[1], dat = if (is.null(r)) x else r,
                   last = x[n]), class = "rlepack")
  }
  else if (n == 1) {
    structure(list(first = x[1], dat = x, last = x[1]),
              class = "rlepack")
  }
  else {
    structure(list(first = as.integer(NA), dat = x, last = as.integer(NA)),
              class = "rlepack")
  }
}
