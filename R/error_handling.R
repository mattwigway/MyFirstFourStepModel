#' Abort with a nice error message if there is an error in Rust.
#' @keywords internal
abort_on_error = function(result) {
  if (!is.null(result$err)) {
    cli_abort(result$err)
  } else {
    result$ok
  }
}
