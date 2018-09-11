#' Convert string to base64 encoded data
#'
#' This function converts a string into a raw base64 result.
#' @param str \dots
#' @export
#' @examples \dontrun{
#' }

str_to_base64 <- function(str) {

  str_to_base64 <- base64enc::base64encode(charToRaw(paste0(str, collapse = "\n")))

  return(str_to_base64)
}