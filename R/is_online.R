#' is_online
#'
#' @param survey_id \dots
#'
#' @export
is_online <- function(id_survey) {

  return(as.character(id_survey) %in% limer::call_limer("list_surveys")$sid)

}