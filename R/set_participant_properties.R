#' set_participant_properties
#'
#' @param iSurveyID \dots
#' @param tid \dots
#' @param property \dots
#' @param value \dots
#'
#' @export
set_participant_properties <- function(survey_id, tid, property, value) {

  limer::call_limer(
    "set_participant_properties",
    params = list(
      "iSurveyID" = survey_id,
      "aTokenQueryProperties" = tid,
      "aTokenData" = as.list(stats::setNames(value, property))
    )
  )

}