#' get_attributes_descriptions
#'
#' @param iSurveyID \dots
#'
#' @export
get_attributes_descriptions <- function(iSurveyID) {

  attributes_descriptions <- limer::call_limer("get_survey_properties", list("iSurveyID" = iSurveyID, list("aSurveySettings" = "attributedescriptions")))

  attributes_descriptions <- jsonlite::fromJSON(attributes_descriptions[["attributedescriptions"]])

  attributes_descriptions <- sapply(attributes_descriptions, `[[`, 1)

  return(attributes_descriptions)
}