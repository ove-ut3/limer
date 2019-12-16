#' update_responses
#'
#' @param survey_id \dots
#' @param token \dots
#' @param question \dots
#' @param value \dots
#' @param session \dots
#'
#' @examples
#'
#' Simple question
#' sidXgidXqid
#'
#' Array question
#' sidXgidXqidTitle
#'
#' @export
update_responses <- function(survey_id, token, question, value, session = FALSE) {

  if (session == TRUE) {
    key <- limer::get_session_key()
  }

  update_responses <- limer::call_limer(
    "update_response",
    params = list(
      "iSurveyID" = survey_id,
      "aResponseData" = list(
        token,
        value
      ) %>%
        setNames(c("token", question))
    )
  )

  if (session == TRUE) {
    release <- limer::release_session_key()
  }

  return(update_responses)

}