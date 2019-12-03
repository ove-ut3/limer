#' Export data from a LimeSurvey survey
#'
#' This function exports and downloads data from a LimeSurvey survey.
#' @param iSurveyID \dots
#' @param sDocumentType \dots
#' @param sLanguageCode \dots
#' @param sCompletionStatus \dots
#' @param sHeadingType \dots
#' @param sResponseType \dots
#' @param \dots Further arguments to \code{\link{call_limer}}.
#' @export
#' @examples \dontrun{
#' get_responses(12345)
#' }
get_responses <- function(iSurveyID, sDocumentType = "csv", sLanguageCode = NULL,
                          sCompletionStatus = "complete", sHeadingType = "code",
                          sResponseType = "long", sep=";", session = FALSE, ...) {

  if (session == TRUE) {
    key <- limer::get_session_key()
  }

  if (any(limer::completed_responses(iSurveyID, session = FALSE) != 0)) {

    iSurveyID <- iSurveyID[which(limer::completed_responses(iSurveyID, session = FALSE) != 0)]

    responses <- iSurveyID %>%
      purrr::map_df(
        get_responses_, sResponseType = "short", sCompletionStatus = sCompletionStatus,
        .id = "id_join"
      ) %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(
        dplyr::tibble(survey_id = iSurveyID) %>%
          dplyr::mutate(id_join = as.character(dplyr::row_number())),
        by = "id_join"
      ) %>%
      dplyr::select(-id_join) %>%
      dplyr::mutate_if(is.character, iconv, from = "UTF-8")

  } else {
    responses <- dplyr::tibble(
      survey_id = character(0),
      token = character(0),
      submitdate = lubridate::ymd_hms(character(0)),
      lastpage = integer(0)
    )
  }

  if (session == TRUE) {
    release <- limer::release_session_key()
  }

  return(responses)
}

#' Export data from a LimeSurvey survey
#'
#' @param iSurveyID \dots
#' @param session \dots
#'
#' @export
completed_responses <- function(iSurveyID, session = FALSE) {

  if (session == TRUE) {
    key <- limer::get_session_key()
  }

  n_reponses <- purrr::map_chr(iSurveyID, ~ limer::call_limer("get_summary", list("iSurveyID" = ., "sStatName" = "completed_responses"))) %>%
    as.integer()

  if (session == TRUE) {
    release <- limer::release_session_key()
  }

  return(n_reponses)
}
