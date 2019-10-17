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

  sCompletionStatus <- dplyr::recode(perimetre, "toutes" = "all", "completes" = "complete", "incompletes" = "incomplete")

  if (any(limer::completed_responses(iSurveyID, session = FALSE) != 0)) {

    reponses <- iSurveyID %>%
      purrr::map_df(get_responses_, sResponseType = "short", sCompletionStatus = sCompletionStatus,
                    .id = "id_join") %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(dplyr::tibble(id_survey = iSurveyID) %>%
                         dplyr::mutate(id_join = as.character(dplyr::row_number())),
                       by = "id_join") %>%
      dplyr::select(-id_join) %>%
      dplyr::mutate_at(which(purrr::map_lgl(., is.character)), iconv, from = "UTF-8") %>%
      dplyr::mutate(date_heure_reponse = lubridate::ymd_hms(submitdate),
                    date_reponse = lubridate::as_date(date_heure_reponse))

  } else {
    reponses <- dplyr::tibble(token = character(0), datestamp = character(0))
  }

  if (session == TRUE) {
    release <- limer::release_session_key()
  }

  return(reponses)
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
