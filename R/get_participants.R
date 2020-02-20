#' Export list of participants from a LimeSurvey survey
#'
#' This function exports and downloads the list of participants from a LimeSurvey survey.
#' @param iSurveyID \dots
#' @param attributes_descriptions \dots
#' @param attributes \dots
#' @param all_attributes \dots
#' @param conditions \dots
#' @param session \dots
#'
#' @export
get_participants <- function(iSurveyID, attributes_descriptions = NULL, attributes = NULL, all_attributes = FALSE, conditions = list(), session = FALSE) {

  if (session == TRUE) {
    key <- limer::get_session_key()
  }

  get_attributes <- function(iSurveyID) {

    attributes_desc <- limer::get_attributes_descriptions(iSurveyID)

    if (all_attributes == TRUE) {
      attributes_descriptions <- attributes_desc
    }

    all_attributes <- character(0)

    if (!is.null(attributes_descriptions)) {

      attributes_desc <- names(attributes_desc)[purrr::map_int(attributes_descriptions, ~ which(. == attributes_desc))]

      if (length(attributes_desc) == 0) {
        message("Pas de champs \"", attributes_descriptions, "\" trouvés...")
      }

      all_attributes <- attributes_desc
    }

    if (!is.null(attributes)) {
      all_attributes <- c(attributes, all_attributes)
    }

    return(all_attributes)
  }
  attributes <- purrr::map(iSurveyID, get_attributes) %>%
    purrr::map(tolower)

  rename <- purrr::map(iSurveyID, limer::get_attributes_descriptions) %>%
    purrr::map( ~ stats::setNames(., tolower(names(.))))
  if (all_attributes == FALSE) {
    rename <- purrr::map(rename, ~ .[which(. %in% attributes_descriptions)])
  }

  participants <- purrr::pmap_dfr(
    list(unname(iSurveyID), attributes, rename),
    ~ get_participants_(..1, aAttributes = as.list(..2), aConditions = conditions) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::matches("^attribute_")), as.character) %>%
      patchr::rename(dplyr::tibble(column = names(..3), rename = ..3), drop = FALSE),
    .id = "id_join"
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(
      dplyr::tibble(survey_id = iSurveyID) %>%
        dplyr::mutate(id_join = as.character(dplyr::row_number())),
      by = "id_join"
    ) %>%
    dplyr::select(-id_join) %>%
    dplyr::mutate(tid = as.integer(tid))

  stock_columns <- c("survey_id", "tid", "firstname", "lastname", "email", "token")
  participants <- participants %>%
    dplyr::select(stock_columns, which(!names(.) %in% stock_columns))

  if (session == TRUE) {
    release <- limer::release_session_key()
  }

  return(participants)
}
