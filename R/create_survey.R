#' Create a complete survey
#'
#' This function is based on \link{add_survey}, \link{add_group} and \link{add_question} functions.\cr
#' They are all three using the original xml templates (LSS, LSG and LSQ) allowing to fully automatise the survey creation process.\cr
#' Data frames are expected for all of the arguments except survey_id.
#'
#' @param survey_id Survey id
#' @param survey_properties Survey properties
#' @param groups Question groups
#' @param questions Questions
#' @param questions_properties Questions properties
#' @param subquestions Subquestions
#' @param answers Answers
#' @param participants_attributes Participants attributes
#' @param quiet If \code{TRUE} then there is no output during survey creation
#'
#' @examples \dontrun{
#'
#' limer::create_survey(
#'   survey_id = 123456,
#'   survey_properties = data.frame(
#'     key = c("language", "surveyls_title", "admin", "showprogress"),
#'     language = c(NA, "en", NA, NA),
#'     value = c("en", "Survey title", "Survey administrator", "Y")
#'   ),
#'   groups = data.frame(
#'     group_order = c(1, 2),
#'     group_name = c("Group 1", "Group 2"),
#'     description = c("Description 1", NA)
#'   ),
#'   questions = data.frame(
#'     group_order = c(1, 1, 2),
#'     type = c("T", "S", "F"),
#'     title = c("q1", "q2", "q3"),
#'     question = c("Text question", "Short text question", "Array radio question"),
#'     help = c("Help 1", NA, NA)
#'   ),
#'   questions_properties = data.frame(
#'     question_title = c("q1", "q2", "q3"),
#'     key = c("mandatory", "default", "other"),
#'     value = c("Y", "{TOKEN:ATTRIBUTE_1}", "Y")
#'   ),
#'   subquestions = data.frame(
#'     question_title = c("q3", "q3"),
#'     title = c("SQ001", "SQ002"),
#'     question = c("Subquestion 1", "Subquestion 2")
#'   ),
#'   answers = data.frame(
#'     question_title = c("q3", "q3", "q3"),
#'     code = c("A1", "A2", "A3"),
#'     answer = c("Answer 1", "Answer 2", "Answer 3")
#'   ),
#'   participants_attributes = data.frame(
#'     description = c("attribute 1 description")
#'   )
#' )
#' }
#'
#' @export
create_survey <- function(survey_id, survey_properties, groups, questions, questions_properties = NULL, subquestions = NULL, answers = NULL, participants_attributes = NULL, quiet = FALSE) {

  survey_languages <- survey_properties %>%
    dplyr::filter(key %in% c("language", "additional_languages")) %>%
    dplyr::pull(value)

  # languages tests between survey, groups, questions, subquestions and surveys
  missing_languages <- setdiff(groups$language, survey_languages)
  if (length(missing_languages) >= 1) {
    stop("Group language(s) ", paste0(missing_languages, collapse = ", ")," are not defined in survey languages")
  }
  missing_languages <- setdiff(questions$language, survey_languages)
  if (length(missing_languages) >= 1) {
    stop("Question language(s) ", paste0(missing_languages, collapse = ", ")," are not defined in survey languages")
  }
  missing_languages <- setdiff(subquestions$language, survey_languages)
  if (length(missing_languages) >= 1) {
    stop("Subquestion language(s) ", paste0(missing_languages, collapse = ", ")," are not defined in survey languages")
  }
  missing_languages <- setdiff(answers$language, survey_languages)
  if (length(missing_languages) >= 1) {
    stop("Answer language(s) ", paste0(missing_languages, collapse = ", ")," are not defined in survey languages")
  }

  survey_id <- limer::add_survey(survey_id, survey_properties, participants_attributes, quiet)

  activation <- limer::call_limer(
    "activate_tokens",
    params = list("iSurveyID" = survey_id,
                  "aAttributeFields" = list(1)
    )
  )

  if (is.null(groups[["language"]])) {
    groups$language <- survey_languages[1]
  }
  groups <- tidyr::nest_legacy(groups, -group_order)

  groups_id <- purrr::map_chr(groups$data, ~ limer::add_group(survey_id, ., quiet = quiet))

  groups$gid <- as.integer(groups_id)

  if (is.null(questions[["language"]])) {
    questions$language <- survey_languages[1]
  }
  if (is.null(subquestions[["language"]])) {
    subquestions$language <- survey_languages[1]
  }
  if (is.null(answers[["language"]])) {
    answers$language <- survey_languages[1]
  }

  questions <- questions %>%
    tidyr::nest_legacy(-group_order, -title, -type, .key = "question") %>%
    dplyr::left_join(
      questions_properties %>%
        tidyr::drop_na(language) %>%
        tidyr::nest_legacy(language, value, .key = "value") %>%
        dplyr::mutate_at("value", ~ purrr::map_if(., ~ nrow(.) >= 2, ~ split(.$value, .$language))) %>%
        dplyr::bind_rows(
          questions_properties %>%
            dplyr::filter(is.na(language)) %>%
            dplyr::select(-language) %>%
            dplyr::mutate_at("value", as.list)
        ) %>%
        tidyr::nest_legacy(-title, .key = "properties") %>%
        dplyr::mutate_at("properties", ~ purrr::map(., ~ tidyr::spread(., key, value))) %>%
        dplyr::mutate_at("properties", ~ purrr::map(., ~ as.list(.))) %>%
        dplyr::mutate_at("properties", ~ purrr::map(., ~ unlist(., recursive = FALSE))),
      by = "title"
    ) %>%
    dplyr::left_join(
      subquestions %>%
        tidyr::nest_legacy(-question_title, .key = "subquestions"),
      by = c("title" = "question_title")
    ) %>%
    dplyr::left_join(
      answers %>%
        tidyr::nest_legacy(-question_title, .key = "answers"),
      by = c("title" = "question_title")
    )

  questions <- groups %>%
    dplyr::select(group_order, gid) %>%
    dplyr::right_join(questions, by = "group_order")

  purrr::pwalk(
    list(questions$gid, questions$title, questions$question, questions$type, questions$properties, questions$subquestions, questions$answers),
    ~ limer::add_question(
      survey_id = survey_id,
      group_id = ..1,
      title = ..2,
      question = ..3,
      type = ..4,
      properties = ..5,
      subquestions = ..6,
      answers = ..7,
      quiet = quiet
    )
  )

  return(survey_id)
}
