#' Add a question to a survey
#'
#' This function adds a question to an existing survey using a LSQ template file.\cr
#' This can be a multilingual question (see details).\cr
#' It also accepts additionnal properties (see table \link{question_attributes}).
#'
#' @param survey_id Survey id
#' @param group_id Question group id
#' @param title Question code
#' @param type Question type (see \url{https://manual.limesurvey.org/Expression_Manager#Qcode_Variable_Naming} and table \link{question_types})
#' @param question Question text\cr
#' It can be a single character in case of a single language survey.\cr
#' Otherwise, a data frame with 2 columns is required : \code{language} (for question language) and \code{question} (for question text).\cr
#' 1 optional column can be added : \code{help} (for question help).
#' @param properties Question properties as a named list (see \url{https://api.limesurvey.org/classes/Question.html} and table \link{question_attributes})\cr
#' Additional question properties are accepted.\cr
#' A list element can contain multiple values in case of a multilingual attribute (See \code{limer::question_attributes} with attributes whose option equals \code{'i18n'=>true}).
#' @param subquestions Subquestions data frame with 4 columns : \code{language} (for subquestion language), \code{title} (for subquestion code), \code{question} (for subquestion text) and \code{relevance} (for conditionnal equation).\cr
#' @param answers Answers data frame with 3 columns : \code{language} (for answer language), \code{code} (for answer code) and \code{answer} (for answer text).\cr
#' @param quiet If \code{TRUE} then there is no output during question creation
#'
#' @details
#'
#' If any of the 3 multi-languages arguments is set (eg : \code{text}, \code{subquestions} and \code{answers}) then all these 3 arguments must conform to multi-languages specifications.\cr
#' Otherwise a error is raised.
#'
#' @examples \dontrun{
#'
#' # Example with long_text
#' limer::add_question(
#'   survey_id = 123456,
#'   group_id = 12345,
#'   title = "q1",
#'   type = "T",
#'   question = data.frame(
#'     question = "Text question",
#'     help = "Help 1"
#'   ),
#'   properties = data.frame(
#'     key = "mandatory",
#'     value = "Y"
#'   )
#' )
#'
#' # Example with array_radio
#'
#' limer::add_question(
#'   survey_id = 123456,
#'   group_id = 12345,
#'   title = "q3",
#'   type = "F",
#'   question = "Array radio question",
#'   properties = data.frame(
#'     key = "other",
#'     value = "Y"
#'   ),
#'   subquestions = data.frame(
#'     title = c("SQ001", "SQ002"),
#'     question = c("Subquestion 1", "Subquestion 2")
#'   ),
#'   answers = data.frame(
#'     code = c("A1", "A2", "A3"),
#'     answer = c("Answer 1", "Answer 2", "Answer 3")
#'   )
#' )
#' }
#'
#' @export
add_question <- function(survey_id, group_id, title, type, question, properties, subquestions = NULL, answers = NULL, quiet = FALSE) {

  if (nchar(title) > 20) {
    stop("A question code can not exceed 20 characters", call. = FALSE)
  }

  if (nchar(stringr::str_remove(title, "[[:alnum:]]+")) >= 1) {
    stop("A question code accepts only alphebetical or numeric characters", call. = FALSE)
  }

  if (!is.null(subquestions)) {

    if (any(nchar(subquestions$title) > 20)) {
      stop("A Subquestion code can not exceed 20 characters", call. = FALSE)
    }

    if (any(nchar(stringr::str_remove(subquestions$title, "[[:alnum:]]+")) >= 1)) {
      stop("A Subquestion code accepts only alphebetical or numeric characters", call. = FALSE)
    }

    if (!all(!is.null(question[["language"]]) & !is.null(subquestions[["language"]]))) {
      stop("If any of the 3 multi-languages arguments is set (eg : \"question\", \"subquestions\" and \"answers\") then all these arguments must conform to multi-languages specifications", call. = FALSE)
    }
  }

  if (!is.null(answers)) {

    if (any(nchar(answers$code) > 5)) {
      stop("An answer code can not exceed 20 characters", call. = FALSE)
    }

    if (any(nchar(stringr::str_remove(answers$code, "[[:alnum:]]+")) >= 1)) {
      stop("An answer code accepts only alphebetical or numeric characters", call. = FALSE)
    }

    if (!all(!is.null(question[["language"]]) & !is.null(answers[["language"]]))) {
      stop("If any of the 3 multi-languages arguments is set (eg : \"question\", \"subquestions\" and \"answers\") then all these arguments must conform to multi-languages specifications", call. = FALSE)
    }

  }

  if (is.null(answers) & type %in% c("L", "!", "O", "F", "H", "1", "R")) {
    stop(glue::glue("Question {title} of type {type} must have at least one answer"), call. = FALSE)
  }

  xml <- xml2::read_xml(system.file(paste0("extdata/question.lsq"), package = "limer"))

  xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/gid"), as.character(group_id))
  xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/title"), title)
  xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/type"), type)

  # Question text
  if (is.character(question)) {
    question <- dplyr::tibble(
      language = limer::call_limer("get_survey_properties",
                                   params = list("iSurveyID" = survey_id, "aSurveySettings" = list("language"))) %>%
        .[["language"]],
      question = question

    )
  }

  question <- dplyr::mutate_if(question, is.character, tidyr::replace_na, "")

  # The question template is copied for the secondaries languages
  for (num in utils::tail(seq_along(question$language), -1)) {
    xml2::xml_add_child(xml2::xml_find_all(xml, "/document/questions/rows"), xml2::xml_find_first(xml, "/document/questions/rows/row"))
  }

  for (num in seq_along(question$question)) {

    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/question")[num], question$question[num])

    xml2::xml_add_child(xml2::xml_find_all(xml, "/document/languages"), "language", question$language[num])
    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/language")[num], question$language[num])

    if (!is.null(question[["help"]])) {
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/help")[num], question$help[num])
    }
  }

  # Subquestions
  if (is.null(subquestions)) {
    xml2::xml_remove(xml2::xml_find_all(xml, "/document/subquestions"))
  } else {

    subquestions <- dplyr::mutate_if(subquestions, is.character, tidyr::replace_na, "") %>%
      dplyr::left_join(dplyr::select(., title) %>%
                         unique() %>%
                         dplyr::mutate(question_order = dplyr::row_number()),
                       by = "title")

    # The question template is copied for the secondaries languages
    for (num in utils::tail(seq_along(subquestions$title), -1)) {
      xml2::xml_add_child(xml2::xml_find_all(xml, "/document/subquestions/rows"), xml2::xml_find_first(xml, "/document/subquestions/rows/row"))
    }

    for (num in seq_along(subquestions$title)) {

      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/subquestions/rows/row/gid"), as.character(group_id))
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/subquestions/rows/row/qid"), as.character(subquestions$question_order))
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/subquestions/rows/row/type"), type)
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/subquestions/rows/row/question_order"), as.character(subquestions$question_order))
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/subquestions/rows/row/title")[num], subquestions$title[num])
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/subquestions/rows/row/question")[num], subquestions$question[num])
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/subquestions/rows/row/language")[num], subquestions$language[num])

    }

  }

  # Answers
  if (is.null(answers)) {
    xml2::xml_remove(xml2::xml_find_all(xml, "/document/answers"))
  } else {

    answers <- dplyr::mutate_if(answers, is.character, tidyr::replace_na, "") %>%
      dplyr::left_join(dplyr::select(., code) %>%
                         unique() %>%
                         dplyr::mutate(sortorder = dplyr::row_number()),
                       by = "code")

    # The question template is copied for the secondaries languages
    for (num in utils::tail(seq_along(answers$language), -1)) {
      xml2::xml_add_child(xml2::xml_find_all(xml, "/document/answers/rows"), xml2::xml_find_first(xml, "/document/answers/rows/row"))
    }

    for (num in seq_along(answers$code)) {

      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/answers/rows/row/code")[num], answers$code[num])
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/answers/rows/row/answer")[num], answers$answer[num])
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/answers/rows/row/language")[num], answers$language[num])
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/answers/rows/row/sortorder")[num], as.character(answers$sortorder[num]))

    }

  }

  # Stock properties : relevance, other, mandatory, preg, same_default
  if (!is.null(properties[["relevance"]])) {
    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/relevance"), properties$relevance)
    properties[["relevance"]] <- NULL
  }
  if (!is.null(properties[["other"]])) {
    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/other"), properties$other)
    properties[["other"]] <- NULL
  }
  if (!is.null(properties[["mandatory"]])) {
    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/mandatory"), properties$mandatory)
    properties[["mandatory"]] <- NULL
  }
  if (!is.null(properties[["preg"]])) {
    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/preg"), properties$preg)
    properties[["preg"]] <- NULL
  }
  if (!is.null(properties[["same_default"]])) {
    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/questions/rows/row/same_default"), properties$same_default)
    properties[["same_default"]] <- NULL
  }

  # Default Value
  if (!is.null(properties[["default_value"]])) {

    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/defaultvalues/rows/row/defaultvalue"), properties$default_value[[1]])
    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/defaultvalues/rows/row/language"), limer::call_limer("get_survey_properties", params = list("iSurveyId" = survey_id, "aSurveySettings" = list("language"))))

    properties[["default_value"]] <- NULL
  } else {
    xml2::xml_remove(xml2::xml_find_all(xml, "/document/defaultvalues"))
  }

  # Additional properties
  if (length(properties) == 0) {
    xml2::xml_remove(xml2::xml_find_all(xml, "/document/question_attributes"))
  } else {

    properties <- dplyr::tibble(
      key = names(unlist(properties)),
      value = unlist(properties)
    ) %>%
      dplyr::mutate(language = stringr::str_match(key, "\\.([a-z]{2})$")[, 2],
                    key = dplyr::if_else(!is.na(language),
                                         stringr::str_match(key, "(.+?)\\.([a-z]{2})$")[, 2],
                                         key))

    # The attribute template is copied for the secondaries attributes
    for (num in utils::tail(1:nrow(properties), -1)) {
      xml2::xml_add_child(xml2::xml_find_all(xml, "/document/question_attributes/rows"), xml2::xml_find_first(xml, "/document/question_attributes/rows/row"))
    }

    for (num in 1:nrow(properties)) {

      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/question_attributes/rows/row/attribute")[num], properties$key[num])
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/question_attributes/rows/row/value")[num], properties$value[num])

      if (!is.null(properties[["language"]])) {
        if (!is.na(properties$language[num])) {
          xml2::xml_set_text(xml2::xml_find_all(xml, "/document/question_attributes/rows/row/language")[num], properties$language[num])
        }
      }

    }

  }

  tempfile <- tempfile(fileext = ".lsq")
  xml2::write_xml(xml, tempfile)

  qid <- limer::call_limer(
    "import_question",
    params = list(
      "iSurveyID" = survey_id,
      "iGroupID" = group_id,
      "sImportData" = readLines(tempfile, encoding = "UTF-8") %>%
        paste0(collapse = "") %>%
        limer::str_to_base64(),
      "sImportDataType" = "lsq"
    ))

  if (class(qid) == "list") {
    stop(qid$status, call. = FALSE)
  }

  # If multilingual and mandatory question
  if ("Y" %in% xml2::xml_text(xml2::xml_find_all(xml, "/document/questions/rows/row/mandatory"))) {

    if (nrow(question) >= 2) {
      # If multilingual
      languages <- limer::call_limer("get_survey_properties", params = list("iSurveyId" = survey_id, "aSurveySettings" = list("additional_languages"))) %>%
        stringr::str_split(" ") %>%
        .[[1]]

    } else {
      # If single language
      languages <- limer::call_limer("get_survey_properties", params = list("iSurveyId" = survey_id, "aSurveySettings" = list("language")))
    }

    set_question_properties <- purrr::map(
      languages,
      ~ limer::call_limer(
        "set_question_properties",
        params = list(
          "iQuestionID" = qid,
          "aQuestionData" = list("mandatory" = "Y"),
          "sLanguage" = .
        ))
    )

  }

  if (quiet == FALSE){
    usethis::ui_done("Question {title}")
  }

  return(qid)

}
