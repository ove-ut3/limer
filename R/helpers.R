#' export_survey
#'
#' @param lss_file \dots
#' @param id \dots
#'
#' @export
export_survey <- function(lss_file, id = TRUE) {

  xml <- xml2::read_xml(lss_file)

  # Survey properties

  survey_properties <- xml %>%
    xml2::xml_find_all("/document/surveys/rows/row") %>%
    xml2::xml_contents()

  survey_properties <- dplyr::tibble(
    key = xml2::xml_name(survey_properties),
    value = xml2::xml_text(survey_properties)
  ) %>%
    dplyr::filter(key != "attributedescriptions")

  survey_properties_language <- xml %>%
    xml2::xml_find_all("/document/surveys_languagesettings/rows/row") %>%
    xml2::xml_contents()

  language <- xml %>%
    xml2::xml_find_all("/document/surveys_languagesettings/rows/row/surveyls_language") %>%
    xml2::xml_text()

  survey_properties_language <- dplyr::tibble(
    key = xml2::xml_name(survey_properties_language),
    value = xml2::xml_text(survey_properties_language)
  ) %>%
    dplyr::filter(key != "attachments") %>%
    tidyr::nest_legacy(value) %>%
    dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., language = !!language))) %>%
    tidyr::unnest_legacy(data) %>%
    dplyr::select(key, language, value) %>%
    dplyr::anti_join(limer::survey_properties, by = c("key", "language", "value" = "default"))

  survey_properties <- dplyr::bind_rows(survey_properties, survey_properties_language) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "") %>%
    tidyr::drop_na(value) %>%
    dplyr::anti_join(limer::survey_properties, by = c("key", "value" = "default")) %>%
    dplyr::select(key, language, value)

  # Participants attributes

  participants_attributes <- xml %>%
    xml2::xml_find_all("/document/surveys/rows/row/attributedescriptions") %>%
    xml2::xml_text() %>%
    jsonlite::fromJSON() %>%
    purrr::map_df(dplyr::as_tibble, .id = "attribute") %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")

  # groups

  groups <- xml %>%
    xml2::xml_find_all("/document/groups/rows/row") %>%
    xml2::xml_contents()

  gid <- xml %>%
    xml2::xml_find_all("/document/groups/rows/row/gid") %>%
    xml2::xml_text()
  language <- xml %>%
    xml2::xml_find_all("/document/groups/rows/row/language") %>%
    xml2::xml_text()

  groups <- dplyr::tibble(
    key = xml2::xml_name(groups),
    value = xml2::xml_text(groups)
  ) %>%
    tidyr::nest_legacy(value) %>%
    dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., gid = !!gid, language = !!language))) %>%
    tidyr::unnest_legacy(data) %>%
    tidyr::spread(key, value) %>%
    dplyr::arrange(as.integer(group_order)) %>%
    dplyr::select(group_order, language, group_name, description, grelevance, gid) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "") %>%
    tidyr::drop_na(group_name)

  if (!is.null(groups[["language"]])) {

    language <- survey_properties %>%
      dplyr::filter(key == "language") %>%
      dplyr::pull(value)

    groups <- groups %>%
      dplyr::anti_join(
        groups %>%
          dplyr::semi_join(dplyr::select(., -language) %>%
                             patchr::duplicate(),
                           by = "group_order") %>%
          dplyr::filter(language != !!language),
        by = c("group_order", "language")
      )

  }

  # Questions

  questions <- xml %>%
    xml2::xml_find_all("/document/questions/rows/row") %>%
    xml2::xml_contents()

  qid <- xml %>%
    xml2::xml_find_all("/document/questions/rows/row/qid") %>%
    xml2::xml_text()
  language <- xml %>%
    xml2::xml_find_all("/document/questions/rows/row/language") %>%
    xml2::xml_text()

  questions <- dplyr::tibble(
    key = xml2::xml_name(questions),
    value = xml2::xml_text(questions)
  ) %>%
    dplyr::filter(key != "modulename") %>%
    tidyr::nest_legacy(value) %>%
    dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., qid = !!qid, language = !!language))) %>%
    tidyr::unnest_legacy(data) %>%
    tidyr::spread(key, value) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")

  if (!is.null(questions[["language"]])) {

    language <- survey_properties %>%
      dplyr::filter(key == "language") %>%
      dplyr::pull(value)

    questions <- questions %>%
      dplyr::anti_join(
        questions %>%
          dplyr::semi_join(dplyr::select(., -language) %>%
                             patchr::duplicate(),
                           by = "title") %>%
          dplyr::filter(language != !!language),
        by = c("title", "language")
      )

  }

  questions <- groups %>%
    dplyr::select(gid, group_order) %>%
    unique() %>%
    dplyr::inner_join(questions, by = "gid")

  questions_attributes_1 <- questions %>%
    dplyr::select(qid, title, mandatory, other, relevance, preg, same_default) %>%
    tidyr::gather("key", "value", -title, -qid, na.rm = TRUE) %>%
    dplyr::anti_join(limer::question_attributes, by = c("key" = "attribute", "value" = "default")) %>%
    unique() %>%
    dplyr::select(title, key, value, qid)

  questions <- dplyr::select(questions, group_order, title, type, question, help, language, qid, gid)

  # Question attributes

  questions_attributes <- dplyr::tibble(
    qid = xml %>%
      xml2::xml_find_all("/document/question_attributes/rows/row/qid") %>%
      xml2::xml_text(),
    key = xml %>%
      xml2::xml_find_all("/document/question_attributes/rows/row/attribute") %>%
      xml2::xml_text(),
    value = xml %>%
      xml2::xml_find_all("/document/question_attributes/rows/row/value") %>%
      xml2::xml_text()
  )

  questions_attributes_language <- questions_attributes %>%
    dplyr::semi_join(
      limer::question_attributes %>%
        dplyr::filter(stringr::str_detect(options, "i18n")),
      by = c("key" = "attribute")
    )

  questions_attributes <- questions_attributes %>%
    dplyr::semi_join(
      limer::question_attributes %>%
        dplyr::filter(!stringr::str_detect(options, "i18n")),
      by = c("key" = "attribute")
    ) %>%
    dplyr::left_join(questions %>%
                       dplyr::select(qid, title) %>%
                       unique(),
                     by = "qid") %>%
    dplyr::select(title, key, value, qid)

  default_values <- dplyr::tibble(
   qid = xml %>%
     xml2::xml_find_all("/document/defaultvalues/rows/row/qid") %>%
     xml2::xml_text(),
   key = "default_value",
   value = xml %>%
     xml2::xml_find_all("/document/defaultvalues/rows/row/defaultvalue") %>%
     xml2::xml_text()
  ) %>%
    dplyr::left_join(questions %>%
                       dplyr::select(qid, title) %>%
                       unique(),
                     by = "qid") %>%
    dplyr::select(title, key, value, qid)

  questions_properties <- dplyr::bind_rows(questions_attributes, questions_attributes_1, default_values)

  if (nrow(questions_attributes_language) >= 1) {

    questions_attributes_language <- questions_attributes_language %>%
      dplyr::mutate(language = xml %>%
                      xml2::xml_find_all("/document/question_attributes/rows/row/language") %>%
                      xml2::xml_text()) %>%
      dplyr::left_join(questions %>%
                         dplyr::select(qid, title) %>%
                         unique(),
                       by = "qid") %>%
      dplyr::select(title, key, language, value, qid)

    questions_properties <- dplyr::bind_rows(questions_properties, questions_attributes_language) %>%
      dplyr::select(title, key, language, value, qid)

  }

  questions_properties <- questions %>%
    dplyr::select(title) %>%
    unique %>%
    dplyr::inner_join(
      questions_properties %>%
        dplyr::arrange(key, language),
      by = "title"
    ) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")

  # Subquestions

  subquestions <- dplyr::tibble(
    qid = xml %>%
      xml2::xml_find_all("/document/subquestions/rows/row/qid") %>%
      xml2::xml_text(),
    parent_qid = xml %>%
      xml2::xml_find_all("/document/subquestions/rows/row/parent_qid") %>%
      xml2::xml_text(),
    language = xml %>%
      xml2::xml_find_all("/document/subquestions/rows/row/language") %>%
      xml2::xml_text(),
    title = xml %>%
      xml2::xml_find_all("/document/subquestions/rows/row/title") %>%
      xml2::xml_text(),
    question = xml %>%
      xml2::xml_find_all("/document/subquestions/rows/row/question") %>%
      xml2::xml_text()
  )

  subquestions <- questions %>%
    dplyr::select(qid, question_title = title) %>%
    unique() %>%
    dplyr::inner_join(subquestions, by = c("qid" = "parent_qid")) %>%
    dplyr::select(question_title, title, language, question, qid) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")

    if (!is.null(subquestions[["language"]])) {

      language <- survey_properties %>%
        dplyr::filter(key == "language") %>%
        dplyr::pull(value)

      subquestions <- subquestions %>%
        dplyr::anti_join(
          subquestions %>%
            dplyr::semi_join(dplyr::select(., -language) %>%
                               patchr::duplicate(),
                             by = c("question_title", "title")) %>%
            dplyr::filter(language != !!language),
          by = c("question_title", "title", "language")
        )

    }

  # Answers

  answers <- dplyr::tibble(
    qid = xml %>%
      xml2::xml_find_all("/document/answers/rows/row/qid") %>%
      xml2::xml_text(),
    language = xml %>%
      xml2::xml_find_all("/document/answers/rows/row/language") %>%
      xml2::xml_text(),
    code = xml %>%
      xml2::xml_find_all("/document/answers/rows/row/code") %>%
      xml2::xml_text(),
    answer = xml %>%
      xml2::xml_find_all("/document/answers/rows/row/answer") %>%
      xml2::xml_text()
  )

  answers <- questions %>%
    dplyr::select(qid, question_title = title) %>%
    unique() %>%
    dplyr::inner_join(answers, by = "qid") %>%
    dplyr::select(question_title, code, language, answer, qid) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")

  if (!is.null(answers[["language"]])) {

    language <- survey_properties %>%
      dplyr::filter(key == "language") %>%
      dplyr::pull(value)

    answers <- answers %>%
      dplyr::anti_join(
        answers %>%
          dplyr::semi_join(dplyr::select(., -language) %>%
                             patchr::duplicate(),
                           by = c("question_title", "code")) %>%
          dplyr::filter(language != !!language),
        by = c("question_title", "code", "language")
      )

  }

  if (id == FALSE) {
    survey_properties <- dplyr::filter(survey_properties, !key %in% c("sid", "surveyls_survey_id"))
    groups$gid <- NULL
    questions <- dplyr::select(questions, -gid, -qid)
    questions_properties$qid <- NULL
    subquestions$qid <- NULL
    answers$qid <- NULL
  }

  survey <- list(
    "survey_properties" = survey_properties,
    "participants_attributes" = participants_attributes,
    "groups" = groups,
    "questions" = questions,
    "questions_properties" = questions_properties,
    "subquestions" = subquestions,
    "answers" = answers
  )

  return(survey)
}

#' conv_question_type
#'
#' @param limer_type \dots
#'
#' @export
conv_question_type <- function(limer_type) {

  conv_question_type <- dplyr::tibble(limer_type) %>%
    dplyr::left_join(limer::question_types, by = "limer_type") %>%
    dplyr::pull(type)

  return(conv_question_type)

}
