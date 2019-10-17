#' Add a survey
#'
#' This function adds a survey using a LSS template file.\cr
#' Compared to add_survey API call, it allows to automatise the participants attributes creation.
#'
#' @param sid Survey id
#' @param properties Survey properties (See table \link{survey_properties})
#' @param attributedescriptions Participants attributes defined in a data.frame
#' @param quiet If \code{TRUE} then there is no output during survey creation
#'
#' @return The created survey id (can differ from the original if already used)
#'
#' @examples \dontrun{
#'
#' limer::add_survey(
#'   survey_id = 123456,
#'   properties = data.frame(
#'     key = c("language", "surveyls_title", "admin", "showprogress"),
#'     language = c(NA, "en", NA, NA),
#'     value = c("en", "Survey title", "Survey administrator", "Y")
#'   ),
#'   participants_attributes = data.frame(
#'     description = c("attribute 1 description")
#'   )
#' )
#' }
#'
#' @export
add_survey <- function(sid, properties, attributedescriptions = NULL, quiet = FALSE) {

  languages <- properties %>%
    dplyr::filter(key %in% c("language", "additional_languages")) %>%
    tidyr::separate_rows(value, sep = " ") %>%
    dplyr::pull(value)

  xml <- xml2::read_xml(system.file(paste0("extdata/survey.lss"), package = "limer"))

  xml2::xml_set_text(xml2::xml_find_all(xml, "/document/surveys/rows/row/sid"), as.character(sid))

  for (num in seq_along(languages)) {
    xml2::xml_add_child(xml2::xml_find_all(xml, "/document/languages"), "language", languages[num])
  }

  # Simple properties
  simple_properties <- properties %>%
    dplyr::semi_join(limer::survey_properties %>%
                       dplyr::filter(!stringr::str_detect(key, "^(surveyls|email)_")),
                     by = "key") %>%
    split(x = .$value, f = .$key)

  for (num in seq_along(simple_properties)) {
    xml2::xml_set_text(
      xml2::xml_find_all(xml, paste0("/document/surveys/rows/row/", names(simple_properties)[[num]])),
      simple_properties[[num]]
    )
  }

  # attributedescriptions
  if (!is.null(attributedescriptions)) {

    names <- attributedescriptions$attribute

    attributedescriptions <- attributedescriptions %>%
      dplyr::select(intersect(names(.), c("description", "mandatory", "show_register", "cpdbmap"))) %>%
      split(f = 1:nrow(.)) %>%
      purrr::map(~ tidyr::gather(., "key", "value", na.rm = TRUE) %>%
                   split(x = .$value, f = .$key))
    names(attributedescriptions) <- names

    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/surveys/rows/row/attributedescriptions"), jsonlite::toJSON(attributedescriptions, auto_unbox = TRUE))

  }

  # Language properties

  if (length(languages) >= 2) {

    # The group template is copied for the secondaries languages
    for (num in utils::tail(seq_along(languages), -1)) {
      xml2::xml_add_child(xml2::xml_find_all(xml, "/document/surveys_languagesettings/rows"), xml2::xml_find_first(xml, "/document/surveys_languagesettings/rows/row"))
    }

  }

  language_properties <- properties %>%
    dplyr::semi_join(limer::survey_properties %>%
                       dplyr::filter(stringr::str_detect(key, "^(surveyls|email)_")),
                     by = "key") %>%
    split(f = .$language) %>%
    purrr::map(~ dplyr::select(., -language) %>%
                split(x = .$value, f = .$key))

  for (num_language in seq_along(language_properties)) {

    xml2::xml_set_text(
      xml2::xml_find_all(xml, "/document/surveys_languagesettings/rows/row/surveyls_language")[[num_language]],
      names(language_properties)[[num_language]]
    )

    for (num_property in seq_along(language_properties[[num_language]])) {

      xml2::xml_set_text(
        xml2::xml_find_all(xml, paste0("/document/surveys_languagesettings/rows/row/", names(language_properties[[num_language]])[[num_property]]))[[num_language]],
        language_properties[[num_language]][[num_property]]
      )

    }

  }

  tempfile <- tempfile(fileext = ".lss")
  xml2::write_xml(xml, tempfile)

  survey_id <- limer::call_limer(
    "import_survey",
    params = list(
      "sImportData" = readLines(tempfile, encoding = "UTF-8") %>%
        paste0(collapse = "") %>%
        limer::str_to_base64(),
      "sImportDataType" = "lss"
    ))

  if (class(survey_id) == "list") {
    stop(survey_id$status, call. = FALSE)
  }

  if ("expires" %in% properties$key) {
    expires <- properties %>%
      dplyr::filter(key == "expires") %>%
      dplyr::pull(value)
    limer::call_limer("set_survey_properties", list("iSurveyID" = survey_id, "aSurveyData" = list("expires" = expires)))
  }

  if (quiet == FALSE){
    usethis::ui_done("Survey {survey_id}")
  }

  return(survey_id)
}