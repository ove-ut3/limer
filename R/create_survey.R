#' create_survey
#'
#' @param lss_file \dots
#' @param tbl_participants \dots
#' @param survey_id \dots
#' @param survey_title \dots
#' @param attributes_description \dots
#'
#' @export
create_survey <- function(lss_file, tbl_participants, survey_id = NULL, survey_title = NULL, attributes_description = NULL) {

  lss <- readr::read_lines(lss_file)

  if (!is.null(attributes_description)) {

    # Ajout des attributs
    attributes <- stringr::str_subset(names(tbl_participants), stringr::regex("^attribute_\\d+$", ignore_case = TRUE))

    if (length(attributes) != length(attributes_description)) {
      stop(length(attributes), " attributs de tbl_participants et ", length(attributes_description), " attributs dans le paramètre attributes_description", call. = FALSE)
    }

    if (length(attributes) >= 1) {
      attributedescriptions <- paste0('"', attributes, '":{"description":"', attributes_description,'","mandatory":"N","show_register":"N","cpdbmap":""}', collapse = ",") %>%
        paste0("{", ., "}")
    } else {
      attributedescriptions <- "{}"
    }

    lss <- stringr::str_replace(lss, "<attributedescriptions><!\\[CDATA\\[.*?\\]\\]><\\/attributedescriptions>",
                                paste0("<attributedescriptions><![CDATA[", attributedescriptions, "]]></attributedescriptions>"))

  }

  key <- limer::get_session_key()

  survey_id <- limer::call_limer(method = "import_survey",
                                    params = list("sImportData" = limer::str_to_base64(lss),
                                                  "sImportDataType" = "lss",
                                                  "NewSurveyName" = survey_title,
                                                  "DestSurveyID" = survey_id))

  activation <- limer::call_limer("activate_tokens", params = list("iSurveyID" = survey_id, "aAttributeFields" = list()))

  ajout <- limer::call_limer("add_participants", params = list("iSurveyID" = survey_id, "aParticipantData" = tbl_participants, "bCreateToken" = FALSE))

  activation <- limer::call_limer("activate_survey", list("iSurveyID" = survey_id))
  limer::call_limer("set_survey_properties", list("iSurveyID" = survey_id, "aSurveyData" = list("expires" = "")))

  release <- limer::release_session_key()

}