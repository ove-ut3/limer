#' Add a question group to a survey
#'
#' This function adds a question group to an existing survey using a LSG template file.\cr
#' Compared to add_group API call, it can add a multilingual question group.
#'
#' @param survey_id Survey id
#' @param properties group properties\cr
#' A data frame is required with at least a column named \code{group_name} (for group name).\cr
#' Optional columns ca also be added : \code{language} (for group language), \code{description} (for group description) and \code{grelevance} (for group relevance).
#' @param quiet If \code{TRUE} then there is no output during group creation
#'
#' @return The created group id.
#'
#' @examples \dontrun{
#'
#' limer::add_group(
#'   survey_id = 123456,
#'   properties = data.frame(
#'     group_name = c("Group 1", "Group 2"),
#'     description = c("Description 1", NA)
#'   )
#' )
#' }
#'
#' @export
add_group <- function(survey_id, properties, quiet = FALSE) {

  properties <- dplyr::mutate_if(properties, is.character, tidyr::replace_na, "")

  xml <- xml2::read_xml(system.file(paste0("extdata/group.lsg"), package = "limer"))

  if (length(properties$language) >= 2) {

    # The group template is copied for the secondaries languages
    for (num in utils::tail(seq_along(properties$language), -1)) {
      xml2::xml_add_child(xml2::xml_find_all(xml, "/document/groups/rows"), xml2::xml_find_first(xml, "/document/groups/rows/row"))
    }

  }

  for (num in seq_along(properties$group_name)) {

    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/groups/rows/row/group_name")[num], properties$group_name[num])

    xml2::xml_add_child(xml2::xml_find_all(xml, "/document/languages"), "language", properties$language[num])
    xml2::xml_set_text(xml2::xml_find_all(xml, "/document/groups/rows/row/language")[num], properties$language[num])

    if (!is.null(properties[["description"]])) {
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/groups/rows/row/description")[num], properties$description[num])
    }

    if (!is.null(properties[["grelevance"]])) {
      xml2::xml_set_text(xml2::xml_find_all(xml, "/document/groups/rows/row/grelevance")[num], properties$grelevance[num])
    }
  }

  tempfile <- tempfile(fileext = ".lsg")
  xml2::write_xml(xml, tempfile)

  group <- limer::call_limer(method = "import_group",
                             params = list(
                               "iSurveyID" = survey_id,
                               "sImportData" = readLines(tempfile, encoding = "UTF-8") %>%
                                 paste0(collapse = "") %>%
                                 limer::str_to_base64(),
                               "sImportDataType" = "lsg"
                             ))

  if (class(group) == "list") {
    stop(group$status, call. = FALSE)
  }

  language <- limer::call_limer("get_survey_properties", params = list("iSurveyId" = survey_id, "aSurveySettings" = list("language")))

  group_name <- properties %>%
    dplyr::filter(language == !!language) %>%
    dplyr::pull(group_name)

  if (quiet == FALSE){
    usethis::ui_done("Group {group_name}")
  }

  return(group)
}
