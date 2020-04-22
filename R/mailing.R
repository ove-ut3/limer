#' Mailing
#'
#' @param from Expéditeur list("email", "alias")
#' @param to Tibble des destinataires
#' @param subject Sujet du mail
#' @param body Corps du mail
#' @param session Open a limesurvey session
#'
#' @export
mailing_create_survey <- function(from, to, subject, body, session = FALSE) {

  if (nrow(to) == 0) {
    message("0 participants in to tibble.")
    return()
  }

  attributes <- stringr::str_subset(names(to), stringr::regex("^attribute_\\d+$", ignore_case = TRUE))

  if (length(attributes) >= 1) {
    attributedescriptions <- paste0('"', attributes, '":{"description":"","mandatory":"N","show_register":"N","cpdbmap":""}', collapse = ",")
    attributedescriptions <- paste0("{", attributedescriptions, "}")

  } else {
    attributedescriptions <- "{}"
  }

  body <- gsub("\n", "<br>", body)

  lss <- readLines(system.file("extdata/mailing.lss", package = "limer"), encoding = "UTF-8")
  lss <- sub("##subject##", subject, lss)
  lss <- sub("##body##", body, lss)
  lss <- sub("##from_alias##", from$alias, lss)
  lss <- sub("##from_email##", from$email, lss)
  lss <- gsub("##attributedescriptions##", attributedescriptions, lss)

  lss <- limer::str_to_base64(lss)

  if (session) {
    key <- limer::get_session_key()
  }

  survey_id <- limer::call_limer(method = "import_survey", params = list("sImportData" = lss, "sImportDataType" = "lss"))

  activation <- limer::call_limer("activate_tokens", params = list("iSurveyID" = survey_id, "aAttributeFields" = list()))

  participants <- limer::call_limer("add_participants", list("iSurveyID" = survey_id, "aParticipantData" = to))

  if (session) {
    release <- limer::release_session_key()
  }

  dplyr::tibble(
    survey_id = survey_id,
    tid = participants$tid
  )

}

#' Perform mailing
#'
#' @param survey_id Id de l'enquête créée
#' @param tid Participants tid
#' @param sleep Temporisation entre chaque envoi de mail
#' @param delete Delete mailing survey after mailing is finished
#' @param progress_bar Display a progres bar
#' @param session Open a limesurvey session
#'
#' @export
mailing <- function(survey_id, tid, sleep = 7, delete = FALSE, progress_bar = TRUE, session = FALSE) {

  if (session) {
    key <- limer::get_session_key()
  }

  if (progress_bar == TRUE) {
    fn_apply <- pbapply::pblapply
  } else {
    fn_apply <- lapply
  }

  mailing <- fn_apply(tid, function(tid) {

    Sys.sleep(sleep)

    mailing <- limer::mail_registered_participant(survey_id, tid = tid)

    if (length(mailing$status) == 0) {

      print("reprise au participant tid ", tid)
      key <- limer::get_session_key()
      mailing <- limer::mail_registered_participant(survey_id, tid = tid)

    }

  })

  if (delete == TRUE) {
    suppression <- limer::call_limer("delete_survey", params = list("iSurveyID" = survey_id))
  }

  if (session) {
    release <- limer::release_session_key()
  }

  return(mailing)
}
