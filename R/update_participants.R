#' update_participants
#'
#' @param id_survey \dots
#' @param tbl_update \dots
#' @param message \dots
#'
#' @export
update_participants <- function(id_survey, tbl_update, message = TRUE) {

  key <- limer::get_session_key()

  attributes_descriptions <- limer::get_attributes_descriptions(id_survey)

  if (!"token" %in% names(tbl_update)) {
    stop("colmun \"token\" must be in tbl_update.", call. = FALSE)
  }

  names(tbl_update)[which(names(tbl_update) %in% attributes_descriptions)] <- names(attributes_descriptions)[unlist(sapply(names(tbl_update), function(x) which(attributes_descriptions == x)))]

  diff <- limer::get_participants(id_survey, aAttributes = as.list(names(attributes_descriptions))) %>%
    tidyr::gather("column", "value_ls", -tid, -token) %>%
    dplyr::full_join(tidyr::gather(tbl_update, "column", "value_update", -token),
                     by = c("token", "column")) %>%
    dplyr::filter(purrr::map2_lgl(value_ls, value_update, ~ !.x %in% .y | !.y %in% .x))

  if (nrow(diff) == 0) {

    if (message == TRUE) message("No updates")

  } else {

    if (message == TRUE) message(nrow(diff), " updates...")

    update <- diff %>%
      split(1:nrow(.)) %>%
      pbapply::pblapply(function(row) {

        Sys.sleep(1)

        set_participant_properties <- limer::call_limer("set_participant_properties",
                                                        params = list("iSurveyID" = row$id_survey,
                                                                      "aTokenQueryProperties" = row$tid,
                                                                      "aTokenData" = as.list(stats::setNames(row$value_update, row$column))))

      })

  }

  release <- limer::release_session_key()

}