get_responses_ <- function(iSurveyID, sDocumentType = "csv", sLanguageCode = NULL,
                           sCompletionStatus = "complete", sHeadingType = "code",
                           sResponseType = "long", sep=";", ...) {
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())
  dots <- list(...)
  if(length(dots) > 0) params <- append(params,dots)
  # print(params) # uncomment to debug the params

  results <- call_limer(method = "export_responses", params = params)
  return(base64_to_df(unlist(results), sep=sep))
}

get_participants_ <- function(iSurveyID, iStart = 0, iLimit = NULL, bUnused = FALSE, aAttributes = list(), aConditions = list()){

  if (is.null(iLimit)) {
    iLimit <- limer::call_limer("get_summary", list("iSurveyID" = iSurveyID, "sStatName" = "token_count"))
    iLimit <- as.integer(iLimit)
  }

  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())

  results <- limer::call_limer(method = "list_participants", params = params)

  if (any(class(results) == "data.frame")) {
    results <- jsonlite::flatten(results)
    names(results) <- sub("^participant_info\\.(firstname|lastname|email)$", "\\1", names(results))
    results[results == ""]  <- NA_character_
  } else {
    if (results$status == "No survey participants found.") {
      results <- data.frame(
        tid = character(0),
        token = character(0),
        firstname = character(0),
        lastname = character(0),
        email = character(0),
        stringsAsFactors = FALSE
      )
    }
  }

  return(results)
}
