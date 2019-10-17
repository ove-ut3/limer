#### Survey properties ####

survey_properties <- read.csv2("data-raw/survey_properties.csv", na = "") %>%
  dplyr::as_tibble()

usethis::use_data(survey_properties, overwrite = TRUE)

#### Question types ####

question_types <- read.csv2("data-raw/question_types.csv", na = "") %>%
  dplyr::as_tibble()

usethis::use_data(question_types, overwrite = TRUE)

#### Question properties ####

documentation <- xml2::read_html("https://github.com/LimeSurvey/LimeSurvey/blob/master/application/helpers/questionHelper.php") %>%
  xml2::xml_text() %>%
  strsplit("\n") %>%
  dplyr::tibble(html = .) %>%
  tidyr::unnest_legacy() %>%
  dplyr::mutate_at("html", trimws) %>%
  dplyr::filter(nchar(html) >= 1) %>%
  dplyr::mutate(attribute = stringr::str_match(html, "self::\\$attributes\\[[\"'](.+?)[\"']\\]")[, 2]) %>%
  dplyr::mutate(line = dplyr::row_number())

attributes_lines <- documentation %>%
  tidyr::drop_na(attribute) %>%
  dplyr::mutate(line_end = dplyr::lead(line, 1) - 1)

last_attribute_line <- documentation %>%
  dplyr::filter(html == ");",
                line > tail(attributes_lines$line, 1)) %>%
  head(1) %>%
  dplyr::pull(line)

attributes_lines <- attributes_lines %>%
  dplyr::mutate(line_end = tidyr::replace_na(line_end, last_attribute_line),
                lines = purrr::map2(line, line_end, ~ .x:.y))

question_attributes <- documentation %>%
  dplyr::select(-attribute) %>%
  dplyr::left_join(
    attributes_lines %>%
      tidyr::unnest_legacy(lines) %>%
      dplyr::select(line = lines, attribute),
    by = "line") %>%
  tidyr::drop_na(attribute) %>%
  dplyr::mutate(key = stringr::str_match(html, "(.+?)=>")[, 2],
                value = stringr::str_match(html, "=>(.+?),?$")[, 2]) %>%
  dplyr::mutate_at("key", trimws) %>%
  dplyr::mutate_at("key", stringr::str_remove, "^['\"]") %>%
  dplyr::mutate_at("key", stringr::str_remove, "['\"]$") %>%
  tidyr::drop_na(key, value) %>%
  dplyr::mutate(bad_extract = !key %in% c("types", "category", "sortorder", "inputtype", "expression", "options", "default", "caption", "help"),
                value = dplyr::if_else(bad_extract, html, value),
                key = dplyr::if_else(bad_extract, "options", key)) %>%
  dplyr::group_by(attribute, key) %>%
  dplyr::summarise_at("value", paste0, collapse = " ") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(value = dplyr::if_else(key == "options", paste0(value, ")"), value),
                value = stringr::str_remove(value, ",\\)$"),
                value = stringr::str_remove(value, "\\);$")) %>%
  dplyr::mutate_at("value", trimws) %>%
  tidyr::spread(key, value) %>%
  dplyr::mutate_at(c("caption", "category", "help"), stringr::str_remove_all, "gT\\(['\"]") %>%
  dplyr::mutate_at(c("caption", "category", "help"), stringr::str_remove_all, "['\"]\\)") %>%
  dplyr::bind_rows(read.csv2("data-raw/question_properties.csv")) %>%
  dplyr::arrange(attribute)

usethis::use_data(question_attributes, overwrite = TRUE)
