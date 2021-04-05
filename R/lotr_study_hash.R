#' Hash studies with LOTR characters
#'
#' Replaces study identifier column with random combination of lord of the rings
#' character and a year, from [lotr_study] dataset found on kaggle.
#'
#' Currently 373 names, so only coded for < 373 studies atm. In future, will
#' need to filter duplicates if want more.
#'
#' @param dat A data frame with `study_col` column.
#' @param study_col Column that uniquely identifies the study.
#' @param sep Separator between name and year, defaults to "_".
#'
#' @export


lotr_study_hash <- function(dat, study_col, sep = "_") {
  # unique studies
  studies <-
    dat %>%
    dplyr::pull({{study_col}}) %>%
    unique()

  #create key table from lotr characters
  key <-
    lotr_study %>%
    head(length(studies)) %>%
    tidyr::unite("fake_id", author, year, sep = sep) %>%
    dplyr::mutate({{study_col}} := studies) %>%
    dplyr::select({{study_col}}, fake_id)

  # join fake id to existing dataset
  dat %>%
    dplyr::left_join(key) %>%
    dplyr::select(-{{study_col}}) %>%
    dplyr::rename(
      {{study_col}} := fake_id
    )
}
