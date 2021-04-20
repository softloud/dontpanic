#' Hash studies with LOTR characters
#'
#' Replaces study identifier column with random combination of lord of the rings
#' character and a year, from [lotr_study] dataset found on kaggle.
#'
#' Currently 373 names, so only coded for < 373 studies atm. In future, will
#' need to filter duplicates if want more.
#'
#' @param dat A data frame with `study_col` column.
#' @param study_col Column indicating which study the observations recorded in
#' that row came from. This will replace them with star wars characters.
#' @param trt_col Column indicating which treatments. Replaced with
#' lotr characters.
#' @param sep Separator between name and year, defaults to "_".
#' @param max_obs Gotta put a number into the year sampler.
#'
#' @export


lotr_study_hash <-
  function(dat,
           study_col,
           trt_col,
           sep = "_",
           max_obs = 500) {
    # unique studies
    studies <-
      dat %>%
      dplyr::pull({
        {
          study_col
        }
      }) %>%
      unique()

    # unique treatments
    treatments <-
      dat %>%
      dplyr::pull({
        {
          trt_col
        }
      }) %>%
      unique()

    # create key table from lotr characters for treatments
    key_trt <-
      tibble::tibble(trt = treatments,
                     lotr = lotr_study %>%
                       head(length(treatments)) %>%
                       pull(author)) %>%
      dplyr::mutate(lotr = dplyr::if_else(trt == "placebo", trt, lotr)) %>%
      dplyr::rename({
        {
          trt_col
        }
      } := trt)

    # and hashes for studies
    key_studies <-
      tibble::tibble(
        study = studies,
        sw = dplyr::starwars %>%
          head(length(studies)) %>%
          pull(name) %>%
          str_extract("[\\w|-]+") %>%
          sample(size = length(studies), replace = TRUE),
        year = sample(
          x = seq(1800, 1800 + max_obs),
          size = length(studies),
          replace = FALSE
        )
      ) %>%
      unite(col = "sw", sw, year, sep = sep) %>%
      rename({{study_col}} := study)

    # join fake id to existing dataset
    dat %>%
      dplyr::left_join(key_trt) %>%
      dplyr::select(-{
        {
          trt_col
        }
      }) %>%
      dplyr::rename({
        {
          trt_col
        }
      } := lotr) %>%
      dplyr::left_join(key_studies) %>%
      dplyr::select(-{{study_col}}) %>%
      dplyr::rename({{study_col}} := sw) %>%
      dplyr::select({{study_col}}, {{trt_col}}, dplyr::everything())

  }
