intensities <- c(
  "nothing",
  "sweet fuck all",
  "eh, got some stuff done",
  "pretty good effort",
  "eaglescream"
)

#' wrangle measured. data
#'
#' this is currently hardcoded in document
#'
#' @param df [measured_data()] output
#'
#' @export

wrangle_measures <- function(timetracker) {
  goals <- workload_key %>%
    select(workload, phi_hrs, theta_hrs, psi_hrs) %>%
    gather(key = "category", value = "poms", phi_hrs, theta_hrs, psi_hrs) %>%
    spread(key = "workload", value = "poms") %>%
    mutate(category = str_sub(category, end = -5))

  daily_work <- timetracker %>%
    mutate(duration_hrs = if_else(is.na(duration_hrs), 0, duration_hrs)) %>%
    dplyr::filter(category == "phi" |
                    category == "theta" | category == "psi") %>%
    select(-note, -description) %>%
    group_by(date, category) %>%
    summarise(hrs = sum(duration_hrs)) %>%
    left_join(goals) %>%
    mutate(
      category_workload = case_when(
        hrs == 0 ~ intensities[[1]],
        hrs < light ~ intensities[[2]],
        hrs < moderate ~ intensities[[3]],
        hrs < hardcore ~ intensities[[4]],
        hrs > hardcore ~ intensities[[5]]
      )
    ) %>%
    group_by(date) %>%
    mutate(
      daily_workload = case_when(
        any(hrs == 0) ~ intensities[[1]],
        any(hrs < light) ~ intensities[[2]],
        any(hrs < moderate) ~ intensities[[3]],
        any(hrs < hardcore) ~ intensities[[4]],
        all(hrs > hardcore) ~ intensities[[5]]
      )
    )

  quinque <- daily_work %>%
    count_workload(days = 5) %>%
    mutate(period = "v")

  decade <-  daily_work %>%
    count_workload(days = 10) %>%
    mutate(period = "x")

  century <- daily_work %>%
    count_workload(days = 100) %>%
    mutate(period = "c")

  daily_measures <- daily_work %>%
    count_workload() %>%
    mutate(period = "m") %>%
    bind_rows(quinque, decade, century) %>%
    ungroup() %>%
    mutate(
      category = fct_relevel(category, "phi", "theta", "psi"),
      period = fct_relevel(period, "v", "x", "c", "m")
    ) %>%
    full_join(daily_work) %>%
    mutate(
      daily_workload = fct_relevel(daily_workload, intensities),
      category_workload = fct_relevel(category_workload, intensities)
    )

  return(daily_measures = daily_measures)
}
