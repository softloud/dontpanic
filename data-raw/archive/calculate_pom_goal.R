#' calculate number of poms
#'
#' @param travel_hrs number of hours of teaching
#' @param community_hrs number of hours with other people
#' Returns goal number of poms the floor of $14 - t - 1.5 c$ where $t$ denotes the number of hours of travel, and $c$ the number of hours of _community_, where the lattter includes teaching, outreach, and research collaborations. That, is I aim to draw a distinction between self-directed time, and time where I have the benefit of external motivations.

calculate_pom_goal <-
  function(travel_hrs = 0,
           community_hrs = 0,
           min_phi_hrs = 0,
           min_theta_hrs = 0,
           min_psi_hrs = 0) {
    goal <- floor(14 - travel_hrs - 1.5 * community_hrs)

    wk <- workload_key %>%
      dplyr::select(workload, phi, theta, psi) %>%
      dplyr::mutate(total = phi + theta + psi,
             aim_for = total <= goal) %>%
      dplyr::filter(
        phi > min_phi_hrs,
        theta > min_theta_hrs,
        psi > min_psi_hrs,
        aim_for == TRUE
      ) %>%
      select(-aim_for) %>%
      tidyr::gather(key = "category", value = "poms", phi, theta, psi) %>%
      rename(usual = total) %>%
      mutate(pom_diff = case_when(
        category == "phi" ~ poms - min_phi_hrs * 3,
        category == "theta" ~ poms - min_theta_hrs * 3,
        category == "psi" ~ poms - min_psi_hrs * 3
      )) %>%
      filter(pom_diff > 0)


    cat(paste0("\naim to get ", goal, " poms done today\n"))
    wk

  }
