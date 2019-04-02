#' calculate number of poms
#'
#' @param travel_hrs number of hours of teaching
#' @param community_hrs number of hours with other people
#' Returns goal number of poms the floor of $14 - t - 1.5 c$ where $t$ denotes the number of hours of travel, and $c$ the number of hours of _community_, where the lattter includes teaching, outreach, and research collaborations. That, is I aim to draw a distinction between self-directed time, and time where I have the benefit of external motivations.
#' @export

calculate_pom_goal <-
  function(travel_hrs = 0,
           community_hrs = 0) {
    goal <- floor(14 - travel_hrs - 1.5 * community_hrs)
    cat(paste0("\naim to get ", goal, " poms done today\n"))
  }
