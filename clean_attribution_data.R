library(dplyr)

clean_attribution_data <- function(journey_path, results_path) {
  journey <- read.csv(journey_path)
  results <- read.csv(results_path)
  
  journey <- journey %>%
    group_by(user_id, session_id) %>%
    arrange(desc(time_till_session_end), .by_group = TRUE) %>%
    mutate(touch_order = row_number() - 1) %>%
    ungroup()
  
  journey_full <- journey %>%
    select(user_id, session_id, touch_order, channel, time_till_session_end) %>%
    left_join(results, by = c("user_id", "session_id")) %>%
    arrange(user_id, session_id, touch_order)
  
  journey_full

  
}

