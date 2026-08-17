
##load cleaned data
source("clean_attribution_data.R")
journey_full <- clean_attribution_data("customer_journey-4.csv", "results-3.csv")

library(ggplot2)
library(dplyr)
##eda function

explore_attribution_data <- function(journey_full) {
  n_touchpoints <- nrow(journey_full)
  n_sessions <- journey_full %>% distinct(user_id, session_id) %>% nrow()
  n_users <- n_distinct(journey_full$user_id)
  
  conversion_rate <- journey_full %>%
    distinct(user_id, session_id, conversion) %>%
    summarise(rate = mean(conversion)) %>%
    pull(rate)
  
  cat("Touchpoints:", n_touchpoints, "\n")
  cat("Sessions:", n_sessions, "\n")
  cat("Users:", n_users, "\n")
  cat("Conversion rate:", round(conversion_rate, 4), "\n")
  
  channel_volume <- journey_full %>%
    count(channel, name = "touchpoints") %>%
    arrange(desc(touchpoints))
  
  print(channel_volume)
  
  p_channel_volume <- ggplot(channel_volume, aes(x = reorder(channel, touchpoints), y = touchpoints)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Touchpoints by channel", x = NULL, y = "Touchpoints")
  
  print(p_channel_volume)
  
  path_length <- journey_full %>%
    count(user_id, session_id, name = "n_touches") %>%
    mutate(n_touches_bucket = if_else(n_touches >= 8, "8+", as.character(n_touches))) %>%
    count(n_touches_bucket, name = "sessions") %>%
    mutate(n_touches_bucket = factor(n_touches_bucket, levels = c(as.character(1:7), "8+")))
  
  print(path_length)
  
  p_path_length <- ggplot(path_length, aes(x = n_touches_bucket, y = sessions)) +
    geom_col(fill = "steelblue") +
    labs(title = "Touches per session (path length)", x = "Touches", y = "Sessions")
  
  print(p_path_length)
  
  first_touch <- journey_full %>%
    group_by(user_id, session_id) %>%
    filter(touch_order == min(touch_order)) %>%
    ungroup() %>%
    group_by(channel) %>%
    summarise(conversion_rate = mean(conversion), .groups = "drop") %>%
    mutate(position = "First touch")
  
  last_touch <- journey_full %>%
    group_by(user_id, session_id) %>%
    filter(touch_order == max(touch_order)) %>%
    ungroup() %>%
    group_by(channel) %>%
    summarise(conversion_rate = mean(conversion), .groups = "drop") %>%
    mutate(position = "Last touch")
  
  touch_position_conv <- bind_rows(first_touch, last_touch)
  
  print(touch_position_conv)
  
  p_conv_by_position <- ggplot(touch_position_conv, aes(x = channel, y = conversion_rate, fill = position)) +
    geom_col(position = "dodge") +
    coord_cartesian(ylim = c(0.55, 0.60)) +
    labs(title = "Conversion rate by channel: first touch vs last touch", x = NULL, y = "Conversion rate", fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_conv_by_position)
}

##
explore_attribution_data(journey_full)
##