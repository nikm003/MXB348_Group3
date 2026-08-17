# Install packages needed
install.packages(c("readr", "dplyr", "writexl"))

# Load packages
library(dplyr)
library(writexl)
library(readr)

# Read files

results <- read_csv("results-3.csv")

journey <- read_csv("customer_journey-4.csv")


# Combine and format data

journey_formatted <- journey %>%
  group_by(user_id, session_id) %>%
  summarise(
    
    # Put channels into one string
    channels = paste(channel, collapse = " -> "),
    
    # Put times into one string
    times = paste(time_till_session_end, collapse = " -> "),
    
    .groups = "drop"
  )


# Add conversion from results

final_data <- journey_formatted %>%
  left_join(
    results %>%
      select(user_id, session_id, conversion),
    by = c("user_id", "session_id")
  ) %>%
  arrange(user_id, session_id)


# Save the data

write_xlsx(
  final_data,
  "formatted_data.xlsx"
)