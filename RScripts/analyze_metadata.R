#!/usr/bin/env Rscript

# analysis/analyze_metadata.R

# Description:
# This script analyzes chatbot conversation data to extract metadata.
# It calculates the number of messages and the total conversation duration for each session.
# Finally, it generates a histogram of the message counts and saves the plot and
# the summary data to the 'analysis_results' directory.

# Ensure the required packages are installed, and if not, install them
if (!require("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!require("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load the required libraries
library(dplyr)
library(lubridate)
library(ggplot2)


# Set working directory
setwd("/Users/kaiyu/Library/CloudStorage/OneDrive-UMCG/Onderwijs/AI stuff/Research/Chatbots application/Pilot BSc Medicine 1/Pilot_2526/Onderzoek/analysis")

# --- Configuration ---
INPUT_CSV_PATH <- "/Users/kaiyu/Library/CloudStorage/OneDrive-UMCG/Onderwijs/AI stuff/Research/Chatbots application/Pilot BSc Medicine 1/Pilot_2526/Onderzoek/analysis/chat-export-Geneeskunde B1-Chatbot Embryologie-2025-09-17-to-2025-09-18.csv"
OUTPUT_DIR <- "analysis_results"
OUTPUT_PLOT_MESSAGES_PATH <- file.path(OUTPUT_DIR, "session_message_count_distribution.png")
OUTPUT_PLOT_DURATION_PATH <- file.path(OUTPUT_DIR, "session_duration_distribution.png")
OUTPUT_CSV_PATH <- file.path(OUTPUT_DIR, "session_conversation_summary.csv")

# --- Main Analysis ---

# Create the output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
}

# Read the data
# The file is encoded in 'latin1', so we specify that
message_data <- read.csv(INPUT_CSV_PATH, fileEncoding = "latin1")

# Clean column names by removing leading/trailing whitespace
colnames(message_data) <- trimws(colnames(message_data))

# Convert 'created_at' to datetime objects
# The format includes microseconds, which we can handle with '%Y-%m-%dT%H:%M:%OS'
message_data$created_at <- ymd_hms(message_data$created_at)

# Remove rows with missing user_id, session_id or created_at
message_data <- message_data %>%
  filter(!is.na(user_id) & user_id != "" & !is.na(session_id) & session_id != "" & !is.na(created_at))

# Group by user and session, then calculate metadata
session_summary <- message_data %>%
  group_by(user_id, session_id) %>%
  summarise(
    user_email = first(user_email),
    message_count = n(),
    first_message_time = min(created_at, na.rm = TRUE),
    last_message_time = max(created_at, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    conversation_duration_secs = as.numeric(last_message_time - first_message_time),
    conversation_duration_mins = conversation_duration_secs / 60
  )

# --- Plotting ---

# Create a histogram of the message counts per session
message_distribution_plot <- ggplot(session_summary, aes(x = message_count)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Messages per Session",
    x = "Number of Messages",
    y = "Number of Sessions"
  ) +
  theme_minimal()

# Create a histogram of the conversation duration in minutes, filtering for 0-60 mins
duration_distribution_plot <- ggplot(session_summary %>% filter(conversation_duration_mins >= 0 & conversation_duration_mins <= 60), aes(x = conversation_duration_mins)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Conversation Duration per Session (0-60 Minutes)",
    x = "Duration (minutes)",
    y = "Number of Sessions"
  ) +
  theme_minimal()

# --- Save Results ---

# Save the plots
ggsave(OUTPUT_PLOT_MESSAGES_PATH, plot = message_distribution_plot, width = 10, height = 6)
ggsave(OUTPUT_PLOT_DURATION_PATH, plot = duration_distribution_plot, width = 10, height = 6)

# Save the summary data to a CSV file
write.csv(session_summary, OUTPUT_CSV_PATH, row.names = FALSE)

# --- Output ---
print("Metadata analysis complete.")
print(paste("Message count plot saved to:", OUTPUT_PLOT_MESSAGES_PATH))
print(paste("Duration distribution plot saved to:", OUTPUT_PLOT_DURATION_PATH))
print(paste("Summary data saved to:", OUTPUT_CSV_PATH))
print("Summary of user sessions:")
print(head(session_summary))
