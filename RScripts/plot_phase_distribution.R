# Title: Plot Conversation Phase Distribution
# Description: This script reads the conversation phase analysis results
#              and generates a bar plot showing the distribution of students
#              across the different phases.

# --- 1. Setup: Install and load necessary packages ---

# List of required packages
required_packages <- c("readr", "ggplot2", "dplyr")

# Check if packages are installed and install them if not
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# --- 2. Configuration ---

# Input file containing the analysis results
input_file <- "conversation_phase_analysis.csv"
# Output file for the plot
output_file <- "phase_distribution.png"

# --- 3. Data Loading and Processing ---

# Check if the input file exists
if (!file.exists(input_file)) {
  stop(paste("Error: Input file not found at", input_file))
}

# Read the CSV data
analysis_results <- readr::read_csv(input_file, col_types = cols())

# Ensure 'phase' is treated as a categorical factor for plotting
analysis_results <- analysis_results %>%
  mutate(phase = as.character(phase)) %>%
  mutate(phase = factor(phase, levels = c("1", "2", "3", "4")))

# Count the number of students in each phase
phase_counts <- analysis_results %>%
  count(phase, .drop = FALSE) %>%
  rename(count = n)

cat("--- Phase Counts ---\n")
print(phase_counts)
cat("--------------------\n")

# --- 4. Plotting ---

# Create the bar plot
phase_plot <- ggplot(phase_counts, aes(x = phase, y = count, fill = phase)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = count), vjust = -0.5, size = 4) + # Add count labels on top of bars
  labs(
    title = "Which stage did the students reach in the conversation?",
    x = "Chatbot Phase",
    y = "Number of Students"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# Save the plot to a file
ggsave(output_file, plot = phase_plot, width = 8, height = 6, dpi = 300)

cat(paste("\nPlot saved successfully to", output_file, "\n"))
