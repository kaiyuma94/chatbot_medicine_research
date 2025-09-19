# Chatbot Survey Analysis - Full Version
# =======================================

# Load required libraries
library(tidyverse)
library(psych)        # For reliability analysis
library(corrplot)     # For correlation plots
library(VIM)          # For missing data visualization
library(effsize)      # For effect sizes
library(car)          # For ANOVA

# 1. DATA IMPORT AND SETUP
# ========================

# Set working directory
setwd("/Users/kaiyu/Library/CloudStorage/OneDrive-UMCG/Onderwijs/AI stuff/Research/Chatbots application/Pilot BSc Medicine 1/Pilot_2526/Onderzoek/analysis")

# Import data
df <- read.csv("data/chatbot_survey_data.csv", stringsAsFactors = FALSE)


# Display basic info about the dataset
cat("Dataset dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
head(df)
str(df)

# 2. MISSING DATA ANALYSIS
# ========================

# Calculate missingness by variable
missing_summary <- df %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = (missing_count / nrow(df)) * 100) %>%
  arrange(desc(missing_percent))

cat("\nMissing Data Summary:\n")
print(missing_summary)

# Identify cases with complete demographic + scale data
demographic_vars <- c("age", "gender", "chatbot_frequency")
scale_vars <- c("g1_construct_answer", "g2_understand_topic", "g3_selfexplain", 
                "g4_trust", "g5_prompt_selfexplain", "g6_mechanisms_case", 
                "g7_recommend", "g8_boring", "g9_person",
                paste0("arcs", 1:12), "mental_effort")

# Check completeness
df$demo_complete <- complete.cases(df[demographic_vars])
df$scales_complete <- complete.cases(df[scale_vars])
df$fully_complete <- df$demo_complete & df$scales_complete

# Summary of completeness
completeness_summary <- df %>%
  summarise(
    total_cases = n(),
    demo_complete = sum(demo_complete, na.rm = TRUE),
    scales_complete = sum(scales_complete, na.rm = TRUE),
    fully_complete = sum(fully_complete, na.rm = TRUE)
  )

cat("\nCompleteness Summary:\n")
print(completeness_summary)

# 3. SCALE DEFINITION AND REVERSE CODING
# ======================================

# Chatbot evaluation items (g1-g9)
chatbot_eval_items <- c("g1_construct_answer", "g2_understand_topic", "g3_selfexplain", 
                        "g4_trust", "g5_prompt_selfexplain", "g6_mechanisms_case", 
                        "g7_recommend", "g8_boring", "g9_person")

# ARCS scale items (arcs1-arcs12) 
arcs_attention_items <- c("arcs1", "arcs2", "arcs3")      # Questions holding attention
arcs_relevance_items <- c("arcs4", "arcs5", "arcs6")      # Relevance to learning/future
arcs_confidence_items <- c("arcs7", "arcs8", "arcs9")     # Confidence in ability
arcs_satisfaction_items <- c("arcs10", "arcs11", "arcs12") # Satisfaction/enjoyment

# Reverse code negatively worded items
# g8_boring: "Working with chatbot was boring" (lower = better)
# g9_person: "I would prefer to dialogue with a person rather than chatbot" (lower = better chatbot rating)
reverse_items <- c("g8_boring", "g9_person")

cat("\nReverse coding negatively worded items...\n")
for(item in reverse_items) {
  if(item %in% names(df)) {
    df[[paste0(item, "_rev")]] <- 6 - df[[item]]  # For 1-5 scale
    cat("Reverse coded:", item, "-> created", paste0(item, "_rev"), "\n")
  }
}

# Update chatbot evaluation items to use reverse coded versions
chatbot_eval_items_final <- chatbot_eval_items
chatbot_eval_items_final[chatbot_eval_items_final == "g8_boring"] <- "g8_boring_rev"
chatbot_eval_items_final[chatbot_eval_items_final == "g9_person"] <- "g9_person_rev"

# 4. RELIABILITY ANALYSIS (CRONBACH'S ALPHA)
# ==========================================

cat("\n=== RELIABILITY ANALYSIS ===\n")

# Function to calculate reliability with complete cases only
calc_reliability <- function(data, items, scale_name) {
  # Check if items exist
  existing_items <- items[items %in% names(data)]
  if(length(existing_items) == 0) {
    cat(scale_name, ": No matching columns found\n")
    return(NULL)
  }
  
  scale_data <- data[existing_items]
  complete_data <- scale_data[complete.cases(scale_data), ]
  
  if(nrow(complete_data) < 10) {
    cat(scale_name, ": Insufficient complete cases (n =", nrow(complete_data), ")\n")
    return(NULL)
  }
  
  alpha_result <- psych::alpha(complete_data)
  cat("\n", scale_name, " (n =", nrow(complete_data), ", k =", length(existing_items), "):\n")
  cat("Cronbach's Alpha =", round(alpha_result$total$std.alpha, 3), "\n")
  
  # Extract confidence intervals properly
  if(!is.null(alpha_result$feldt)) {
    lower_ci <- alpha_result$feldt$lower.ci
    upper_ci <- alpha_result$feldt$upper.ci
    # Handle case where CI might be a vector
    if(is.list(lower_ci)) lower_ci <- lower_ci[[1]]
    if(is.list(upper_ci)) upper_ci <- upper_ci[[1]]
    cat("95% CI: [", round(lower_ci, 3), ", ", round(upper_ci, 3), "]\n")
  }
  
  # Item-total correlations
  if(alpha_result$total$std.alpha < 0.70) {
    cat("⚠️  Alpha below 0.70 - check item-total correlations:\n")
    print(round(alpha_result$item.stats[c("r.cor", "r.drop")], 3))
  } else if(alpha_result$total$std.alpha >= 0.80) {
    cat("✓ Excellent reliability (α ≥ 0.80)\n")
  } else {
    cat("✓ Acceptable reliability (0.70 ≤ α < 0.80)\n")
  }
  
  return(alpha_result)
}

# Calculate reliability for each scale
alpha_chatbot <- calc_reliability(df, chatbot_eval_items_final, "Chatbot Evaluation Scale")
alpha_attention <- calc_reliability(df, arcs_attention_items, "ARCS - Attention")
alpha_relevance <- calc_reliability(df, arcs_relevance_items, "ARCS - Relevance") 
alpha_confidence <- calc_reliability(df, arcs_confidence_items, "ARCS - Confidence")
alpha_satisfaction <- calc_reliability(df, arcs_satisfaction_items, "ARCS - Satisfaction")

# Overall ARCS scale
all_arcs_items <- c(arcs_attention_items, arcs_relevance_items, arcs_confidence_items, arcs_satisfaction_items)
alpha_arcs_total <- calc_reliability(df, all_arcs_items, "ARCS - Total Scale")

# 5. SCALE CONSTRUCTION
# ====================

cat("\n=== CREATING COMPOSITE SCORES ===\n")

# Create composite scores (mean of items, requiring at least 50% completion)
df$chatbot_eval_score <- psych::scoreItems(keys = rep(1, length(chatbot_eval_items_final)), 
                                           items = df[chatbot_eval_items_final], 
                                           min = ceiling(length(chatbot_eval_items_final) * 0.5))$scores

df$arcs_attention_score <- psych::scoreItems(keys = rep(1, length(arcs_attention_items)), 
                                             items = df[arcs_attention_items], 
                                             min = ceiling(length(arcs_attention_items) * 0.5))$scores

df$arcs_relevance_score <- psych::scoreItems(keys = rep(1, length(arcs_relevance_items)), 
                                             items = df[arcs_relevance_items], 
                                             min = ceiling(length(arcs_relevance_items) * 0.5))$scores

df$arcs_confidence_score <- psych::scoreItems(keys = rep(1, length(arcs_confidence_items)), 
                                              items = df[arcs_confidence_items], 
                                              min = ceiling(length(arcs_confidence_items) * 0.5))$scores

df$arcs_satisfaction_score <- psych::scoreItems(keys = rep(1, length(arcs_satisfaction_items)), 
                                                items = df[arcs_satisfaction_items], 
                                                min = ceiling(length(arcs_satisfaction_items) * 0.5))$scores

df$arcs_total_score <- psych::scoreItems(keys = rep(1, length(all_arcs_items)), 
                                         items = df[all_arcs_items], 
                                         min = ceiling(length(all_arcs_items) * 0.5))$scores

# 6. DESCRIPTIVE STATISTICS
# =========================

cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# Create labeled variables for better output
df$gender_label <- factor(df$gender, levels = c(1, 2), 
                          labels = c("Female", "Male"))

df$frequency_label <- factor(df$chatbot_frequency, 
                             levels = 1:6, 
                             labels = c("Never", "Less than once/month", 
                                        "About once/month", "About once/week", 
                                        "Multiple times/week", "Daily"))

# Demographic frequencies
cat("Age Distribution:\n")
print(table(df$age, useNA = "ifany"))
cat("\nGender Distribution:\n") 
print(table(df$gender_label, useNA = "ifany"))
cat("\nChatbot Usage Frequency:\n")
print(table(df$frequency_label, useNA = "ifany"))

# Scale descriptives
composite_vars <- c("mental_effort", "chatbot_eval_score", "arcs_attention_score", 
                    "arcs_relevance_score", "arcs_confidence_score", 
                    "arcs_satisfaction_score", "arcs_total_score")

descriptives <- df %>%
  select(all_of(composite_vars)) %>%
  psych::describe() %>%
  round(3)

cat("\nScale Descriptives:\n")
print(descriptives[c("n", "mean", "sd", "min", "max", "skew", "kurtosis")])

# Check for ceiling/floor effects (>15% at extremes)
ceiling_floor <- df %>%
  select(all_of(composite_vars)) %>%
  summarise_all(list(
    ceiling_15 = ~mean(. >= (max(., na.rm = TRUE) - 0.15 * 
                               (max(., na.rm = TRUE) - min(., na.rm = TRUE))), na.rm = TRUE),
    floor_15 = ~mean(. <= (min(., na.rm = TRUE) + 0.15 * 
                             (max(., na.rm = TRUE) - min(., na.rm = TRUE))), na.rm = TRUE)
  )) %>%
  round(3)

cat("\nCeiling/Floor Effects (>15% at extremes indicates potential issue):\n")
print(ceiling_floor)

# 7. GROUP COMPARISONS
# ===================

cat("\n=== GROUP COMPARISONS ===\n")

# Create usage groups: High usage = 4+ (weekly or more), Low usage = 1-3
df$usage_high_low <- ifelse(df$chatbot_frequency >= 4, "High", "Low")

cat("Usage Groups:\n")
print(table(df$usage_high_low, useNA = "ifany"))

# T-tests for high vs low usage
outcomes <- composite_vars

cat("\nGroup Comparisons (High vs Low Chatbot Usage):\n")
for(outcome in outcomes) {
  if(sum(!is.na(df[[outcome]]) & !is.na(df$usage_high_low)) > 10) {
    t_test <- t.test(df[[outcome]] ~ df$usage_high_low)
    effect <- effsize::cohen.d(df[[outcome]] ~ df$usage_high_low, na.rm = TRUE)
    
    # Get group statistics
    group_stats <- df %>%
      filter(!is.na(!!sym(outcome)) & !is.na(usage_high_low)) %>%
      group_by(usage_high_low) %>%
      summarise(n = n(), 
                mean = mean(!!sym(outcome)), 
                sd = sd(!!sym(outcome)), 
                .groups = 'drop')
    
    cat("\n", outcome, ":\n")
    print(group_stats)
    cat("t(", round(t_test$parameter, 1), ") = ", round(t_test$statistic, 3), 
        ", p = ", round(t_test$p.value, 3), "\n")
    cat("Cohen's d =", round(effect$estimate, 3), 
        " (", effect$magnitude, " effect)\n")
  }
}

# ANOVA for age groups (if sufficient variation)
if(length(unique(df$age[!is.na(df$age)])) > 2) {
  cat("\nAge Group Comparisons (ANOVA):\n")
  for(outcome in outcomes) {
    if(sum(!is.na(df[[outcome]]) & !is.na(df$age)) > 15) {
      aov_result <- aov(df[[outcome]] ~ factor(df$age))
      aov_summary <- summary(aov_result)
      
      # Effect size (eta-squared)
      eta_sq <- aov_summary[[1]][1, "Sum Sq"] / 
        (aov_summary[[1]][1, "Sum Sq"] + aov_summary[[1]][2, "Sum Sq"])
      
      cat("\n", outcome, " by Age:\n")
      print(aov_summary)
      cat("Eta-squared =", round(eta_sq, 3), "\n")
    }
  }
}

# 8. CORRELATION ANALYSIS
# ======================

cat("\n=== CORRELATION ANALYSIS ===\n")

# Select variables for correlation matrix
cor_vars <- c("mental_effort", "chatbot_eval_score", "arcs_attention_score", 
              "arcs_relevance_score", "arcs_confidence_score", 
              "arcs_satisfaction_score", "arcs_total_score")

cor_data <- df[cor_vars]
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

cat("Correlation Matrix:\n")
print(round(cor_matrix, 3))

# Create correlation plot
corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                   tl.cex = 0.8, tl.col = "black", addCoef.col = "black", 
                   number.cex = 0.7, title = "Scale Intercorrelations")

# Test significance of correlations
cat("\nSignificant Correlations (p < 0.05):\n")
cor_test_results <- psych::corr.test(cor_data, use = "pairwise.complete.obs")
sig_cors <- which(cor_test_results$p < 0.05 & upper.tri(cor_test_results$p), arr.ind = TRUE)

if(nrow(sig_cors) > 0) {
  for(i in 1:nrow(sig_cors)) {
    row_idx <- sig_cors[i, 1]
    col_idx <- sig_cors[i, 2]
    var1 <- rownames(cor_test_results$r)[row_idx]
    var2 <- colnames(cor_test_results$r)[col_idx]
    r_val <- cor_test_results$r[row_idx, col_idx]
    p_val <- cor_test_results$p[row_idx, col_idx]
    
    cat(var1, " <-> ", var2, ": r = ", round(r_val, 3), ", p = ", round(p_val, 3), "\n")
  }
}

# 9. ITEM-LEVEL ANALYSIS
# ======================

cat("\n=== ITEM-LEVEL DESCRIPTIVES ===\n")

# Chatbot evaluation items
cat("Chatbot Evaluation Items:\n")
chatbot_items_desc <- df %>%
  select(all_of(chatbot_eval_items)) %>%
  psych::describe() %>%
  round(2)
print(chatbot_items_desc[c("n", "mean", "sd")])

# ARCS items by subscale
cat("\nARCS - Attention Items:\n")
attention_desc <- df %>%
  select(all_of(arcs_attention_items)) %>%
  psych::describe() %>%
  round(2)
print(attention_desc[c("n", "mean", "sd")])

cat("\nARCS - Relevance Items:\n")
relevance_desc <- df %>%
  select(all_of(arcs_relevance_items)) %>%
  psych::describe() %>%
  round(2)
print(relevance_desc[c("n", "mean", "sd")])

cat("\nARCS - Confidence Items:\n")
confidence_desc <- df %>%
  select(all_of(arcs_confidence_items)) %>%
  psych::describe() %>%
  round(2)
print(confidence_desc[c("n", "mean", "sd")])


cat("\nARCS - Satisfaction Items:\n")
satisfaction_desc <- df %>%
  select(all_of(arcs_satisfaction_items)) %>%
  psych::describe() %>%
  round(2)
print(satisfaction_desc[c("n", "mean", "sd")])

# 10. SUMMARY AND RECOMMENDATIONS
# ===============================

cat("\n=== ANALYSIS SUMMARY ===\n")

total_cases <- nrow(df)
complete_cases <- sum(df$fully_complete, na.rm = TRUE)
usable_cases <- sum(complete.cases(df[composite_vars]))

cat("Sample Summary:\n")
cat("  Total cases: n =", total_cases, "\n")
cat("  Complete cases (all variables): n =", complete_cases, "\n")
cat("  Usable for main analyses: n =", usable_cases, "\n")

cat("\nReliability Summary:\n")
reliability_results <- list(
  "Chatbot Evaluation" = alpha_chatbot, 
  "ARCS - Attention" = alpha_attention,
  "ARCS - Relevance" = alpha_relevance, 
  "ARCS - Confidence" = alpha_confidence,
  "ARCS - Satisfaction" = alpha_satisfaction,
  "ARCS - Total" = alpha_arcs_total
)

for(scale_name in names(reliability_results)) {
  alpha_result <- reliability_results[[scale_name]]
  if(!is.null(alpha_result) && !is.na(alpha_result$total$std.alpha)) {
    status <- ifelse(alpha_result$total$std.alpha >= 0.80, "Excellent", 
                     ifelse(alpha_result$total$std.alpha >= 0.70, "Acceptable", "Poor"))
    cat("  ", scale_name, ": α =", round(alpha_result$total$std.alpha, 3), 
        " (", status, ")\n")
  } else {
    cat("  ", scale_name, ": Could not calculate\n")
  }
}

cat("\nKey Findings:\n")
cat("1. Check scales with α < 0.70 for problematic items\n")
cat("2. Examine significant correlations for theoretical consistency\n") 
cat("3. Consider group differences in chatbot usage patterns\n")
cat("4. Look for ceiling/floor effects in key measures\n")

# Save processed data
write.csv(df, "chatbot_survey_processed.csv", row.names = FALSE)
cat("\nProcessed data saved to: chatbot_survey_processed.csv\n")

# Save key results
results_summary <- list(
  sample_size = total_cases,
  complete_cases = complete_cases,
  reliability_results = sapply(reliability_results, function(x) {
    if(!is.null(x) && !is.na(x$total$std.alpha)) x$total$std.alpha else NA
  }),
  correlations = cor_matrix,
  group_comparisons = "See console output"
)

saveRDS(results_summary, "analysis_results.rds")
cat("Analysis results saved to: analysis_results.rds\n")
