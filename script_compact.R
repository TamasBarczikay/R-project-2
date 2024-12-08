# Load required packages
library(dplyr)
library(ggplot2)
library(vcd) # Phi coeffs
library(MASS)  # for the different phi function

# Read in data
data <- read.csv("/Users/tamasbarczikay/Desktop/Innio/data.csv")

# Clean data
data <- data %>%
  filter(!is.na(pist_m)) %>%
  select(-op_set_2) %>%
  filter(oph != 1000000000) %>%
  mutate(
    issue_type = as.factor(issue_type),
    resting_analysis_results = factor(resting_analysis_results, 
                                      levels = c(0, 1, 2),
                                      labels = c("normal", "abnormal", "critical"))
  ) %>%
  select(-op_set_1, -op_set_3)

################################################################################
# Baseline stats
################################################################################
# point-biserial correlation with the following vars
numeric_variables <- c("oph", "bmep", "ng_imp", "rpm_max", "number_up", "number_tc")

correlation_table <- data.frame(
  variable = character(),
  correlation = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in numeric_variables) {
  cor_test_result <- cor.test(data$high_breakdown_risk, data[[var]])
  
  correlation_table <- rbind(
    correlation_table,
    data.frame(
      variable = var,
      correlation = round(cor_test_result$estimate, 3),
      p_value = round(cor_test_result$p.value, 3)
    )
  )
}
rownames(correlation_table) <- NULL

# phi-coefficient is suitable for a 2x2 contingency table
categorical_variables <- c("pist_m", "past_dmg", "full_load_issues")

phi_coefficient_table <- data.frame(
  variable = character(),
  phi_coefficient = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in categorical_variables) {
  contingency_table <- table(data$high_breakdown_risk, data[[var]])
  chi_square_result <- chisq.test(contingency_table)
  
  # Calculate phi coefficient using assocstats
  phi_coefficient <- round(assocstats(contingency_table)$phi, 3)
  
  phi_coefficient_table <- rbind(
    phi_coefficient_table,
    data.frame(
      variable = var,
      phi_coefficient = phi_coefficient,
      p_value = round(chi_square_result$p.value, 3)
    )
  )
}

# factor variables with more than two levels
factor_variables <- c("issue_type", "resting_analysis_results", "number_tc")

correlation_table_factors <- data.frame(
  variable = character(),
  correlation = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in factor_variables) {
  cor_test_result <- chisq.test(table(data$high_breakdown_risk, data[[var]]))
  
  correlation_table_factors <- rbind(
    correlation_table_factors,
    data.frame(
      variable = var,
      correlation = round(sqrt(cor_test_result$statistic / sum(cor_test_result$observed)), 3),
      p_value = round(cor_test_result$p.value, 2)
    )
  )
}

# Remove row names
rownames(correlation_table_factors) <- NULL


# Empty global environment except for data
global_environment <- ls()
objects_to_keep <- c("data", "correlation_table",
                     "phi_coefficient_table",
                     "correlation_table_factors")
removeable <- global_environment[!global_environment %in% objects_to_keep]
rm(list = removeable, global_environment, objects_to_keep)

# Tables will be printed in the end of the script to be able to 
# analyse it together with the graphs

################################################################################
# Graph categorical vars
################################################################################
ggplot(data, aes(x = issue_type, fill = factor(high_breakdown_risk))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown Risk for each Issue Types",
       x = "Issue Type",
       y = "Percentage") +
  theme_classic() +
  guides(fill = FALSE) +
  annotate("text", x = Inf, y = -Inf, label = "Teal: High Breakdown Risk, Red: Low Breakdown Risk",
           hjust = 1, vjust = -0.5, size = 3, color = "black", angle = 0)

ggplot(data, aes(x = resting_analysis_results, fill = factor(high_breakdown_risk))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown Risk Based on Resting Analysis Results",
       x = "Resting Analysis Results",
       y = "Percentage") +
  theme_classic() +
  guides(fill = FALSE) +
  annotate("text", x = Inf, y = -Inf, label = "Teal: High Breakdown Risk, Red: Low Breakdown Risk",
           hjust = 1, vjust = -0.5, size = 3, color = "black", angle = 0)

ggplot(data, aes(x = factor(number_tc), fill = factor(high_breakdown_risk, labels = c("Low", "High")))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown by Number of Turbo Charges",
       x = "number_tc",
       y = "Percentage",
       fill = "Breakdown Risk") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL))

# Clear console
cat("\014")

# Print both tables with different correlation stats
print(correlation_table)
print(phi_coefficient_table)
print(correlation_table_factors)

