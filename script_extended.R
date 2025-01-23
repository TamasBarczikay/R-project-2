################################################################################
# Gather domain specific knowledge
################################################################################
# Cogeneration systems are  devices that are built into facilities and households
# to decrease energy consumption and at the same time serve as heating.

# Understanding the variables
# oph: operating hours of the engine
# pist_m: piston material
# issue_type: combustion issue type
# - value 1: typical issue
# - value 2: atypical issue
# - value 3: non-related
# - value 4: non-symptomatic
# bmep: break mean effective pressure is the average preassure forcing the pistions inside an engine down
# ng_imp: natural gas impurities measured in nmol
# past_dmg: engine had past damages (1 = true; 0 = false)
# resting_analysis_results: resting results after operation
# - Value 0: normal
# - Value 1: abnormal
# - Value 2: critical
# rpm_max: maximum rotations per minute achieved
# full_load_issues: issues were induced due to full load operation (1 = yes; 0 = no)
# number_up: number of unplanned events
# number_tc: number of installed turbo chargers
# op_set_1: operational engine setting 1
# op_set_2: operational engine setting 2
# op_set_3: operational engine setting 3
# breakdown: 0 = less chance of breakdown 1= more chance of breakdown

################################################################################
# Tasks
################################################################################
# Which patterns can you derive from the data and which analytical 
# and/or statistical statements can you make based on the data?

# Which attributes would be relevant for forecasting the risk of a breakdown 
# and which models would suit the task?

################################################################################
# Load dataset
################################################################################
library(dplyr)
library(vcd) # Phi coeffs
library(ggplot2)

data_raw <- read.csv("/Users/tamasbarczikay/R_projects/R-project-2/data.csv")

# Check dimensions of the dataset - 316x15 df
print(dim(data_raw))
print(class(data_raw))

################################################################################
# Clean variables
################################################################################
# Structure
str(data_raw)

# Missing values (NA or empty string) - op_set_2 can be excluded
print(colSums(is.na(data_raw) | data_raw == ""))

# Check whether the  two missing values are coming from the same rows
data_raw %>% filter(is.na(pist_m)) %>% print()

# They do, so I exclude them from the analysis
data_clean <- data_raw %>%
  filter(is.na(pist_m) == FALSE)

# Works like charm!
print(colSums(is.na(data_clean) | data_clean == ""))

# Sanity check of variables
for (col in colnames(data_clean)) {
  unique_values <- unique(data_clean[[col]])
  cat("Column:", col, "\n")
  cat("Unique Values:", unique_values, "\n\n")
}

# Issues:
# 1) extreme value in oph
# 2) issue_type should be turned into a factor
# 3) resting_analysis_results should be turned into a factor
# 4) op_set_1 and op_set_3 have no variation + op_set_2 from before


# 1) oph has an extreme value 1000000000
print(unique(data_raw$oph))
# There were 44 different oph values and none looked similar in volume
# so I excluded this row from the analysis

# Excluding this observation
data_clean <- data_raw %>%
  filter(is.na(pist_m) == FALSE) %>%
  filter(oph != 1000000000)

# 2) issue_type should be turned into a factor
# issue_type: combustion issue type
# - value 1: typical issue
# - value 2: atypical issue
# - value 3: non-related
# - value 4: non-symptomatic

data_clean <- data_raw %>%
  filter(!is.na(pist_m)) %>%
  filter(oph != 1000000000) %>%
  mutate(issue_type = as.factor(issue_type))

str(data_clean)

# 3) resting_analysis_results should be turned into a factor
# resting_analysis_results: resting results after operation
# - Value 0: normal
# - Value 1: abnormal
# - Value 2: critical

data_clean <- data_raw %>%
  filter(!is.na(pist_m)) %>%
  filter(oph != 1000000000) %>%
  mutate(
    issue_type = as.factor(issue_type),
    resting_analysis_results = factor(resting_analysis_results, 
                                      levels = c(0, 1, 2),
                                      labels = c("normal", "abnormal", "critical"))
  )

str(data_clean)

# 4) op_set_1 and op_set_3 have no variation + op_set_2 from before
data_clean <- data_raw %>%
  filter(!is.na(pist_m)) %>%
  filter(oph != 1000000000) %>%
  mutate(
    issue_type = as.factor(issue_type),
    resting_analysis_results = factor(resting_analysis_results, 
                                      levels = c(0, 1, 2),
                                      labels = c("normal", "abnormal", "critical"))
  )

str(data_clean)

# remove non-needed dfs
rm(data_raw, col, unique_values)

################################################################################
# Baseline stats
################################################################################
# point-biserial correlation with the following vars
# high_breakdown_risk - oph, bmep, ng_imp, rpm_max, number_up, number_tc

# Create list of numeric vars
numeric_variables <- c("oph", "bmep", "ng_imp", "rpm_max", "number_up", "number_tc")

# Create empty table to store caclulations
correlation_table <- data.frame(
  variable = character(),
  correlation = numeric(),
  stringsAsFactors = FALSE
)

# Fill in the cells
for (var in numeric_variables) {
  cor_test_result <- cor.test(data_clean$high_breakdown_risk, data_clean[[var]])
  
  correlation_table <- rbind(
    correlation_table,
    data.frame(
      variable = var,
      correlation = cor_test_result$estimate
    )
  )
}

# Print the corr table
print(correlation_table)

# phi-coefficient is suitable fro for 2x2 contingency table
# high_breakdown_risk - pist_m, past_dmg, full_load_issues

# Create list of factor vars
categorical_variables <- c("pist_m", "past_dmg", "full_load_issues")

# Similar empty data frame is needed
phi_coefficient_table <- data.frame(
  variable = character(),
  phi_coefficient = numeric(),
  stringsAsFactors = FALSE
)

# Fill in cells as before
for (var in categorical_variables) {
  contingency_table <- table(data_clean$high_breakdown_risk, data_clean[[var]])
  assoc_stats <- vcd::assocstats(contingency_table)
  
  phi_coefficient_table <- rbind(
    phi_coefficient_table,
    data.frame(
      variable = var,
      phi_coefficient = assoc_stats$phi
    )
  )
}

# Print the table
print(phi_coefficient_table)

# Empty global environment except for data_clean
global_environment <- ls()
removeable <- global_environment[global_environment != "data_clean"]
rm(list = removeable, global_environment, removeable)


################################################################################
# Graph categorical vars
################################################################################
ggplot(data_clean, aes(x = issue_type, fill = factor(high_breakdown_risk))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown Risk for each Issue Types",
       x = "Issue Type",
       y = "Percentage") +
  theme_classic() +
  guides(fill = FALSE) +
  annotate("text", x = Inf, y = -Inf, label = "Teal: High Breakdown Risk, Red: Low Breakdown Risk",
           hjust = 1, vjust = -0.5, size = 3, color = "black", angle = 0)

ggplot(data_clean, aes(x = resting_analysis_results, fill = factor(high_breakdown_risk))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown Risk Based on Resting Analysis Results",
       x = "Resting Analysis Results",
       y = "Percentage") +
  theme_classic() +
  guides(fill = FALSE) +
  annotate("text", x = Inf, y = -Inf, label = "Teal: High Breakdown Risk, Red: Low Breakdown Risk",
           hjust = 1, vjust = -0.5, size = 3, color = "black", angle = 0)

# pist_m
ggplot(data_clean, aes(x = factor(pist_m), fill = factor(high_breakdown_risk, labels = c("Low", "High")))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown Risk by Piston Material",
       x = "Piston Material",
       y = "Percentage",
       fill = "Breakdown Risk") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL))

# past_dmg
ggplot(data_clean, aes(x = factor(past_dmg), fill = factor(high_breakdown_risk, labels = c("Low", "High")))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown Risk by Past Damage",
       x = "Past Damage",
       y = "Percentage",
       fill = "Breakdown Risk") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL))

# full_load_issues
ggplot(data_clean, aes(x = factor(full_load_issues), fill = factor(high_breakdown_risk, labels = c("Low", "High")))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown Full Load Issues",
       x = "full_load_issues",
       y = "Percentage",
       fill = "Breakdown Risk") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL))

# number_tc
ggplot(data_clean, aes(x = factor(number_tc), fill = factor(high_breakdown_risk, labels = c("Low", "High")))) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Breakdown by Number of Turbo Charges",
       x = "number_tc",
       y = "Percentage",
       fill = "Breakdown Risk") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL))


# NOTES for the presentation:
# op_set_2 column is empty
# oph has no missing values
# 2 missing for all other
# Entry error in oph deleted
# no variation in op_set_1 and op_set_3 columns

# Uncorrelated vars
# ng_imp            -0.093   0.102, numeric
# past_dmg           0.036   0.634, category

# Correlated with multiple categories (based on graphs)
# issue_type                -   typical value has low breakdown risk
# resting_analysis_results  -   critical value has low breakdown risk
# number_tc                 -   2 value has high breakdown risk




