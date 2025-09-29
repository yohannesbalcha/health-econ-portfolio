install.packages("readxl")
library(readxl)
data <- read_excel("file path")
head(data)
str(data)

# Install and load dplyr
install.packages("dplyr")
library(dplyr)

# Calculate Total Healthcare Payer Costs
data <- data %>%
  mutate(Total_HC_Cost = `Intervention costs` + `Costs of inpatient care` + 
           `Costs of outpatient care` + `Costs of medication`)

# Calculate Total Societal Costs (includes productivity losses)
data <- data %>%
  mutate(Total_Societal_Cost = Total_HC_Cost + Presenteism + Absenteism)

# Preview the new columns
head(data %>% select(Total_HC_Cost, Total_Societal_Cost))

# Compute Mean Costs and QALYs for each perspective
summary_stats_perspective <- data %>%
  group_by(`Group (1 if treatement group, 0 otherwise)`) %>%
  summarise(
    Mean_Total_HC_Cost = mean(Total_HC_Cost, na.rm = TRUE),
    Mean_Total_Societal_Cost = mean(Total_Societal_Cost, na.rm = TRUE),
    Mean_QALYs = mean(QALYs, na.rm = TRUE)
  )

# Display the summary
summary_stats_perspective

# Calculate Incremental Cost and QALYs
incremental_cost_hc <- summary_stats_perspective$Mean_Total_HC_Cost[summary_stats_perspective$`Group (1 if treatement group, 0 otherwise)` == 1] - 
  summary_stats_perspective$Mean_Total_HC_Cost[summary_stats_perspective$`Group (1 if treatement group, 0 otherwise)` == 0]

incremental_cost_societal <- summary_stats_perspective$Mean_Total_Societal_Cost[summary_stats_perspective$`Group (1 if treatement group, 0 otherwise)` == 1] - 
  summary_stats_perspective$Mean_Total_Societal_Cost[summary_stats_perspective$`Group (1 if treatement group, 0 otherwise)` == 0]

incremental_qalys <- summary_stats_perspective$Mean_QALYs[summary_stats_perspective$`Group (1 if treatement group, 0 otherwise)` == 1] - 
  summary_stats_perspective$Mean_QALYs[summary_stats_perspective$`Group (1 if treatement group, 0 otherwise)` == 0]

# Calculate ICERs
icer_hc <- incremental_cost_hc / incremental_qalys
icer_societal <- incremental_cost_societal / incremental_qalys

# Display results
cat("Incremental Cost (Healthcare): ", incremental_cost_hc, "\n")
cat("Incremental Cost (Societal): ", incremental_cost_societal, "\n")
cat("Incremental QALYs: ", incremental_qalys, "\n")
cat("ICER (Healthcare Perspective): ", icer_hc, "\n")
cat("ICER (Societal Perspective): ", icer_societal, "\n")

# Subgroup Analysis by Gender
gender_summary <- data %>%
  group_by(Sex, `Group (1 if treatement group, 0 otherwise)`) %>%
  summarise(
    Mean_Total_HC_Cost = mean(Total_HC_Cost, na.rm = TRUE),
    Mean_Total_Societal_Cost = mean(Total_Societal_Cost, na.rm = TRUE),
    Mean_QALYs = mean(QALYs, na.rm = TRUE),
    .groups = 'drop'
  )

# View gender-specific summary
print(gender_summary)

# Function to calculate incremental values for gender
calculate_gender_icers <- function(gender) {
  treatment <- gender_summary %>% filter(Sex == gender, `Group (1 if treatement group, 0 otherwise)` == 1)
  control <- gender_summary %>% filter(Sex == gender, `Group (1 if treatement group, 0 otherwise)` == 0)
  
  incremental_cost_hc <- treatment$Mean_Total_HC_Cost - control$Mean_Total_HC_Cost
  incremental_cost_societal <- treatment$Mean_Total_Societal_Cost - control$Mean_Total_Societal_Cost
  incremental_qalys <- treatment$Mean_QALYs - control$Mean_QALYs
  
  icer_hc <- incremental_cost_hc / incremental_qalys
  icer_societal <- incremental_cost_societal / incremental_qalys
  
  return(data.frame(
    Gender = gender,
    Incremental_Cost_HC = incremental_cost_hc,
    Incremental_Cost_Societal = incremental_cost_societal,
    Incremental_QALYs = incremental_qalys,
    ICER_HC = icer_hc,
    ICER_Societal = icer_societal
  ))
}

# Apply function for both genders
male_results <- calculate_gender_icers("male")
female_results <- calculate_gender_icers("female")

# Combine and display results
gender_results <- rbind(male_results, female_results)
print(gender_results)

# Create Age Groups
data <- data %>%
  mutate(Age_Group = ifelse(Age <= 61, "18-61", "62+"))

# Subgroup Analysis by Age
age_summary <- data %>%
  group_by(Age_Group, `Group (1 if treatement group, 0 otherwise)`) %>%
  summarise(
    Mean_Total_HC_Cost = mean(Total_HC_Cost, na.rm = TRUE),
    Mean_Total_Societal_Cost = mean(Total_Societal_Cost, na.rm = TRUE),
    Mean_QALYs = mean(QALYs, na.rm = TRUE),
    .groups = 'drop'
  )

# View age-specific summary
print(age_summary)

# Function to calculate incremental values for age
calculate_age_icers <- function(age_group) {
  treatment <- age_summary %>% filter(Age_Group == age_group, `Group (1 if treatement group, 0 otherwise)` == 1)
  control <- age_summary %>% filter(Age_Group == age_group, `Group (1 if treatement group, 0 otherwise)` == 0)
  
  incremental_cost_hc <- treatment$Mean_Total_HC_Cost - control$Mean_Total_HC_Cost
  incremental_cost_societal <- treatment$Mean_Total_Societal_Cost - control$Mean_Total_Societal_Cost
  incremental_qalys <- treatment$Mean_QALYs - control$Mean_QALYs
  
  icer_hc <- incremental_cost_hc / incremental_qalys
  icer_societal <- incremental_cost_societal / incremental_qalys
  
  return(data.frame(
    Age_Group = age_group,
    Incremental_Cost_HC = incremental_cost_hc,
    Incremental_Cost_Societal = incremental_cost_societal,
    Incremental_QALYs = incremental_qalys,
    ICER_HC = icer_hc,
    ICER_Societal = icer_societal
  ))
}

# Apply function for both age groups
age_18_61_results <- calculate_age_icers("18-61")
age_62_plus_results <- calculate_age_icers("62+")

# Combine and display results
age_results <- rbind(age_18_61_results, age_62_plus_results)
print(age_results)

# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define the number of bootstrap iterations
n_iterations <- 1000

# Create empty dataframe to store bootstrap results
bootstrap_results <- data.frame(
  Incremental_QALYs = numeric(n_iterations),
  Incremental_Cost_HC = numeric(n_iterations),
  Incremental_Cost_Societal = numeric(n_iterations)
)

# Start bootstrapping
for (i in 1:n_iterations) {
  
  # Display progress every 100 iterations
  if (i %% 100 == 0) cat("Completed iteration:", i, "\n")
  
  # Resample with replacement
  bootstrap_sample <- data %>% sample_frac(size = 1, replace = TRUE)
  
  # Calculate Healthcare and Societal Costs
  bootstrap_sample <- bootstrap_sample %>%
    mutate(Total_HC_Cost = `Intervention costs` + `Costs of inpatient care` + 
             `Costs of outpatient care` + `Costs of medication`,
           Total_Societal_Cost = Total_HC_Cost + Presenteism + Absenteism)
  
  # Group by Treatment and Control
  summary_bootstrap <- bootstrap_sample %>%
    group_by(`Group (1 if treatement group, 0 otherwise)`) %>%
    summarise(
      Mean_HC_Cost = mean(Total_HC_Cost, na.rm = TRUE),
      Mean_Societal_Cost = mean(Total_Societal_Cost, na.rm = TRUE),
      Mean_QALYs = mean(QALYs, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Explicitly reference control and treatment groups
  control <- summary_bootstrap %>% filter(`Group (1 if treatement group, 0 otherwise)` == 0)
  treatment <- summary_bootstrap %>% filter(`Group (1 if treatement group, 0 otherwise)` == 1)
  
  # Calculate Incremental values
  incremental_cost_hc <- treatment$Mean_HC_Cost - control$Mean_HC_Cost
  incremental_cost_societal <- treatment$Mean_Societal_Cost - control$Mean_Societal_Cost
  incremental_qalys <- treatment$Mean_QALYs - control$Mean_QALYs
  
  # Handle cases where Incremental QALYs == 0 (to avoid division by zero in ICER)
  if (incremental_qalys == 0) {
    incremental_qalys <- NA  # Assign NA to avoid skewed ICERs
  }
  
  # Store bootstrap results
  bootstrap_results$Incremental_QALYs[i] <- incremental_qalys
  bootstrap_results$Incremental_Cost_HC[i] <- incremental_cost_hc
  bootstrap_results$Incremental_Cost_Societal[i] <- incremental_cost_societal
}

# Preview the first few rows of bootstrap results
head(bootstrap_results)

# Sanity Check - Summary statistics for bootstrap results
summary(bootstrap_results)

# Boxplots to visualize outliers
par(mfrow = c(1, 2))  # Side-by-side plots
boxplot(bootstrap_results$Incremental_Cost_HC, main = "HC Cost Incremental - Outliers", col = "lightblue")
boxplot(bootstrap_results$Incremental_QALYs, main = "Incremental QALYs - Outliers", col = "lightgreen")
par(mfrow = c(1, 1))  # Reset plot layout

cat("Bootstrapping complete!\n")

# Function to remove outliers using IQR method
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  data_filtered <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  return(data_filtered)
}

# Apply outlier removal independently to each column
filtered_qalys <- remove_outliers(bootstrap_results, "Incremental_QALYs")
filtered_hc_cost <- remove_outliers(bootstrap_results, "Incremental_Cost_HC")
filtered_societal_cost <- remove_outliers(bootstrap_results, "Incremental_Cost_Societal")

# Find the intersection of filtered rows (rows that survived all filters)
filtered_bootstrap_results <- bootstrap_results %>%
  semi_join(filtered_qalys, by = names(bootstrap_results)) %>%
  semi_join(filtered_hc_cost, by = names(bootstrap_results)) %>%
  semi_join(filtered_societal_cost, by = names(bootstrap_results))

# Verify the filtered data
summary(filtered_bootstrap_results)

# Plot boxplots to confirm outlier removal
par(mfrow = c(1, 3))  # Plot 3 side-by-side

boxplot(filtered_bootstrap_results$Incremental_QALYs, main = "Trimmed Incremental QALYs", col = "lightgreen")
boxplot(filtered_bootstrap_results$Incremental_Cost_HC, main = "Trimmed HC Cost", col = "skyblue")
boxplot(filtered_bootstrap_results$Incremental_Cost_Societal, main = "Trimmed Societal Cost", col = "salmon")

par(mfrow = c(1, 1))  # Reset plotting layout

# Load ggplot2 for plotting
library(ggplot2)

# Create the Cost-Effectiveness Plane using trimmed data
ggplot(filtered_bootstrap_results, aes(x = Incremental_QALYs, y = Incremental_Cost_HC)) +
  geom_point(color = "blue", alpha = 0.5) +  # Healthcare perspective points
  geom_point(aes(y = Incremental_Cost_Societal), color = "red", alpha = 0.5) +  # Societal perspective points
  geom_hline(yintercept = 0, linetype = "dashed") +  # Horizontal line at 0 cost
  geom_vline(xintercept = 0, linetype = "dashed") +  # Vertical line at 0 QALYs
  labs(title = "Cost-Effectiveness Plane (Trimmed Bootstrap Results)",
       x = "Incremental QALYs",
       y = "Incremental Cost (SEK)") +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Healthcare Perspective", "Societal Perspective")) +
  theme_minimal()

# Load necessary library
library(ggplot2)

# Define WTP thresholds (0 to 500,000 SEK with intervals of 10,000)
wtp_values <- seq(0, 500000, by = 10000)

# Initialize a data frame to store NMB results
ceac_results <- data.frame(
  WTP = wtp_values,
  Prob_CE_HC = numeric(length(wtp_values)),
  Prob_CE_Societal = numeric(length(wtp_values))
)

# Calculate Net Monetary Benefit (NMB) and probabilities for CEAC
for (i in seq_along(wtp_values)) {
  wtp <- wtp_values[i]
  
  # NMB for Healthcare Perspective
  nmb_hc <- (filtered_bootstrap_results$Incremental_QALYs * wtp) - filtered_bootstrap_results$Incremental_Cost_HC
  ceac_results$Prob_CE_HC[i] <- mean(nmb_hc > 0)
  
  # NMB for Societal Perspective
  nmb_societal <- (filtered_bootstrap_results$Incremental_QALYs * wtp) - filtered_bootstrap_results$Incremental_Cost_Societal
  ceac_results$Prob_CE_Societal[i] <- mean(nmb_societal > 0)
}

# Plot the CEAC
ggplot(ceac_results, aes(x = WTP)) +
  geom_line(aes(y = Prob_CE_HC, color = "Healthcare Perspective"), size = 1) +
  geom_line(aes(y = Prob_CE_Societal, color = "Societal Perspective"), size = 1) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Cost-Effectiveness Acceptability Curve (CEAC)",
       x = "Willingness-To-Pay Threshold (SEK per QALY)",
       y = "Probability Cost-Effective",
       color = "Perspective") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

# Load necessary library
library(ggplot2)

# Assuming your filtered bootstrap results are stored in a dataframe called 'filtered_bootstrap_results'

# Step 1: Define Quadrants
# Function to classify points into quadrants
classify_quadrant <- function(qaly, cost) {
  if (qaly >= 0 & cost >= 0) {
    return("NE")  # More effective & more costly
  } else if (qaly < 0 & cost >= 0) {
    return("NW")  # Less effective & more costly
  } else if (qaly < 0 & cost < 0) {
    return("SW")  # Less effective & less costly
  } else {
    return("SE")  # More effective & less costly
  }
}

# Apply the function to the filtered data for societal perspective
filtered_bootstrap_results$Quadrant_Societal <- mapply(classify_quadrant, 
                                                       filtered_bootstrap_results$Incremental_QALYs, 
                                                       filtered_bootstrap_results$Incremental_Cost_Societal)

# Step 2: Calculate Proportion in Each Quadrant
quadrant_distribution <- table(filtered_bootstrap_results$Quadrant_Societal)
quadrant_percentages <- prop.table(quadrant_distribution) * 100

# Display quadrant distribution
print("Quadrant Distribution for Societal Perspective:")
print(quadrant_distribution)

print("Quadrant Percentages for Societal Perspective:")
print(round(quadrant_percentages, 2))

# Step 3: Plot the Cost-Effectiveness Plane with Quadrants Highlighted
ggplot(filtered_bootstrap_results, aes(x = Incremental_QALYs, y = Incremental_Cost_Societal)) +
  geom_point(aes(color = Quadrant_Societal), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Cost-Effectiveness Plane (Societal Perspective) with Quadrants",
       x = "Incremental QALYs",
       y = "Incremental Societal Cost (SEK)") +
  scale_color_manual(values = c("NE" = "blue", "NW" = "purple", "SW" = "red", "SE" = "green"),
                     name = "Quadrant") +
  theme_minimal()

install.packages("tidyr")
library(tidyr)

# Load required libraries
library(ggplot2)
library(dplyr)

# Calculate mean incremental values for both perspectives
mean_qaly <- mean(filtered_bootstrap_results$Incremental_QALYs)
mean_cost_hc <- mean(filtered_bootstrap_results$Incremental_Cost_HC)
mean_cost_societal <- mean(filtered_bootstrap_results$Incremental_Cost_Societal)

# Calculate 95% Confidence Intervals
ci_qaly <- quantile(filtered_bootstrap_results$Incremental_QALYs, probs = c(0.025, 0.975))
ci_cost_hc <- quantile(filtered_bootstrap_results$Incremental_Cost_HC, probs = c(0.025, 0.975))
ci_cost_societal <- quantile(filtered_bootstrap_results$Incremental_Cost_Societal, probs = c(0.025, 0.975))

# Define WTP line slope
wtp_threshold <- 500000

# Prepare data for plotting
filtered_bootstrap_long <- filtered_bootstrap_results %>%
  pivot_longer(cols = c(Incremental_Cost_HC, Incremental_Cost_Societal),
               names_to = "Perspective", values_to = "Incremental_Cost") %>%
  mutate(Perspective = ifelse(Perspective == "Incremental_Cost_HC", "Healthcare", "Societal"))

# Plot the Cost-Effectiveness Plane
ggplot(filtered_bootstrap_long, aes(x = Incremental_QALYs, y = Incremental_Cost, color = Perspective)) +
  geom_point(alpha = 0.4) +  # Scatter plot for both perspectives
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Vertical reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Horizontal reference line
  
  # Add WTP Line
  geom_abline(slope = wtp_threshold, intercept = 0, linetype = "dotted", color = "blue", size = 1) +
  
  # Plot Mean Points for both perspectives
  geom_point(aes(x = mean_qaly, y = mean_cost_hc), shape = 18, color = "darkblue", size = 4) +
  geom_point(aes(x = mean_qaly, y = mean_cost_societal), shape = 17, color = "darkred", size = 4) +
  
  # Add 95% Confidence Interval (CI) Lines for Healthcare Perspective
  geom_errorbar(aes(x = mean_qaly, ymin = ci_cost_hc[1], ymax = ci_cost_hc[2]), width = 0.001, color = "darkblue", size = 1) +
  geom_errorbarh(aes(y = mean_cost_hc, xmin = ci_qaly[1], xmax = ci_qaly[2]), height = 50, color = "darkblue", size = 1) +
  
  # Add 95% CI Lines for Societal Perspective
  geom_errorbar(aes(x = mean_qaly, ymin = ci_cost_societal[1], ymax = ci_cost_societal[2]), width = 0.001, color = "darkred", size = 1) +
  geom_errorbarh(aes(y = mean_cost_societal, xmin = ci_qaly[1], xmax = ci_qaly[2]), height = 50, color = "darkred", size = 1) +
  
  labs(title = "Cost-Effectiveness Plane (Healthcare & Societal) with WTP, Means, and CIs",
       x = "Incremental QALYs",
       y = "Incremental Cost (SEK)") +
  scale_color_manual(values = c("Healthcare" = "blue", "Societal" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(min(filtered_bootstrap_results$Incremental_QALYs) - 0.01, 
                           max(filtered_bootstrap_results$Incremental_QALYs) + 0.01),
                  ylim = c(min(filtered_bootstrap_long$Incremental_Cost) - 500, 
                           max(filtered_bootstrap_long$Incremental_Cost) + 500)) +
  annotate("text", x = max(filtered_bootstrap_results$Incremental_QALYs), 
           y = wtp_threshold * max(filtered_bootstrap_results$Incremental_QALYs),
           label = paste("WTP =", wtp_threshold, "SEK/QALY"), color = "blue", hjust = 1, vjust = -1)

# Load required libraries
library(dplyr)

# Base case values
base_qaly <- mean(bootstrap_results$Incremental_QALYs, na.rm = TRUE)
base_cost_hc <- mean(bootstrap_results$Incremental_Cost_HC, na.rm = TRUE)
base_icer <- base_cost_hc / base_qaly

# Define parameters for OWSA
parameters <- c("Intervention costs", "Costs of inpatient care", 
                "Costs of outpatient care", "Costs of medication", 
                "Presenteism", "Absenteism", "QALYs")

# Calculate mean values of parameters
param_means <- data %>%
  summarise(across(all_of(parameters), mean, na.rm = TRUE))

# Set variation range (±20%)
variation <- 0.2

# Initialize dataframe for OWSA results
owsa_results <- data.frame(
  Parameter = character(),
  Low_ICER = numeric(),
  High_ICER = numeric(),
  stringsAsFactors = FALSE
)

# Perform OWSA
for (param in parameters) {
  base_value <- param_means[[param]]
  
  # Define low and high values (±20%)
  low_value <- base_value * (1 - variation)
  high_value <- base_value * (1 + variation)
  
  # Adjust based on parameter type (QALY vs. cost)
  if (param == "QALYs") {
    # Vary QALYs
    low_qaly <- base_qaly * (1 - variation)
    high_qaly <- base_qaly * (1 + variation)
    
    # Calculate ICERs
    low_icer <- base_cost_hc / low_qaly
    high_icer <- base_cost_hc / high_qaly
  } else {
    # Vary cost parameters
    low_cost <- base_cost_hc - (base_value - low_value)
    high_cost <- base_cost_hc + (high_value - base_value)
    
    # Calculate ICERs
    low_icer <- low_cost / base_qaly
    high_icer <- high_cost / base_qaly
  }
  
  # Store results
  owsa_results <- rbind(owsa_results, data.frame(
    Parameter = param,
    Low_ICER = low_icer,
    High_ICER = high_icer
  ))
}

# Display OWSA results
print(owsa_results)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Prepare data for plotting
owsa_plot_data <- owsa_results %>%
  mutate(
    Parameter = factor(Parameter, levels = Parameter[order(abs(High_ICER - Low_ICER), decreasing = TRUE)]),
    Low = pmin(Low_ICER, High_ICER),
    High = pmax(Low_ICER, High_ICER),
    Impact = ifelse(Low < base_icer & High > base_icer, "Mixed",
                    ifelse(High < base_icer, "Decrease", "Increase"))
  )

# Calculate Base ICER for reference line
base_icer <- mean(c(owsa_results$Low_ICER, owsa_results$High_ICER))

# Plot Tornado Diagram with Correct Colors
ggplot(owsa_plot_data) +
  geom_segment(aes(x = Parameter, xend = Parameter, y = Low, yend = High, color = Impact), size = 6) +
  coord_flip() +
  geom_vline(xintercept = base_icer, linetype = "dashed", color = "red", size = 1) +
  scale_color_manual(values = c("Increase" = "blue", "Decrease" = "gray", "Mixed" = "purple")) +
  labs(
    title = "Tornado Diagram - One-Way Sensitivity Analysis",
    x = "Parameter",
    y = "ICER (SEK/QALY)",
    color = "Impact on ICER"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )






