# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(readxl)
library(car) # for MANOVA

# Filter the data to include only the desired groups and exclude cases with missing values
data_filtered_complete_cases <- data_combined %>%
  filter(formal_diagnosis_numeric %in% c("ALS", "Control")) %>%
  drop_na(`Trans-vol-cor-IOC`, `SLong-vol-cor-IOC`, `Genio-vol-cor-IOC`, `ILong-vol-cor-IOC`)



# Descriptive Statistics
# Summary of continuous variables
descriptive_stats_cont <- data_combined %>% 
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                           sd = ~sd(.x, na.rm = TRUE),
                                           median = ~median(.x, na.rm = TRUE),
                                           IQR = ~IQR(.x, na.rm = TRUE),
                                           min = ~min(.x, na.rm = TRUE),
                                           max = ~max(.x, na.rm = TRUE))))

# Summary of categorical variables
descriptive_stats_cat <- data_combined %>% 
  summarise(across(where(is.factor), list(counts = ~table(.x))))

# Output the descriptive statistics
print(descriptive_stats_cont)
print(descriptive_stats_cat)

######
#MANOVA
library(dplyr)
# Ensure that the diagnosis is a factor
data_filtered_complete_cases$formal_diagnosis_numeric <- as.factor(data_filtered_complete_cases$formal_diagnosis_numeric)

# Perform the MANOVA with the dataset that has no missing values
manova_results <- manova(cbind(`Trans-vol-cor-IOC`, `SLong-vol-cor-IOC`, `Genio-vol-cor-IOC`, `ILong-vol-cor-IOC`) ~ formal_diagnosis_numeric, data = data_filtered_complete_cases)

# Summary of MANOVA
summary(manova_results)

# Tukey's HSD tests for each volume
volume_names <- c("`Trans-vol-cor-IOC`", "`SLong-vol-cor-IOC`", "`Genio-vol-cor-IOC`", "`ILong-vol-cor-IOC`")
tukey_results <- list()

for (volume in volume_names) {
  # Fit the model
  model <- aov(reformulate("formal_diagnosis_numeric", response = volume), data = data_filtered_complete_cases)
  
  # Perform Tukey's HSD test
  tukey_results[[volume]] <- TukeyHSD(model, which = "formal_diagnosis_numeric")
  
  # Print Tukey's HSD results
  print(paste("Tukey's HSD for", volume))
  print(tukey_results[[volume]])
}

# Save Tukey's results as PNG with increased height
for (volume in volume_names) {
  png_filename <- paste0("C:/Users/thoma/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical/Tukey_", volume, ".png")
  png(png_filename, height = 800)  # Increased height
  plot(tukey_results[[volume]])
  dev.off()
}

# Plot means comparison
library(gplots)
png_filename <- "C:/Users/thoma/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical/Means_Comparison.png"
png(png_filename, height = 800, width = 480)  # Increased height, default width
plotmeans(`Trans-vol-cor-IOC-clean` ~ formal_diagnosis_numeric, data = data_filtered_complete_cases, xlab = "Groups", ylab = "Corrected Transverse Volume", main = "Means Comparison", col = "blue")
dev.off()




#############
#Plots of stupid things
library(ggplot2)
library(dplyr)
library(tidyr)


# First, filter only ALS and control groups
data_for_plot <- data_filtered_complete_cases %>%
  filter(formal_diagnosis_numeric %in% c("ALS", "Control"))

# Then, gather the volume columns into a long format
data_long <- data_for_plot %>%
  select(`Trans-vol-cor-IOC`, `SLong-vol-cor-IOC`, `Genio-vol-cor-IOC`, `ILong-vol-cor-IOC`, formal_diagnosis_numeric) %>%
  gather(key = "Muscle", value = "Volume", `Trans-vol-cor-IOC`, `SLong-vol-cor-IOC`, `Genio-vol-cor-IOC`, `ILong-vol-cor-IOC`)

# Now create the violin plots
ggplot(data_long, aes(x = Muscle, y = Volume, fill = formal_diagnosis_numeric)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill = "white") +
  facet_wrap(~Muscle, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Group", x = "Muscle Volume", y = "Corrected Volume (mm³)") +
  scale_fill_manual(values = c("ALS" = "blue", "Control" = "red"))


# First, filter only ALS and control groups
data_for_plot <- data_filtered_complete_cases %>%
  filter(formal_diagnosis_numeric %in% c("ALS", "Control"))

# Then, gather the volume columns into a long format
data_long <- data_for_plot %>%
  select(`Trans-vol`, `SLong-vol`, `Genio-vol`, `ILong-vol`, formal_diagnosis_numeric) %>%
  gather(key = "Muscle", value = "Volume", `Trans-vol`, `SLong-vol`, `Genio-vol`, `ILong-vol`)

# Now create the violin plots
ggplot(data_long, aes(x = Muscle, y = Volume, fill = formal_diagnosis_numeric)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill = "white") +
  facet_wrap(~Muscle, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Group", x = "Muscle Volume", y = "Corrected Volume (mm³)") +
  scale_fill_manual(values = c("ALS" = "blue", "Control" = "red"))

############
#Regression Analysis
# Load necessary libraries
library(ggplot2)
library(lmtest)
library(car)

# Define the predictor variables and the outcome variables
predictors <- c("`ALSFRS-R-bulbar`", "age_at_scan", "formal_diagnosis_numeric")
outcome_variables <- c("`Trans-vol-cor-IOC`", "`SLong-vol-cor-IOC`", "`Genio-vol-cor-IOC`", "`ILong-vol-cor-IOC`")

# Loop through each outcome variable to fit a model and check assumptions
for(outcome in outcome_variables) {
  
  # Fit the linear model
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- lm(formula, data = data_filtered_complete_cases)
  
  # Print the model summary
  print(summary(model))
  
  # Plot residuals vs. fitted values (Linearity and Homoscedasticity)
  plot_title <- paste("Residuals vs Fitted for", outcome)
  plot(model, which = 1, main = plot_title)
  
  # Q-Q plot of residuals (Normality)
  qq_title <- paste("Normal Q-Q for", outcome)
  plot(model, which = 2, main = qq_title)
  
  # Residuals vs. index (Independence)
  index_plot <- ggplot(data_filtered_complete_cases, aes(x = seq_along(resid(model)), y = resid(model))) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Residuals vs Index for", outcome), x = "Index", y = "Residuals")
  print(index_plot)
  
  # Durbin-Watson test (Independence)
  dw_test <- dwtest(model)
  print(dw_test)
  
  # Breusch-Pagan test (Homoscedasticity)
  bp_test <- bptest(model)
  print(bp_test)
  
  # Print a line to separate the output for each variable
  cat(rep("-", 80), "\n")
}



# Specify the actual predictors you want to include instead of 'other_predictors'
bulbar_pred_model <- lm(`ALSFRS-R-bulbar` ~ `Trans-vol-cor-IOC` + `SLong-vol-cor-IOC` + `Genio-vol-cor-IOC` + `ILong-vol-cor-IOC`, data = data)

# Outlier Detection and Normalization
# Example for one volume
q75 <- quantile(data$`Trans-vol-cor-IOC`, 0.75, na.rm = TRUE)
q25 <- quantile(data$`Trans-vol-cor-IOC`, 0.25, na.rm = TRUE)
iqr <- q75 - q25

outliers <- data %>% 
  filter(`Trans-vol-cor-IOC` < (q25 - 1.5 * iqr) | `Trans-vol-cor-IOC` > (q75 + 1.5 * iqr))

# Data normalization (placeholder)
data <- data %>%
  mutate(`Trans-vol-cor-IOC` = log(`Trans-vol-cor-IOC`))

# Reporting Results
# Save plots and tables to files
ggsave("scatter_plot.png", plot = last_plot(), device = "png")
write.csv(descriptive_stats, "descriptive_stats.csv")

# Written summary (placeholder)
summary_text <- "Summary of findings will be documented here."

# Advanced Analysis
## Multilevel Modeling
multilevel_models <- list()
# Your multilevel modeling code here

## Survival Analysis
survival_analysis <- list()
# Your survival analysis code here

# Post-hoc Analysis
## Adjust for Multiple Comparisons
# Your multiple comparison adjustment code here

## Sensitivity Analysis
# Your sensitivity analysis code here
