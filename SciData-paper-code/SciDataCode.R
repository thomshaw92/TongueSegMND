##########################
### Segmentation results ###
##########################

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

# Load the data (assuming you have saved it as an Excel file)
# Replace "your_file_path.xlsx" with the actual path to your file
# Load the data from the specified directory
data_path <- "/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Projects/Tongue_seg/data/SciDataReleaseData/control-to-release-data-SS.xlsx"
data <- read_excel(data_path)
plot_path <- "/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Publications/2024_SciData_Tongue/"



####################
## Muscle Volumes ##
####################



# Rename columns for muscle names
data <- data %>%
  rename(
    `Transverse/Vertical` = Trans.vol,
    `Superior Longitudinal` = SLong.vol,
    `Genioglossus` = Genio.vol,
    `Inferior Longitudinal` = ILong.vol
  )

# Melt the data for plotting, including 'dataset' column
melted_data <- data %>%
  select(dataset, `Transverse/Vertical`, `Superior Longitudinal`, `Genioglossus`, `Inferior Longitudinal`) %>%
  pivot_longer(cols = c(`Transverse/Vertical`, `Superior Longitudinal`, `Genioglossus`, `Inferior Longitudinal`),
               names_to = "Muscle", values_to = "Volume")

# Violin plot with custom colors, faceted by dataset
violin_plot <- ggplot(melted_data, aes(x = Muscle, y = Volume, fill = Muscle)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c(
    "Transverse/Vertical" = "#b3e2cd",
    "Superior Longitudinal" = "#fdcdac",
    "Genioglossus" = "#cbd5e8",
    "Inferior Longitudinal" = "#f4cae4"
  )) +
  theme_minimal() +
  labs(title = "Violin Plots of Tongue Muscle Volumes by Dataset",
       x = "Muscle",
       y = "Volume") +
  facet_wrap(~dataset, ncol = 1)  # Facet by dataset, arrange plots vertically

# Display the plot
print(violin_plot)

# Save the violin plot
ggsave(filename = file.path(plot_path, "violin_plot_tongue_muscle_volumes_by_dataset.pdf"), plot = violin_plot, device = "pdf", width = 8, height = 12)


########################
## Outlier detection  ##
########################


# Function to detect outliers using IQR method with NA handling
detect_outliers <- function(df, muscle_col) {
  Q1 <- quantile(df[[muscle_col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[muscle_col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- df %>%
    filter(df[[muscle_col]] < lower_bound | df[[muscle_col]] > upper_bound) %>%
    select(segmentation.id, all_of(muscle_col))  # Use all_of() here
  return(outliers)
}

# Apply the outlier detection function to each muscle
outliers_transverse_vertical <- detect_outliers(data, "Transverse/Vertical")
outliers_superior_longitudinal <- detect_outliers(data, "Superior Longitudinal")
outliers_genioglossus <- detect_outliers(data, "Genioglossus")
outliers_inferior_longitudinal <- detect_outliers(data, "Inferior Longitudinal")

# Combine all outliers into one data frame for easier inspection
all_outliers <- bind_rows(
  outliers_transverse_vertical %>% mutate(Muscle = "Transverse/Vertical"),
  outliers_superior_longitudinal %>% mutate(Muscle = "Superior Longitudinal"),
  outliers_genioglossus %>% mutate(Muscle = "Genioglossus"),
  outliers_inferior_longitudinal %>% mutate(Muscle = "Inferior Longitudinal")
)

# Display the detected outliers
print(all_outliers)


###################
#### Normality ####
###################
# Modified normality_plot function to ensure the variable is continuous and handle empty or non-numeric data
normality_plot <- function(var) {
  # Check if the variable exists and has numeric values
  if (!var %in% names(data)) {
    stop(paste("The variable", var, "is not found in the dataset."))
  }
  
  # Convert variable to numeric, if necessary
  data[[var]] <- suppressWarnings(as.numeric(as.character(data[[var]])))
  
  # Check if the conversion resulted in NAs (indicative of non-numeric data) or if the variable is empty
  if (all(is.na(data[[var]])) || length(data[[var]]) == 0) {
    stop(paste("The variable", var, "is not continuous, numeric, or has no valid data for plotting."))
  }
  
  # Create histogram
  p1 <- ggplot(data, aes(x = !!sym(var))) + 
    geom_histogram(bins = 30, fill = "lightblue", color = "black") + 
    ggtitle(paste("Histogram for", var))
  
  # Create Q-Q plot
  p2 <- ggplot(data, aes(sample = !!sym(var))) + 
    stat_qq() + 
    stat_qq_line() + 
    ggtitle(paste("Q-Q Plot for", var))
  
  # Arrange both plots
  combined_plot <- grid.arrange(p1, p2, ncol = 2)
  
  # Return the combined plot
  return(combined_plot)
}

# Save normality plots for each variable
lapply(c("age.at.scan", "Transverse/Vertical", "Superior Longitudinal", "Genioglossus", "Inferior Longitudinal", "SNR", "CNR"), function(var) {
  # Sanitize the variable name for file naming
  sanitized_var <- gsub("[^A-Za-z0-9]", "_", var)  # Replace non-alphanumeric characters with an underscore
  
  # Create the file name
  nu_plot_name <- paste0("normality_plot_", sanitized_var, ".pdf")
  
  # Try generating and saving the plot
  try({
    # Generate the plot using the normality_plot function
    plot_to_save <- normality_plot(var)
    
    # Save the plot as PDF
    ggsave(filename = file.path(plot_path, nu_plot_name), plot = plot_to_save, device = "pdf", width = 8, height = 6)
  }, silent = TRUE)
})
#####################################
#### Differences Between Dataset ####
#####################################

# Ensure sex.numerical is a factor
data$sex.numerical <- as.factor(data$sex.numerical)
# Rename columns to avoid special characters
data <- data %>%
  rename(
    Transverse_Vertical = `Transverse/Vertical`,
    Superior_Longitudinal = `Superior Longitudinal`,
    Inferior_Longitudinal = `Inferior Longitudinal`
  )
# Remove rows with NA, NaN, or Inf values in the variables of interest
data <- data %>%
  filter(
    complete.cases(age.at.scan, Transverse_Vertical, Superior_Longitudinal, Genioglossus, Inferior_Longitudinal, SNR, CNR)
  )
# Ensure 'dataset' is a factor
data$dataset <- as.factor(data$dataset)

# List of variables to analyze
variables <- c("age.at.scan", "Transverse_Vertical", "Superior_Longitudinal", "Genioglossus", "Inferior_Longitudinal", "SNR", "CNR")

# Function to perform ANOVA for each variable
anova_results <- lapply(variables, function(var) {
  formula <- as.formula(paste(var, "~ dataset"))  # Change to dataset
  aov_result <- aov(formula, data = data)
  summary(aov_result)
})

# Name the list elements for easier reference
names(anova_results) <- variables

# Print ANOVA results
anova_results

# Function to perform pairwise t-tests for each variable
pairwise_t_tests <- lapply(variables, function(var) {
  t_test_result <- pairwise.t.test(data[[var]], data$dataset, p.adjust.method = "bonferroni")  # Change to dataset
  return(t_test_result)
})

# Name the list elements for easier reference
names(pairwise_t_tests) <- variables

# Print pairwise t-test results
pairwise_t_tests

## just EATT and syd for CNR/SNR
# Ensure CNR and SNR are numeric
data$CNR <- as.numeric(data$CNR)
data$SNR <- as.numeric(data$SNR)

# Subset data to include only EATT and Sydney, and remove missing values in CNR and SNR
subset_data <- data %>%
  filter(dataset %in% c("EATT", "Sydney")) %>%
  filter(!is.na(CNR) & !is.na(SNR))

# Perform t-tests for CNR and SNR
t_test_CNR <- t.test(CNR ~ dataset, data = subset_data)
t_test_SNR <- t.test(SNR ~ dataset, data = subset_data)

# Print the results
print(t_test_CNR)
print(t_test_SNR)

#############################################
#### Correlations Between Tongue Volumes ####
#############################################

# Load the necessary package for melt
library(reshape2)

# Correlation matrix for tongue volumes
tongue_volumes <- data %>% select(Transverse_Vertical, Superior_Longitudinal, Genioglossus, Inferior_Longitudinal)
correlation_matrix <- cor(tongue_volumes, use = "complete.obs")

# Melt the correlation matrix for plotting
melted_corr_matrix <- melt(correlation_matrix)

# Plot the correlation matrix
corr_plot <- ggplot(melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(low = "lightblue", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal() +
  ggtitle("Correlation Matrix for Tongue Volumes")

# Display the plot
print(corr_plot)
# Specify the file path


# Create an informative file name for the plot
file_name <- "correlation_matrix_plot.pdf"

# Combine plot_path and file_name to create the full file path
file_path <- file.path(plot_path, file_name)

# Save the plot as a PDF to the specified file path
ggsave(filename = file_path, plot = corr_plot, device = "pdf", width = 8, height = 6)

# Confirm the plot has been saved
print(paste("Plot saved to:", file_path))