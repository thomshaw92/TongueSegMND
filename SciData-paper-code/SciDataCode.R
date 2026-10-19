##########################
### Segmentation results ###
##########################

library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

# Load the data
data_path <- file.path(Sys.getenv("TONGUE_ROOT", "."), "Projects/Tongue_seg/data/SciDataReleaseData/control-to-release-data-SS.xlsx")
data <- read_excel(data_path)
plot_path <- Sys.getenv("TONGUE_OUT", "output")



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

# Melt the data for plotting
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

print(violin_plot)

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
    select(segmentation.id, all_of(muscle_col))
  return(outliers)
}

# Apply the outlier detection function to each muscle
outliers_transverse_vertical <- detect_outliers(data, "Transverse/Vertical")
outliers_superior_longitudinal <- detect_outliers(data, "Superior Longitudinal")
outliers_genioglossus <- detect_outliers(data, "Genioglossus")
outliers_inferior_longitudinal <- detect_outliers(data, "Inferior Longitudinal")

# Combine all outliers into one data frame
all_outliers <- bind_rows(
  outliers_transverse_vertical %>% mutate(Muscle = "Transverse/Vertical"),
  outliers_superior_longitudinal %>% mutate(Muscle = "Superior Longitudinal"),
  outliers_genioglossus %>% mutate(Muscle = "Genioglossus"),
  outliers_inferior_longitudinal %>% mutate(Muscle = "Inferior Longitudinal")
)

print(all_outliers)


###################
#### Normality ####
###################
# Histogram and Q-Q plot for a variable
normality_plot <- function(var) {
  # Check if the variable exists
  if (!var %in% names(data)) {
    stop(paste("The variable", var, "is not found in the dataset."))
  }
  
  # Convert variable to numeric, if necessary
  data[[var]] <- suppressWarnings(as.numeric(as.character(data[[var]])))
  
  # Check if the conversion resulted in NAs or if the variable is empty
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
  
  return(combined_plot)
}

# Save normality plots for each variable
lapply(c("age.at.scan", "Transverse/Vertical", "Superior Longitudinal", "Genioglossus", "Inferior Longitudinal", "SNR", "CNR"), function(var) {
  # Sanitize the variable name for file naming
  sanitized_var <- gsub("[^A-Za-z0-9]", "_", var)  # Replace non-alphanumeric characters with an underscore
  
  nu_plot_name <- paste0("normality_plot_", sanitized_var, ".pdf")
  
  # Try generating and saving the plot
  try({
    plot_to_save <- normality_plot(var)
    
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
  formula <- as.formula(paste(var, "~ dataset"))
  aov_result <- aov(formula, data = data)
  summary(aov_result)
})

# Name the list elements
names(anova_results) <- variables

anova_results

# Function to perform pairwise t-tests for each variable
pairwise_t_tests <- lapply(variables, function(var) {
  t_test_result <- pairwise.t.test(data[[var]], data$dataset, p.adjust.method = "bonferroni")
  return(t_test_result)
})

# Name the list elements
names(pairwise_t_tests) <- variables

pairwise_t_tests

## CNR/SNR
# Ensure CNR and SNR are numeric
data$CNR <- as.numeric(data$CNR)
data$SNR <- as.numeric(data$SNR)

# Subset data and remove missing values in CNR and SNR
subset_data <- data %>%
  filter(dataset %in% c("EATT", "Sydney")) %>%
  filter(!is.na(CNR) & !is.na(SNR))

# Perform t-tests for CNR and SNR
t_test_CNR <- t.test(CNR ~ dataset, data = subset_data)
t_test_SNR <- t.test(SNR ~ dataset, data = subset_data)

print(t_test_CNR)
print(t_test_SNR)

#############################################
#### Correlations Between Tongue Volumes ####
#############################################

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

print(corr_plot)


file_name <- "correlation_matrix_plot.pdf"

file_path <- file.path(plot_path, file_name)

ggsave(filename = file_path, plot = corr_plot, device = "pdf", width = 8, height = 6)

print(paste("Plot saved to:", file_path))