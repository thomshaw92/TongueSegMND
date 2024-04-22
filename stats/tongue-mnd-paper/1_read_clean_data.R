library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(DescTools)

# Directory to save the plots
plots_directory <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical"
data_dir <- ("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/")
file_name <- "main-tongue-data-spreadsheet.xlsx"
file_path <- file.path(data_dir, file_name)

# Step 1: Read in the data
df <- read_excel(file_path)
# Step 2: Rename columns to be more R-friendly (avoid spaces and special characters)
colnames(df) <- make.names(colnames(df))

#Re-code 0s to 9s in formal_diagnosis_numeric

df$formal_diagnosis_numeric <- recode(df$formal_diagnosis_numeric, `0` = 9)

# List of IDs to exclude due to poor seg or missing ALSFRS-R (NB this is ifferent from seg paper)
exclude_ids <- c(9, 61, 126, 7, 63, 116, 21, 72, 149, 84, 121, 22, 129, 15, 133, 135,
                 138, 161, 168, 176, 182, 185, 186, 193, 198, 200, 201, 103)

# Filter out the data
df <- df %>% 
  filter(!(`segmentation.id` %in% exclude_ids))

# Recode formal_diagnosis_numeric to have more descriptive labels
df$formal_diagnosis_numeric <- factor(
  df$formal_diagnosis_numeric,
  levels = c(1, 2, 3, 4, 6, 7, 8, 9), # Exclude 0 since it's recoded to 9
  labels = c("ALS", "PLS", "PBP", "Flail limb/PMA", "Other mimic 1", "Other mimic 2", "Other mimic 3", "Control")
)

# Verify labels after recoding
print(table(df$formal_diagnosis_numeric))

# Define which columns represent tongue volume
tongue_volume_columns <- c("Trans.vol.cor.IOC", "SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC")


# Function to winsorize a vector at the 1st and 99th percentiles
winsorize_at_percentiles <- function(x, lower_perc = 0.01, upper_perc = 0.99) {
  lower_bound <- quantile(x, lower_perc, na.rm = TRUE)
  upper_bound <- quantile(x, upper_perc, na.rm = TRUE)
  Winsorize(x, probs = c(lower_perc, upper_perc), na.rm = TRUE)
}


# Apply the winsorize function to  data for each tongue volume column
controls <- df %>% 
  filter(formal_diagnosis == "Control") %>%
  mutate(across(all_of(tongue_volume_columns), ~winsorize_at_percentiles(.)))
# Apply the winsorize function to data for each tongue volume column
patients <- df %>% 
  filter(formal_diagnosis != "Control") %>% # Assuming "Control" is the only non-patient category
  mutate(across(all_of(tongue_volume_columns), ~winsorize_at_percentiles(.)))


# Combine the cleaned data from controls and patients
df_clean <- bind_rows(controls, patients)

# Normality checks for the cleaned tongue volume data
normality_results <- list()
for (col in tongue_volume_columns) {
  column_data <- df_clean[[col]][!is.na(df_clean[[col]])]
  shapiro_test <- shapiro.test(column_data)
  normality_results[[col]] <- shapiro_test$p.value
  
  # Interpretation of the Shapiro-Wilk test results
  if(shapiro_test$p.value > 0.05) {
    print(paste(col, "appears to be normally distributed (p-value:", shapiro_test$p.value, ")"))
  } else {
    print(paste(col, "does not appear to be normally distributed (p-value:", shapiro_test$p.value, ")"))
  }
}

# Perform independent normality checks for controls and ALS patients
normality_results_controls <- list()
normality_results_als <- list()

for (col in tongue_volume_columns) {
  # Controls
  column_data_controls <- controls[[col]][!is.na(controls[[col]])]
  shapiro_test_controls <- shapiro.test(column_data_controls)
  normality_results_controls[[col]] <- shapiro_test_controls$p.value
  
  if(shapiro_test_controls$p.value > 0.05) {
    print(paste(col, "in controls appears to be normally distributed (p-value:", shapiro_test_controls$p.value, ")"))
  } else {
    print(paste(col, "in controls does not appear to be normally distributed (p-value:", shapiro_test_controls$p.value, ")"))
  }
  
  # ALS Patients
  column_data_als <- patients[[col]][!is.na(patients[[col]]) & patients$formal_diagnosis_numeric == "ALS"]
  shapiro_test_als <- shapiro.test(column_data_als)
  normality_results_als[[col]] <- shapiro_test_als$p.value
  
  if(shapiro_test_als$p.value > 0.05) {
    print(paste(col, "in ALS patients appears to be normally distributed (p-value:", shapiro_test_als$p.value, ")"))
  } else {
    print(paste(col, "in ALS patients does not appear to be normally distributed (p-value:", shapiro_test_als$p.value, ")"))
  }
}

# Function to create and save both histogram and QQ plot for a given dataset and column
plot_and_save <- function(data, col_name, file_path) {
  # Generate histogram
  hist_plot <- ggplot(data, aes_string(x = col_name)) + 
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle(paste("Histogram of", col_name))
  
  # Generate QQ plot
  qq_plot <- ggplot(data, aes_string(sample = col_name)) + 
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    ggtitle(paste("Q-Q Plot of", col_name))
  
  # Arrange both plots in a grid
  g <- grid.arrange(hist_plot, qq_plot, nrow = 1)
  
  # Save the combined plots to the specified file path
  ggsave(filename = paste0(file_path, "/", col_name, "_plots.png"), plot = g, width = 10, height = 5)
}

# Create and save plots for each tongue volume column
for (col in tongue_volume_columns) {
  plot_and_save(df_clean, col, plots_directory)
}

#visualisations
visualize_data <- function(data, col_name, plots_directory) {
  # Check if col_name is a valid column name
  if (!col_name %in% names(data)) {
    stop("The column name provided does not exist in the data frame.")
  }
  
  # Generate violin plots for control and patient groups
  violin_plot <- ggplot(data, aes(x = formal_diagnosis_numeric, y = .data[[col_name]], group = formal_diagnosis_numeric, fill = formal_diagnosis_numeric)) +
    geom_violin(trim = FALSE) + # Draw violin plot
    scale_fill_brewer(palette = "Set1") + # Use a color palette for distinction
    theme_minimal() +
    ggtitle(paste("Violin Plot of", col_name, "by Diagnosis Group")) +
    xlab("Diagnosis Group") +
    ylab("Value") +
    theme(legend.position = "bottom") # Ensure legend is shown for distinction
  
  # Display the plot
  print(violin_plot)
  
  # Save the plot
  plot_filename <- paste0("ViolinPlot_", gsub("\\.", "_", col_name), ".png")
  ggsave(filename = plot_filename, plot = violin_plot, path = plots_directory, width = 10, height = 6, dpi = 300)
}

# Apply the violin plot visualization to each tongue volume column and save them
for (col in tongue_volume_columns) {
  visualize_data(df_clean, col, plots_directory)
}

# Save the cleaned dataframe to an   RDS file for future use
saveRDS(df_clean, file.path(data_dir, "df_clean.rds"))

# Print the final message to the console indicating the script has completed
print("Data cleaning, visualization, and normality testing script has completed. Cleaned data saved as 'df_clean.rds'.")
