library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(DescTools)
library(readxl)

# Define the directory to save the plots
plots.directory <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical"

# Define the data directory and file name
data.dir <- "~/OneDrive - The University of Queensland/Projects/BeLong/clinical_neuropsyc/"
file.name <- "BeLong-aggregate-clin-neuropsyc-demographics_20230918.xlsx"
file.path <- file.path(data.dir, file.name)

# Specify the sheet name
sheet.name <- "MND.Aggregate"

# Read the specific sheet from the Excel file
df <- read_excel(file.path, sheet = sheet.name)

# Step 2: Rename columns to be more R-friendly (avoid spaces and special characters)
colnames(df) <- make.names(colnames(df))

# List of IDs to exclude due to poor seg or missing ALSFRS-R (NB this is different from seg paper)
exclude.ids <- c(9, 61, 126, 7, 63, 116, 21, 72, 149, 84, 121, 22, 129, 15, 133, 135,
                 138, 161, 168, 176, 182, 185, 186, 193, 198, 200, 201, 103, 157)

# Filter out the data
df <- df %>% 
  filter(!(`segmentation.id` %in% exclude.ids))
#remove any that aren;t session one
df <- df %>% filter(MND.IMAGING.session == 'ses-01')

# Convert columns to numeric if they are not already
df$Trans.vol <- as.numeric(as.character(df$Trans.vol))
df$SLong.vol <- as.numeric(as.character(df$SLong.vol))
df$Genio.vol <- as.numeric(as.character(df$Genio.vol))
df$ILong.vol <- as.numeric(as.character(df$ILong.vol))
df$IOC.vol.mm3 <- as.numeric(as.character(df$IOC.vol.mm3))
df$total.vol.cor <- as.numeric(as.character(df$total.vol.cor))
# Check for NA values after conversion
sum(is.na(df$Trans.vol))
sum(is.na(df$SLong.vol))
sum(is.na(df$Genio.vol))
sum(is.na(df$ILong.vol))
sum(is.na(df$IOC.vol.mm3))
# Check for NA values after conversion
sum(is.na(df$Trans.vol))
sum(is.na(df$SLong.vol))
sum(is.na(df$Genio.vol))
sum(is.na(df$ILong.vol))
sum(is.na(df$IOC.vol.mm3))
# Calculate corrected volumes
df$Trans.vol.cor.IOC <- df$Trans.vol / df$IOC.vol.mm3
df$SLong.vol.cor.IOC <- df$SLong.vol / df$IOC.vol.mm3
df$Genio.vol.cor.IOC <- df$Genio.vol / df$IOC.vol.mm3
df$ILong.vol.cor.IOC <- df$ILong.vol / df$IOC.vol.mm3

# View summaries of corrected volumes
summary(df$Trans.vol.cor.IOC)
summary(df$SLong.vol.cor.IOC)
summary(df$Genio.vol.cor.IOC)
summary(df$ILong.vol.cor.IOC)
summary(df$total.vol.cor)
# Define the valid levels
valid.levels <- c(0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15)

# Replace unexpected values with NA
df$formal.diagnosis.numeric <- ifelse(df$formal.diagnosis.numeric %in% valid.levels,
                                      df$formal.diagnosis.numeric, NA)

# Recode the column to a factor with descriptive labels
df$formal.diagnosis.numeric <- factor(
  df$formal.diagnosis.numeric,
  levels = valid.levels, 
  labels = c("Control", "ALS", "PLS", "Flail Limb/PMA", "FTD", "MMN", "PNFA", "PSP", "Asympt", "PPA", "HSP", "CMT", "Kennedy", "NotMND")
)

# Verify the results after recoding
print(table(df$formal.diagnosis.numeric, useNA = "ifany"))

# Define which columns represent tongue volume
tongue.volume.columns <- c("Trans.vol.cor.IOC", "SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "total.vol.cor")

# Function to winsorize a vector at the 1st and 99th percentiles
winsorize.at.percentiles <- function(x, lower.perc = 0.01, upper.perc = 0.99) {
  lower.bound <- quantile(x, lower.perc, na.rm = TRUE)
  upper.bound <- quantile(x, upper.perc, na.rm = TRUE)
  Winsorize(x, probs = c(lower.perc, upper.perc), na.rm = TRUE)
}

# Apply the winsorize function to data for each tongue volume column
controls <- df %>% 
  filter(formal.diagnosis.numeric == "Control") %>%
  mutate(across(all_of(tongue.volume.columns), ~winsorize.at.percentiles(.)))
# Apply the winsorize function to data for each tongue volume column
patients <- df %>% 
  filter(formal.diagnosis.numeric != "Control") %>%
  mutate(across(all_of(tongue.volume.columns), ~winsorize.at.percentiles(.)))

# Combine the cleaned data from controls and patients
df.clean <- bind_rows(controls, patients)

# Normality checks for the cleaned tongue volume data
normality.results <- list()
for (col in tongue.volume.columns) {
  column.data <- df.clean[[col]][!is.na(df.clean[[col]])]
  shapiro.test <- shapiro.test(column.data)
  normality.results[[col]] <- shapiro.test$p.value
  
  # Interpretation of the Shapiro-Wilk test results
  if(shapiro.test$p.value > 0.05) {
    print(paste(col, "appears to be normally distributed (p-value:", shapiro.test$p.value, ")"))
  } else {
    print(paste(col, "does not appear to be normally distributed (p-value:", shapiro.test$p.value, ")"))
  }
}

# Perform independent normality checks for controls and ALS patients
normality.results.controls <- list()
normality.results.als <- list()

for (col in tongue.volume.columns) {
  # Controls
  column.data.controls <- controls[[col]][!is.na(controls[[col]])]
  shapiro.test.controls <- shapiro.test(column.data.controls)
  normality.results.controls[[col]] <- shapiro.test.controls$p.value
  
  if(shapiro.test.controls$p.value > 0.05) {
    print(paste(col, "in controls appears to be normally distributed (p-value:", shapiro.test.controls$p.value, ")"))
  } else {
    print(paste(col, "in controls does not appear to be normally distributed (p-value:", shapiro.test.controls$p.value, ")"))
  }
  
  # ALS Patients
  column.data.als <- patients[[col]][!is.na(patients[[col]]) & patients$formal.diagnosis.numeric == "ALS"]
  shapiro.test.als <- shapiro.test(column.data.als)
  normality.results.als[[col]] <- shapiro.test.als$p.value
  
  if(shapiro.test.als$p.value > 0.05) {
    print(paste(col, "in ALS patients appears to be normally distributed (p-value:", shapiro.test.als$p.value, ")"))
  } else {
    print(paste(col, "in ALS patients does not appear to be normally distributed (p-value:", shapiro.test.als$p.value, ")"))
  }
}

# Function to create and save both histogram and QQ plot for a given dataset and column
plot.and.save <- function(data, col.name, file.path) {
  # Generate histogram
  hist.plot <- ggplot(data, aes_string(x = col.name)) + 
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle(paste("Histogram of", col.name))
  
  # Generate QQ plot
  qq.plot <- ggplot(data, aes_string(sample = col.name)) + 
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    ggtitle(paste("Q-Q Plot of", col.name))
  
  # Arrange both plots in a grid
  g <- grid.arrange(hist.plot, qq.plot, nrow = 1)
  
  # Save the combined plots to the specified file path
  ggsave(filename = paste0(file.path, "/", col.name, ".plots.png"), plot = g, width = 10, height = 5)
}

# Create and save plots for each tongue volume column
for (col in tongue.volume.columns) {
  plot.and.save(df.clean, col, plots.directory)
}

# Visualizations
visualize.data <- function(data, col.name, plots.directory) {
  # Check if col.name is a valid column name
  if (!col.name %in% names(data)) {
    stop("The column name provided does not exist in the data frame.")
  }
  
  # Define the groups to include
  included_groups <- c("Control", "ALS", "PLS", "Flail Limb/PMA")
  
  # Filter the data for specified groups
  filtered_data <- data %>%
    filter(formal.diagnosis.numeric %in% included_groups)
  
  # Generate violin plots for the specified diagnosis groups
  violin.plot <- ggplot(filtered_data, aes(x = formal.diagnosis.numeric, y = .data[[col.name]], group = formal.diagnosis.numeric, fill = formal.diagnosis.numeric)) +
    geom_violin(trim = FALSE) + # Draw violin plot
    scale_fill_manual(values = c("Control" = "#b3e2cd", "ALS" = "#fdcdac", "PLS" = "#cbd5e8", "Flail Limb/PMA" = "#f4cae4")) + 
    theme_minimal() +
    ggtitle(paste("Violin Plot of", col.name, "by Diagnosis Group")) +
    xlab("Diagnosis Group") +
    ylab("Value") +
    theme(legend.position = "bottom") # Ensure legend is shown for distinction
  

  # Display the plot
  print(violin.plot)
  
  # Save the plot
  plot.filename <- paste0("ViolinPlot.", gsub("\\.", "_", col.name), ".png")
  ggsave(filename = plot.filename, plot = violin.plot, path = plots.directory, width = 10, height = 6, dpi = 300)
}

# Apply the violin plot visualization to each tongue volume column and save them
for (col in tongue.volume.columns) {
  visualize.data(df.clean, col, plots.directory)
}

# Save the cleaned dataframe to an   RDS file for future use
saveRDS(df.clean, file.path(data.dir, "df.clean.rds"))

# Print the final message to the console indicating the script has completed
print("Data cleaning, visualization, and normality testing script has completed. Cleaned data saved as 'df.clean.rds'.")
