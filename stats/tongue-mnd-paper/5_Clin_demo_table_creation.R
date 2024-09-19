#Clinical table creation - methods/clin paper tongue

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(DescTools)

# Directory to save the plots and load the data
plots_directory <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical"
data_dir <- ("~/OneDrive - The University of Queensland/Projects/BeLong/clinical_neuropsyc/")

# Load the cleaned dataframe from the RDS file
df.clean <- readRDS(file.path(data_dir, "df.clean.rds"))


#################################
##  Stats. for clinical table  ##
#################################
# Define the list of included diagnosis groups
included_diagnoses <- c("ALS", "PLS", "Control", "Flail Limb/PMA")

# Filter the dataset
summary.stat.data <- df.clean %>%
  filter(MND.IMAGING.session == "ses-01" & 
           !is.na(segmentation.id) & 
           segmentation.id != "NA" & 
           !is.na(SLong.vol) & 
           is.numeric(SLong.vol) & 
           formal.diagnosis.numeric %in% included_diagnoses)

# View the filtered data
print(summary.stat.data)

# 1. Count of participants with SLong.vol for each dataset
tongue_vol_count <- summary.stat.data %>%
  group_by(dataset) %>%
  summarise(total_with_tongue_vol = sum(!is.na(SLong.vol)))

print("Total with tongue volumes (SLong.vol) per dataset:")
print(tongue_vol_count)

# 2. Count of ALS, Control, PLS, PMA for each dataset
diagnosis_counts <- summary.stat.data %>%
  group_by(dataset) %>%
  summarise(
    ALS_count = sum(formal.diagnosis.numeric == "ALS", na.rm = TRUE),
    Control_count = sum(formal.diagnosis.numeric == "Control", na.rm = TRUE),
    PLS_count = sum(formal.diagnosis.numeric == "PLS", na.rm = TRUE),
    PMA_count = sum(formal.diagnosis.numeric == "Flail Limb/PMA", na.rm = TRUE)
  )

print("Diagnosis counts (ALS, Control, PLS, PMA) per dataset:")
print(diagnosis_counts)

###########################################
## Clinical demographics table creation  ##
###########################################

# 3. Age, height, weight for ALS, Control, and (PLS + PMA combined) for each dataset
# Ensure that age, height, and weight are numeric, keeping NA for non-numeric values
summary.stat.data <- summary.stat.data %>%
  mutate(
    # Convert columns to numeric, handling non-numeric values as NA
    age.at.scan = suppressWarnings(as.numeric(as.character(age.at.scan))),
    height = suppressWarnings(as.numeric(as.character(height))),
    weight = suppressWarnings(as.numeric(as.character(weight))),
    Trans.vol.cor.IOC = suppressWarnings(as.numeric(as.character(Trans.vol.cor.IOC))),
    Trans.vol = suppressWarnings(as.numeric(as.character(Trans.vol))),
    SLong.vol.cor.IOC = suppressWarnings(as.numeric(as.character(SLong.vol.cor.IOC))),
    SLong.vol = suppressWarnings(as.numeric(as.character(SLong.vol))),
    Genio.vol.cor.IOC = suppressWarnings(as.numeric(as.character(Genio.vol.cor.IOC))),
    Genio.vol = suppressWarnings(as.numeric(as.character(Genio.vol))),
    ILong.vol.cor.IOC = suppressWarnings(as.numeric(as.character(ILong.vol.cor.IOC))),
    ILong.vol = suppressWarnings(as.numeric(as.character(ILong.vol))),
    IOC.vol.mm3 = suppressWarnings(as.numeric(as.character(IOC.vol.mm3))),
    ALSFRS.R.tot = suppressWarnings(as.numeric(as.character(ALSFRS.R.tot))),
    ALSFRS.R.bulbar = suppressWarnings(as.numeric(as.character(ALSFRS.R.bulbar))),
    Onset.location.coded = suppressWarnings(as.numeric(as.character(Onset.location.coded)))
  )

# Check for any non-numeric entries that could not be converted
non_numeric_entries <- summary.stat.data %>%
  summarise(across(everything(), ~sum(is.na(.)), .names = "NA_{col}"))
print(non_numeric_entries)
# Now calculate the summary statistics for age, height, weight, and the new variables
summary.stat.data <- summary.stat.data %>%
  mutate(
    age.at.scan = suppressWarnings(as.numeric(as.character(age.at.scan))),
    height = suppressWarnings(as.numeric(as.character(height))),
    weight = suppressWarnings(as.numeric(as.character(weight))),
    Trans.vol.cor.IOC = suppressWarnings(as.numeric(as.character(Trans.vol.cor.IOC))),
    Trans.vol = suppressWarnings(as.numeric(as.character(Trans.vol))),
    SLong.vol.cor.IOC = suppressWarnings(as.numeric(as.character(SLong.vol.cor.IOC))),
    SLong.vol = suppressWarnings(as.numeric(as.character(SLong.vol))),
    Genio.vol.cor.IOC = suppressWarnings(as.numeric(as.character(Genio.vol.cor.IOC))),
    Genio.vol = suppressWarnings(as.numeric(as.character(Genio.vol))),
    ILong.vol.cor.IOC = suppressWarnings(as.numeric(as.character(ILong.vol.cor.IOC))),
    ILong.vol = suppressWarnings(as.numeric(as.character(ILong.vol))),
    IOC.vol.mm3 = suppressWarnings(as.numeric(as.character(IOC.vol.mm3))),
    ALSFRS.R.tot = suppressWarnings(as.numeric(as.character(ALSFRS.R.tot))),
    ALSFRS.R.bulbar = suppressWarnings(as.numeric(as.character(ALSFRS.R.bulbar))),
    Onset.location.coded = suppressWarnings(as.numeric(as.character(Onset.location.coded)))
  )

# Now calculate the summary statistics for age, height, weight, ALSFRS.R.tot, and ALSFRS.R.bulbar, and count Onset.location.coded
summary_stats <- summary.stat.data %>%
  mutate(
    diagnosis_group = case_when(
      formal.diagnosis.numeric == "Control" ~ "Control",
      formal.diagnosis.numeric == "ALS" ~ "ALS",
      formal.diagnosis.numeric %in% c("PLS", "Flail Limb/PMA") ~ "PLS/PMA",  # Combine PLS and PMA as "Other"
      TRUE ~ NA_character_  # Treat others as NA
    )
  ) %>%
  filter(!is.na(diagnosis_group)) %>%
  group_by(dataset, diagnosis_group) %>%
  summarise(
    Age_mean = mean(age.at.scan, na.rm = TRUE),
    Age_sd = sd(age.at.scan, na.rm = TRUE),
    Height_mean = mean(height, na.rm = TRUE),
    Height_sd = sd(height, na.rm = TRUE),
    Weight_mean = mean(weight, na.rm = TRUE),
    Weight_sd = sd(weight, na.rm = TRUE),
    Male_count = sum(sex.numerical == 0, na.rm = TRUE),  # Count of males
    Female_count = sum(sex.numerical == 1, na.rm = TRUE),  # Count of females
    Trans_vol_cor_IOC_mean = mean(Trans.vol.cor.IOC, na.rm = TRUE),
    Trans_vol_cor_IOC_sd = sd(Trans.vol.cor.IOC, na.rm = TRUE),
    Trans_vol_mean = mean(Trans.vol, na.rm = TRUE),
    Trans_vol_sd = sd(Trans.vol, na.rm = TRUE),
    SLong_vol_cor_IOC_mean = mean(SLong.vol.cor.IOC, na.rm = TRUE),
    SLong_vol_cor_IOC_sd = sd(SLong.vol.cor.IOC, na.rm = TRUE),
    SLong_vol_mean = mean(SLong.vol, na.rm = TRUE),
    SLong_vol_sd = sd(SLong.vol, na.rm = TRUE),
    Genio_vol_cor_IOC_mean = mean(Genio.vol.cor.IOC, na.rm = TRUE),
    Genio_vol_cor_IOC_sd = sd(Genio.vol.cor.IOC, na.rm = TRUE),
    Genio_vol_mean = mean(Genio.vol, na.rm = TRUE),
    Genio_vol_sd = sd(Genio.vol, na.rm = TRUE),
    ILong_vol_cor_IOC_mean = mean(ILong.vol.cor.IOC, na.rm = TRUE),
    ILong_vol_cor_IOC_sd = sd(ILong.vol.cor.IOC, na.rm = TRUE),
    ILong_vol_mean = mean(ILong.vol, na.rm = TRUE),
    ILong_vol_sd = sd(ILong.vol, na.rm = TRUE),
    IOC_vol_mm3_mean = mean(IOC.vol.mm3, na.rm = TRUE),
    IOC_vol_mm3_sd = sd(IOC.vol.mm3, na.rm = TRUE),
    ALSFRS.R_tot_mean = mean(ALSFRS.R.tot, na.rm = TRUE),
    ALSFRS.R_tot_sd = sd(ALSFRS.R.tot, na.rm = TRUE),
    ALSFRS.R_bulbar_mean = mean(ALSFRS.R.bulbar, na.rm = TRUE),
    ALSFRS.R_bulbar_sd = sd(ALSFRS.R.bulbar, na.rm = TRUE),
    Onset_location_count = sum(Onset.location.coded == 1, na.rm = TRUE)  # Count of Onset.location.coded == 1
  ) %>%
  arrange(dataset, diagnosis_group)
# Print the summary statistics with the sex counts at the bottom
print("Summary statistics (age, height, weight, and sex count, tongue vols, alsfrsr) for ALS, Control, PLS/PMA combined per dataset:")
print(summary_stats)

#################################################
## Function to print out table for publication ##
#################################################
reshape_summary_table <- function(summary_stats) {
  # Create a new column that combines dataset and diagnosis_group
  summary_stats <- summary_stats %>%
    mutate(group_combined = paste(dataset, diagnosis_group, sep = "."))
  
  # Define the unique combined groups
  group_combinations <- unique(summary_stats$group_combined)
  
  # Create an empty list to store each group's statistics
  final_table_list <- list()
  
  # Loop over each combined group
  for (group in group_combinations) {
    # Extract relevant statistics for this group
    stats <- summary_stats %>%
      filter(group_combined == group)
    
    # Handle cases where there's no data for this group (this is just a safeguard)
    if (nrow(stats) == 0) {
      final_table_list[[length(final_table_list) + 1]] <- rep(NA, 15)
    } else {
      # Create the formatted string with means and SDs in parentheses for each variable
      row <- c(
        paste0(round(stats$Age_mean, 2), " (", round(stats$Age_sd, 2), ")"),
        paste0(round(stats$Height_mean, 2), " (", round(stats$Height_sd, 2), ")"),
        paste0(round(stats$Weight_mean, 2), " (", round(stats$Weight_sd, 2), ")"),
        paste0(round(stats$Trans_vol_cor_IOC_mean, 3), " (", round(stats$Trans_vol_cor_IOC_sd, 3), ")"),
        paste0(round(stats$Trans_vol_mean, 2), " (", round(stats$Trans_vol_sd, 2), ")"),
        paste0(round(stats$SLong_vol_cor_IOC_mean, 3), " (", round(stats$SLong_vol_cor_IOC_sd, 3), ")"),
        paste0(round(stats$SLong_vol_mean, 2), " (", round(stats$SLong_vol_sd, 2), ")"),
        paste0(round(stats$Genio_vol_cor_IOC_mean, 3), " (", round(stats$Genio_vol_cor_IOC_sd, 3), ")"),
        paste0(round(stats$Genio_vol_mean, 2), " (", round(stats$Genio_vol_sd, 2), ")"),
        paste0(round(stats$ILong_vol_cor_IOC_mean, 3), " (", round(stats$ILong_vol_cor_IOC_sd, 3), ")"),
        paste0(round(stats$ILong_vol_mean, 2), " (", round(stats$ILong_vol_sd, 2), ")"),
        paste0(round(stats$IOC_vol_mm3_mean, 2), " (", round(stats$IOC_vol_mm3_sd, 2), ")"),
        paste0(round(stats$ALSFRS.R_tot_mean, 2), " (", round(stats$ALSFRS.R_tot_sd, 2), ")"),
        paste0(round(stats$ALSFRS.R_bulbar_mean, 2), " (", round(stats$ALSFRS.R_bulbar_sd, 2), ")"),
        stats$Onset_location_count
      )
      final_table_list[[length(final_table_list) + 1]] <- row
    }
  }
  
  # Combine all rows for each group
  final_table <- do.call(cbind, final_table_list)
  
  # Set the row names (15 expected rows)
  rownames(final_table) <- c(
    "Age years (SD)", "Height cm (SD)", "Weight Kg (SD)", "Transverse/Vertical volume IOC Corrected mm^3 (SD)","Transverse/Vertical volume mm^3 (SD)",
    "Superior longitudinal volume IOC corrected mm^3 (SD)", "Superior longitudinal volume mm^3 (SD)", "Genioglossus volume IOC corrected mm^3 (SD)", 
    "Genioglossus volume mm^3 (SD)", "Inferior longitudinal volume IOC corrected mm^3 (SD)", "Inferior longitudinal volume mm^3 (SD)",
    "Intra-oral cavity volume mm^3 (SD)", "ALSFRS-R total (SD)", "ALSFRS-R bulbar (SD)", "Bulbar onset count"
  )
  
  # Set the column names as the unique group combinations
  colnames(final_table) <- group_combinations
  
  return(final_table)
}
# Example usage of the function:
final_table <- reshape_summary_table(summary_stats)

# Load the writexl package
library(writexl)

# Convert final_table to a data frame and add row names as a column
final_table_df <- as.data.frame(final_table)
final_table_df <- cbind(Row_Names = rownames(final_table_df), final_table_df)

# Define the file path
file_path <- "/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Publications/2024_Tongue_Radiology/final_table_with_row_names.xlsx"

# Write the final_table_df to the Excel file
write_xlsx(final_table_df, file_path)
#####################

# 4. Perform ANOVA for age.at.scanbetween datasets
perform_anovas <- function(summary_stat_data) {
  # Combine dataset and diagnosis_group into a single grouping variable
  summary_stat_data <- summary_stat_data %>%
    mutate(group_combined = paste(dataset, diagnosis_group, sep = "."))
  
  # List of numeric variables to perform ANOVA on
  variables_to_analyze <- c("age.at.scan", "height", "weight", 
                            "Trans.vol.cor.IOC", "Trans.vol", "SLong.vol.cor.IOC", 
                            "SLong.vol", "Genio.vol.cor.IOC", "Genio.vol", 
                            "ILong.vol.cor.IOC", "ILong.vol", "IOC.vol.mm3", 
                            "ALSFRS.R.tot", "ALSFRS.R.bulbar")
  
  # Create an empty list to store ANOVA results
  anova_results <- list()
  
  # Loop over each variable to perform ANOVA
  for (variable in variables_to_analyze) {
    formula <- as.formula(paste(variable, "~ group_combined"))
    
    # Perform ANOVA
    anova_result <- aov(formula, data = summary_stat_data)
    
    # Store the ANOVA summary in the list
    anova_results[[variable]] <- summary(anova_result)
    
    # Print the results for the current variable
    cat("\nANOVA results for", variable, ":\n")
    print(summary(anova_result))
  }
  
  return(anova_results)
}

# Example usage
#anova_results <- perform_anovas(summary.stat.data)
