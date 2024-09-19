##########################
### Segmentation Results ###
##########################

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(introdataviz)
library(patchwork)
library(car)


#### Outlier analysis
## Checking normality, differences between dataset variables (ANOVA plus t-tests),
## correlations between tongue volumes within the dataset

# Define the data directory and file name
data.dir <- "~/OneDrive - The University of Queensland/Projects/BeLong/clinical_neuropsyc/"
file.name <- "BeLong-aggregate-clin-neuropsyc-demographics_20230918.xlsx"
file.path <- file.path(data.dir, file.name)

# Specify the sheet name and read the data from the Excel file
sheet.name <- "MND.Aggregate"
data <- read_excel(file.path, sheet = sheet.name)

# List of IDs to exclude (these are those with bulbar involvement + seg failures)
exclude_ids <- c(9, 61, 126, 7, 53, 113, 63, 116, 21, 72, 149, 8, 68, 114, 84, 121, 22, 10, 69, 115, 
                 129, 15, 18, 77, 118, 133, 28, 78, 119, 135, 29, 82, 120, 138, 30, 96, 126, 161, 
                 32, 97, 141, 168, 33, 98, 143, 176, 39, 144, 182, 40, 147, 185, 42, 149, 186, 
                 46, 151, 193, 154, 198, 157, 200, 158, 201, 172, 187, 189, 191, 196, 197, 199,
                 90, 117, 6, 179, 43, 44, 92, 51, 109, 38, 89, 67, 194) #low Pt vols

#89 and 67 were outliers for weight
#90, 117, 6, 179, 43, 44, 92, 51, 109, 38 low patient volumes (outliers)
#194 huge outlier (missgementation?)
# Filter out the excluded IDs from the dataset
# Filter data to include only those with a segmentation.id and exclude those in exclude_ids
filtered_data <- data %>%
  filter(!is.na(segmentation.id) & segmentation.id != "" & !(segmentation.id %in% exclude_ids))

# Safely convert bulbar-related columns to numeric, keeping the original NAs and non-numeric values intact
filtered_data <- filtered_data %>%
  mutate(across(c(ALSFRS.R.bulbar, ALSFRS.R.Speech, ALSFRS.R.Swallowing, ALSFRS.R.Saliva), 
                ~suppressWarnings(as.numeric(as.character(.)))))  # Suppress warnings for coercion
# Add a column to classify bulbar severity, ignoring NA values in the classification
filtered_data <- filtered_data %>%
  mutate(bulbar_severity = case_when(
    ALSFRS.R.bulbar <= 6 ~ "Severe Bulbar Involvement",  # Severe if bulbar score is 6 or less
    ALSFRS.R.Swallowing <= 2 | ALSFRS.R.Speech <= 2 ~ "Bulbar Involvement",  # Bulbar symptoms if Swallowing or Speech is 2 or less
    TRUE ~ "Normal"  # All others classified as Normal
  ))

# Filter out anyone with bulbar involvement or severe bulbar involvement, but keep rows with missing bulbar data
filtered_data <- filtered_data %>%
  filter(is.na(bulbar_severity) | bulbar_severity == "Normal")  # Keep missing data as well as "Normal"

# Function to calculate outliers based on IQR
calculate_outliers <- function(numeric_column) {
  Q1 <- quantile(numeric_column, 0.25, na.rm = TRUE)
  Q3 <- quantile(numeric_column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Logical vector indicating whether each value is an outlier
  return(numeric_column < lower_bound | numeric_column > upper_bound)
}

######################################
#
# Define the volume columns of interest
volume_columns <- c('Trans.vol', 'SLong.vol', 'Genio.vol', 'ILong.vol', 'Total.vol')

# Ensure columns are numeric before applying the outlier function
filtered_data <- filtered_data %>%
  mutate_at(volume_columns, as.numeric)

# Create an outlier flag for each volume column
for (column in volume_columns) {
  outlier_flag_col <- paste0(column, "_outlier")
  
  # Flag outliers for each column
  filtered_data[[outlier_flag_col]] <- calculate_outliers(filtered_data[[column]])
}

# Filter out rows where any volume column is flagged as an outlier
filtered_data <- filtered_data %>%
  filter_at(vars(ends_with("_outlier")), all_vars(. == FALSE))

# Optionally, remove the outlier flag columns
filtered_data <- filtered_data %>%
  select(-ends_with("_outlier"))

# Ensure 'Total-vol' and 'IOC-vol-mm3' are numeric
filtered_data <- filtered_data %>%
  mutate_at(c("Total.vol", "IOC.vol.mm3"), as.numeric)

# Calculate normalized volume after removing outliers
filtered_data <- filtered_data %>%
  mutate(normalized_vol = `Total.vol` / `IOC.vol.mm3`)

# Convert all relevant columns to numeric
filtered_data <- filtered_data %>%
  mutate(
    sex.numerical = as.numeric(sex.numerical),
    Total.vol = as.numeric(Total.vol),
    weight = as.numeric(weight),
    height = as.numeric(height)
  )
#########################################################################
#do the descriptives for inclusion/exclusion in segmentation experiment #
#########################################################################

####################################################
## descriptives after outlier removal for table 1 ##
####################################################
# Filter to include only rows where MND.IMAGING.session equals "ses-01"
seg.summary.stat.data <- filtered_data %>%
  filter(MND.IMAGING.session == "ses-01" & !is.na(segmentation.id) & segmentation.id != "NA")

# Ensure relevant columns are numeric before summarizing
seg.summary.stat.data <- seg.summary.stat.data %>%
  mutate(
    age.at.scan = as.numeric(as.character(age.at.scan)),
    height = as.numeric(as.character(height)),
    weight = as.numeric(as.character(weight))
  )

# Group by dataset and calculate the required summary statistics for all data (aggregate across diagnoses)
summary_stats_aggregate <- seg.summary.stat.data %>%
  group_by(dataset) %>%
  summarise(
    Age_mean = mean(age.at.scan, na.rm = TRUE),
    Age_sd = sd(age.at.scan, na.rm = TRUE),
    Height_mean = mean(height, na.rm = TRUE),
    Height_sd = sd(height, na.rm = TRUE),
    Weight_mean = mean(weight, na.rm = TRUE),
    Weight_sd = sd(weight, na.rm = TRUE),
    Male_count = sum(sex.numerical == 0, na.rm = TRUE),  # Count of males
    Female_count = sum(sex.numerical == 1, na.rm = TRUE)  # Count of females
  ) %>%
  arrange(dataset)

# Print the summary statistics with the sex counts for each dataset
print("Summary statistics (age, height, weight, and sex count) for all data aggregated per dataset:")
print(summary_stats_aggregate)






###############################
## Correlations between vars ##
###############################


# Function to print correlation results
print_correlation <- function(test_result, description) {
  cat(description, "\n")
  print(test_result)
}

# Check for correlation between Sex and Total Volume
correlation_result <- cor.test(filtered_data$`sex.numerical`, filtered_data$`Total.vol`, use = "complete.obs")
print_correlation(correlation_result, "Correlation between Sex and Total Volume:")

# Check for correlation between Sex and Normalized Tongue Volume
correlation_normalized <- cor.test(filtered_data$`sex.numerical`, filtered_data$normalized_vol, use = "complete.obs")
print_correlation(correlation_normalized, "Correlation between Sex and Normalized Volume:")

# Correlation between Unnormalized Total Tongue Volume and Weight
correlation_unnorm_weight <- cor.test(filtered_data$`Total.vol`, filtered_data$weight, use = "complete.obs")
print_correlation(correlation_unnorm_weight, "Correlation between Unnormalized Volume and Weight:")

# Correlation between Normalized Total Tongue Volume and Weight
correlation_norm_weight <- cor.test(filtered_data$normalized_vol, filtered_data$weight, use = "complete.obs")
print_correlation(correlation_norm_weight, "Correlation between Normalized Volume and Weight:")

# Correlation between Unnormalized Total Tongue Volume and Height
correlation_unnorm_height <- cor.test(filtered_data$`Total.vol`, filtered_data$height, use = "complete.obs")
print_correlation(correlation_unnorm_height, "Correlation between Unnormalized Volume and Height:")

# Correlation between Normalized Total Tongue Volume and Height
correlation_norm_height <- cor.test(filtered_data$normalized_vol, filtered_data$height, use = "complete.obs")
print_correlation(correlation_norm_height, "Correlation between Normalized Volume and Height:")

#####################################################
###CHECK WHICH MODEL IS BEST FOR PREDICTING VOLUME###
#####################################################
# Assuming 'filtered_data' is your original dataset
non0_data <- filtered_data %>%
  filter(!is.na(sex.numerical) & !is.na(weight) & !is.na(height)) %>%  # Removes NAs
  filter(weight != 0 & height != 0)  # Removes zeros

# Checking for collinearity among weight, height, and sex
model_collinearity_check <- lm(`Total.vol` ~ `sex.numerical` + weight + height, data = non0_data)
vif(model_collinearity_check)  # This will give you the VIF for each predictor

# Model with only sex for Total Volume
model_TotalVol_Sex <- lm(`Total.vol` ~ `sex.numerical`, data = non0_data)
summary(model_TotalVol_Sex)

# Model with only sex for Normalized Volume
model_NormVol_Sex <- lm(normalized_vol ~ `sex.numerical`, data = non0_data)
summary(model_NormVol_Sex)
# Model including weight, height, and sex for Total Volume
model_TotalVol_Whs <- lm(`Total.vol` ~ `sex.numerical` + weight + height, data = non0_data)
summary(model_TotalVol_Whs)

# Model including weight, height, and sex for Normalized Volume
model_NormVol_Whs <- lm(normalized_vol ~ `sex.numerical` + weight + height, data = non0_data)
summary(model_NormVol_Whs)

# Model with IOC volume alone for comparison
model_IOC <- lm(`Total.vol` ~ `IOC.vol.mm3`, data = non0_data)
summary(model_IOC)

#  Scatter plot of IOC.vol.mm3 vs. Total.vol (as before)
pdf("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Scatterplot_IOC_vs_Total_Volume.pdf")
ggplot(filtered_data, aes(x=`IOC.vol.mm3`, y=`Total.vol`, color=`sex.numerical`)) +
  geom_point(size=3, alpha=0.7) +
  labs(title="Scatter plot of IOC volume vs Total Tongue Volume", x="IOC Volume", y="Total Volume") +
  theme_minimal()
dev.off()


# Check for correlation between Total.vol and IOC.vol.mm3
correlation_result_total_IOC <- cor.test(filtered_data$`Total.vol`, filtered_data$`IOC.vol.mm3`)

# Print the result
print(correlation_result_total_IOC)


####ANOVA of tongue muscle and dataset on volume
# Reshape the filtered data to long format for ANOVA analysis
long_format_data <- filtered_data %>%
  pivot_longer(cols = c(`Trans.vol`, `SLong.vol`, `Genio.vol`, `ILong.vol`),
               names_to = "Muscle",
               values_to = "Volume") %>%
  mutate(Muscle = factor(Muscle),
         Dataset = factor(dataset))

# Perform ANOVA on tongue volume
anova_result <- aov(Volume ~ Muscle * dataset, data = long_format_data)
anova_summary <- summary(anova_result)
print(anova_summary)

# Check for significant effects in the ANOVA model
if (anova_summary[[1]]$`Pr(>F)`[1] < 0.05 || 
    anova_summary[[1]]$`Pr(>F)`[2] < 0.05 || 
    anova_summary[[1]]$`Pr(>F)`[3] < 0.05) {
  
  # Perform Tukey's HSD test for post-hoc analysis
  post_hoc_result <- TukeyHSD(anova_result)
  print(post_hoc_result)
} else {
  cat("No significant interaction or main effects found. Tukey HSD test not performed.\n")
}

# Custom colors for muscles and sex (Female and Male)
muscle_colors <- c(
  "Transverse/Vertical_Female" = rgb(141, 211, 199, maxColorValue = 255),
  "Transverse/Vertical_Male"   = rgb(204, 235, 197, maxColorValue = 255),  # Lighter for male
  "Superior Longitudinal_Female" = rgb(251, 154, 153, maxColorValue = 255),
  "Superior Longitudinal_Male"   = rgb(251, 180, 174, maxColorValue = 255),  # Lighter for male
  "Inferior Longitudinal_Female" = rgb(166, 206, 227, maxColorValue = 255),
  "Inferior Longitudinal_Male"   = rgb(179, 205, 227, maxColorValue = 255),  # Lighter for male
  "Genioglossus_Female" = rgb(190, 186, 218, maxColorValue = 255),
  "Genioglossus_Male"   = rgb(222, 203, 228, maxColorValue = 255)  # Lighter for male
)
# Reshape the data for the first (uncorrected) plot
long_format_data_uncorrected <- filtered_data %>%
  pivot_longer(cols = c(Trans.vol, SLong.vol, Genio.vol, ILong.vol),
               names_to = "Muscle", values_to = "Volume") %>%
  mutate(
    Muscle = factor(Muscle, levels = c("Genio.vol", "ILong.vol", "SLong.vol", "Trans.vol")),
    Muscle_Sex = paste0(
      case_when(
        Muscle == "Trans.vol" ~ "Transverse/Vertical",
        Muscle == "SLong.vol" ~ "Superior Longitudinal",
        Muscle == "ILong.vol" ~ "Inferior Longitudinal",
        Muscle == "Genio.vol" ~ "Genioglossus"
      ), "_", ifelse(sex.numerical == 0, "Male", "Female")
    )
  )

# Create the violin plot for the uncorrected volumes
p_combined <- ggplot(long_format_data_uncorrected, aes(x = Muscle, y = Volume, fill = Muscle_Sex)) +
  introdataviz::geom_split_violin(alpha = 0.8, trim = FALSE) +  # Reduced space between violins
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(0.3)) +
  scale_fill_manual(values = muscle_colors) +  # Set custom colors based on muscle and sex
  scale_x_discrete(expand = c(0.05, 0)) +  # Reduce x-axis spacing
  scale_y_continuous(name = "Volume (mm³)") +
  coord_cartesian(clip = "off") +  # To slightly trim any excess whitespace
  labs(title = "Combined Violin Plot of Muscle Volume by Sex and Muscle Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center title
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title to reduce space
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")  # Reduce margin space
  )

# Save the combined plot
ggsave("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Combined_Violin_Sex_Muscle.pdf", 
       plot = p_combined, width = 10, height = 8)

# Print the plot
print(p_combined)

# Custom colors for muscles and sex (Female and Male)
muscle_colors <- c(
  "Transverse/Vertical_Female" = rgb(141, 211, 199, maxColorValue = 255),
  "Transverse/Vertical_Male"   = rgb(204, 235, 197, maxColorValue = 255),  # Lighter for male
  "Superior Longitudinal_Female" = rgb(251, 154, 153, maxColorValue = 255),
  "Superior Longitudinal_Male"   = rgb(251, 180, 174, maxColorValue = 255),  # Lighter for male
  "Inferior Longitudinal_Female" = rgb(166, 206, 227, maxColorValue = 255),
  "Inferior Longitudinal_Male"   = rgb(179, 205, 227, maxColorValue = 255),  # Lighter for male
  "Genioglossus_Female" = rgb(190, 186, 218, maxColorValue = 255),
  "Genioglossus_Male"   = rgb(222, 203, 228, maxColorValue = 255)  # Lighter for male
)

# Step 1: Filter out rows where IOC.vol.mm3 is NA or zero
filtered_data <- filtered_data %>%
  filter(!is.na(IOC.vol.mm3) & IOC.vol.mm3 != 0)

# Step 2: Correct the volumes by IOC
filtered_data <- filtered_data %>%
  mutate(
    Trans.vol.cor.IOC = Trans.vol / IOC.vol.mm3,
    SLong.vol.cor.IOC = SLong.vol / IOC.vol.mm3,
    Genio.vol.cor.IOC = Genio.vol / IOC.vol.mm3,
    ILong.vol.cor.IOC = ILong.vol / IOC.vol.mm3
  )

# Step 3: Reshape the data to a long format for plotting
long_format_data_corrected <- filtered_data %>%
  pivot_longer(cols = c(Trans.vol.cor.IOC, SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC),
               names_to = "Muscle", values_to = "Volume") %>%
  mutate(
    Muscle = factor(Muscle, levels = c("Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "SLong.vol.cor.IOC", "Trans.vol.cor.IOC")),
    Muscle_Sex = paste0(
      case_when(
        Muscle == "Trans.vol.cor.IOC" ~ "Transverse/Vertical",
        Muscle == "SLong.vol.cor.IOC" ~ "Superior Longitudinal",
        Muscle == "ILong.vol.cor.IOC" ~ "Inferior Longitudinal",
        Muscle == "Genio.vol.cor.IOC" ~ "Genioglossus"
      ), "_", ifelse(sex.numerical == 0, "Male", "Female"))
  )

# Step 4: Create a single violin plot for IOC-corrected volumes
p_combined_corrected <- ggplot(long_format_data_corrected, aes(x = Muscle, y = Volume, fill = Muscle_Sex)) +
  introdataviz::geom_split_violin(alpha = 0.8, trim = FALSE) +  # Reduced space between violins
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(0.3)) +
  scale_fill_manual(values = muscle_colors) +  # Set custom colors based on muscle and sex
  scale_x_discrete(expand = c(0.05, 0), 
                   labels = c("Genioglossus", "Inferior Longitudinal", "Superior Longitudinal", "Transverse/Vertical")) +  # Muscle labels
  scale_y_continuous(name = "Volume (corrected by IOC)") +
  coord_cartesian(clip = "off") +  # To slightly trim any excess whitespace
  labs(title = "Combined Violin Plot of IOC-Corrected Muscle Volume by Sex and Muscle Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center title
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title to reduce space
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")  # Reduce margin space
  )

# Save the combined plot with IOC correction
ggsave("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Combined_Violin_IOC_Corrected_Sex_Muscle.pdf", 
       plot = p_combined_corrected, width = 10, height = 8)

# Print the plot
print(p_combined_corrected)

# Calculate the ratio of the corrected volumes to the uncorrected volumes

combined_violin <- p_combined / p_combined_corrected
print(combined_violin)


#### significance bars for the volumes pre/post correction:
##first check if they are normally distributed:
# Check normality for each muscle (pre and post IOC correction independently)

# Perform the Shapiro-Wilk test for pre- and post-IOC volumes independently

# Transverse muscle (pre-IOC and post-IOC)
shapiro_test_trans_pre <- shapiro.test(filtered_data$Trans.vol)
shapiro_test_trans_post <- shapiro.test(filtered_data$Trans.vol.cor.IOC)
cat("Shapiro-Wilk test for Transverse muscle (pre-IOC):\n")
print(shapiro_test_trans_pre)
cat("Shapiro-Wilk test for Transverse muscle (post-IOC):\n")
print(shapiro_test_trans_post)

# Superior Longitudinal muscle (pre-IOC and post-IOC)
shapiro_test_slong_pre <- shapiro.test(filtered_data$SLong.vol)
shapiro_test_slong_post <- shapiro.test(filtered_data$SLong.vol.cor.IOC)
cat("Shapiro-Wilk test for Superior Longitudinal muscle (pre-IOC):\n")
print(shapiro_test_slong_pre)
cat("Shapiro-Wilk test for Superior Longitudinal muscle (post-IOC):\n")
print(shapiro_test_slong_post)

# Genioglossus muscle (pre-IOC and post-IOC)
shapiro_test_genio_pre <- shapiro.test(filtered_data$Genio.vol)
shapiro_test_genio_post <- shapiro.test(filtered_data$Genio.vol.cor.IOC)
cat("Shapiro-Wilk test for Genioglossus muscle (pre-IOC):\n")
print(shapiro_test_genio_pre)
cat("Shapiro-Wilk test for Genioglossus muscle (post-IOC):\n")
print(shapiro_test_genio_post)

# Inferior Longitudinal muscle (pre-IOC and post-IOC)
shapiro_test_ilong_pre <- shapiro.test(filtered_data$ILong.vol)
shapiro_test_ilong_post <- shapiro.test(filtered_data$ILong.vol.cor.IOC)
cat("Shapiro-Wilk test for Inferior Longitudinal muscle (pre-IOC):\n")
print(shapiro_test_ilong_pre)
cat("Shapiro-Wilk test for Inferior Longitudinal muscle (post-IOC):\n")
print(shapiro_test_ilong_post)


# Independent t-test for Transverse muscle (uncorrected) between males and females
t_test_trans_un <- t.test(Trans.vol ~ sex.numerical, data = filtered_data)
print(t_test_trans_un)

# Independent t-test for Transverse muscle (IOC corrected) between males and females
t_test_trans_cor <- t.test(Trans.vol.cor.IOC ~ sex.numerical, data = filtered_data)
print(t_test_trans_cor)

# Independent t-test for Superior Longitudinal muscle (uncorrected) between males and females
t_test_slong_un <- t.test(SLong.vol ~ sex.numerical, data = filtered_data)
print(t_test_slong_un)

# Independent t-test for Superior Longitudinal muscle (IOC corrected) between males and females
t_test_slong_cor <- t.test(SLong.vol.cor.IOC ~ sex.numerical, data = filtered_data)
print(t_test_slong_cor)

# Independent t-test for Genioglossus muscle (uncorrected) between males and females
t_test_genio_un <- t.test(Genio.vol ~ sex.numerical, data = filtered_data)
print(t_test_genio_un)

# Independent t-test for Genioglossus muscle (IOC corrected) between males and females
t_test_genio_cor <- t.test(Genio.vol.cor.IOC ~ sex.numerical, data = filtered_data)
print(t_test_genio_cor)

# Independent t-test for Inferior Longitudinal muscle (uncorrected) between males and females
t_test_ilong_un <- t.test(ILong.vol ~ sex.numerical, data = filtered_data)
print(t_test_ilong_un)

# Independent t-test for Inferior Longitudinal muscle (IOC corrected) between males and females
t_test_ilong_cor <- t.test(ILong.vol.cor.IOC ~ sex.numerical, data = filtered_data)
print(t_test_ilong_cor)



### all  pre / post sig then NS


### do the same thing with the IOC corrected volumes:
####ANOVA of tongue muscle and dataset on volume


# Conduct a two-way ANOVA to investigate the effects of muscle type and dataset on tongue volume
anova_result <- aov(Volume ~ Muscle * dataset, data = long_format_data_corrected)
summary(anova_result)

# Conduct Tukey's HSD test regardless of the ANOVA results
post_hoc_result <- TukeyHSD(anova_result)
print(post_hoc_result)


####
# Check dataset totals
# Calculate overall average volume and SD
overall_avg_volume <- mean(filtered_data$`Total.vol`, na.rm = TRUE)
overall_sd_volume <- sd(filtered_data$`Total.vol`, na.rm = TRUE)

# Calculate average volume and SD per muscle, per dataset
average_volume_per_muscle_dataset <- filtered_data %>%
  pivot_longer(cols = c(`Trans.vol`, `SLong.vol`, `Genio.vol`, `ILong.vol`),
               names_to = "Muscle",
               values_to = "Volume") %>%
  group_by(Muscle, dataset) %>%
  summarise(AverageVolume = mean(Volume, na.rm = TRUE),
            SDVolume = sd(Volume, na.rm = TRUE))

# Print results
print(paste("Overall average volume:", overall_avg_volume, "SD:", overall_sd_volume))
print(average_volume_per_muscle_dataset)


#################################################################
# check if there are differences between controls and patients: #
#################################################################
# Define the muscle volume columns to test
muscle_volumes <- c('Trans.vol.cor.IOC', 'SLong.vol.cor.IOC', 'Genio.vol.cor.IOC', 'ILong.vol.cor.IOC')

# Add a new column for the overall corrected volume by summing all the corrected muscle volumes
filtered_data <- filtered_data %>%
  mutate(Overall_Vol_Corrected = rowSums(across(all_of(muscle_volumes)), na.rm = TRUE))

# Function to perform t-test and print results for each muscle volume independently
perform_t_test_independent <- function(data, column) {
  control_group <- data %>% filter(formal.diagnosis.numeric == '0') %>% pull(column)
  case_group <- data %>% filter(formal.diagnosis.numeric == '1') %>% pull(column)
  
  t_test_result <- t.test(control_group, case_group, na.action = na.exclude)
  cat("\nT-test for", column, ":\n")
  print(t_test_result)
}

# Perform t-tests for each corrected muscle volume independently
for (column in muscle_volumes) {
  perform_t_test_independent(filtered_data, column)
}

# Additional t-test for the overall corrected muscle volume
cat("\nT-test for Overall Corrected Volume:\n")
perform_t_test_independent(filtered_data, "Overall_Vol_Corrected")
###NS between groups

##############################################################
#make sure there are no differences in demos across dataset:
##############################################################

# ANOVA for Age across datasets
anova_age <- aov(age.at.scan ~ dataset, data = filtered_data)
summary(anova_age)
print("ANOVA results for Age across Datasets:")
print(summary(anova_age))

# ANOVA for Weight across datasets
anova_weight <- aov(weight ~ dataset, data = filtered_data)
summary(anova_weight)
print("ANOVA results for Weight across Datasets:")
print(summary(anova_weight))

# ANOVA for Height across datasets
anova_height <- aov(height ~ dataset, data = filtered_data)
summary(anova_height)
print("ANOVA results for Height across Datasets:")
print(summary(anova_height))

# Ensure that 'sex.numerical' is treated as a factor for better interpretation
filtered_data$sex.numerical <- as.factor(filtered_data$sex.numerical)
filtered_data$dataset <- as.factor(filtered_data$dataset)
filtered_data$sex.numerical <- factor(filtered_data$sex.numerical, labels = c("Male", "Female"))

# Create a contingency table of sex by dataset and print it
table_sex_dataset <- table(filtered_data$sex.numerical, filtered_data$dataset)
print("Contingency Table for Sex across Datasets:")
print(table_sex_dataset)

# Check the expected counts to ensure the validity of the chi-squared test
expected_counts <- chisq.test(table_sex_dataset)$expected
print("Expected counts:")
print(expected_counts)

# If expected counts are all greater than 5, we can consider the chi-squared test valid; otherwise, consider Fisher's Exact Test
if (any(expected_counts < 5)) {
  print("Some expected counts are less than 5. Considering Fisher's Exact Test for better accuracy.")
  fisher_test_result <- fisher.test(table_sex_dataset)
  print("Fisher's Exact Test results for Sex across Datasets:")
  print(fisher_test_result)
} else {
  # Perform the Chi-squared test again if all counts are adequate
  chi_sq_test_sex <- chisq.test(table_sex_dataset)
  print("Chi-squared Test results for Sex across Datasets:")
  print(chi_sq_test_sex)
}


#age significant:   
print("Performing Tukey's HSD for Age")
tukey_age <- TukeyHSD(anova_age)
print(tukey_age)


#sydney and eatt have different ages
##################
##    PLOTS.   ##
##################
# Define colors for the plots
#colors <- c("Male" = rgb(27,158,119, maxColorValue = 255), 
#            "Female" = rgb(117,112,179, maxColorValue = 255))

#or orange and purple
colors <- c("Male" = "#e66101", 
            "Female" = "#5e3c99")
  

# Define colors for the violin plots
violin_colors <- c("Sup. Longitudinal" = "#fbb4ae", 
                   "Inf. Longitudinal" = "#b3cde3", 
                   "Transverse" = "#ccebc5", 
                   "Genioglossus" = "#decbe4")

# Define shapes based on the 'dataset' column
shapes <- c("BeLong" = 8, "EATT" = 17, "Sydney" = 16) # Numeric codes for star, triangle, circle

# Reshape the data to long format for plotting
long_data <- filtered_data %>%
  pivot_longer(cols = c(Trans.vol, SLong.vol, Genio.vol, ILong.vol),
               names_to = "Muscle", values_to = "Volume") %>%
  mutate(Muscle = case_when(
    Muscle == "SLong.vol" ~ "Sup. Longitudinal",
    Muscle == "ILong.vol" ~ "Inf. Longitudinal",
    Muscle == "Trans.vol" ~ "Transverse",
    Muscle == "Genio.vol" ~ "Genioglossus"
  ))

# Create the violin plot for each muscle volume
violin_plot <- ggplot(long_data, aes(x = Muscle, y = Volume, fill = Muscle)) +
  geom_violin(trim = FALSE, alpha = 0.9, width = 0.8) +
  geom_jitter(aes(color = sex.numerical, shape = dataset), position = position_jitter(width = 0.4), size = 1.75, alpha = 0.6) +
  scale_fill_manual(values = violin_colors) +
  scale_color_manual(values = colors) +  # Updated to use correct factor levels for color
  scale_shape_manual(values = shapes) +
  labs(title = "Tongue Muscle Volumes by Sex and Dataset",
       x = "Muscle",
       y = "Volume (mm³)",
       fill = "Muscle Volume",
       color = "Sex",
       shape = "Dataset") +
  theme_minimal() +
  theme(legend.position = "right" ,
        legend.title = element_text(size = 14),  # Make legend titles bigger
        legend.text = element_text(size = 12),  # Make legend text bigger
        plot.title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))  # Ensure legend for violin fill is not transparent

# Save the violin plot to a file
ggsave("~/OneDrive - The University of Queensland/Publications/2023_Tongue_seg/figures/violin_plot_colored.png", violin_plot)  # Convert cm to inches for width

# Display the plot
print(violin_plot)

# Create the scatter plot for Total volume vs IOC volume
p1 <- ggplot(filtered_data, aes(x = `IOC.vol.mm3`, y = `Total.vol`, color = sex.numerical)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = colors, labels = c("Male", "Female")) +  # Use the correct labels
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add correlation line
  labs(title = "Total Non-Normalised Tongue Volume vs. IOC Volume", x = "IOC Volume (mm^3)", y = "Total Tongue Volume", color = "Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )
p1


p2 <- ggplot(filtered_data, aes(x=weight, y=`Total.vol`, color=factor(`sex.numerical`))) +
  geom_point(alpha=0.7) +
  scale_color_manual(values=colors, labels=c("0"="Male", "1"="Female")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add correlation line
  labs(title="Non-Normalised Tongue Volume vs. Weight", x="Weight", y="Total Tongue Volume", color="Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14, face="bold"), 
    axis.title.x = element_text(size=14), 
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14)
  )+
  guides(color = guide_none()) 
p2
p3 <- ggplot(filtered_data, aes(x=weight, y=normalized_vol, color=factor(`sex.numerical`))) +
  geom_point(alpha=0.7) +
  scale_color_manual(values=colors, labels=c("0"="Male", "1"="Female")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add correlation line
  labs(title="Normalised Tongue Volume vs. Weight", x="Weight", y="Normalized Tongue Volume", color="Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14, face="bold"), 
    axis.title.x = element_text(size=14), 
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14)
  ) +
  guides(color = guide_none()) 
p3
p6 <- ggplot(filtered_data, aes(x=factor(`sex.numerical`), y=`Total.vol`, fill=factor(`sex.numerical`))) +
  geom_boxplot() +
  scale_fill_manual(values=colors, labels=c("0"="Male", "1"="Female")) +
  labs(title="Non-Normalised Tongue Volume vs. Sex", x="Sex", y="Total Volume (mm³)", fill="Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14, face="bold"), 
    axis.title.x = element_text(size=14), 
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14)
  ) +
  guides(color = guide_none()) 
p6
p7 <- ggplot(filtered_data, aes(x=factor(`sex.numerical`), y=normalized_vol, fill=factor(`sex.numerical`))) +
  geom_boxplot() +
  scale_fill_manual(values=colors, labels=c("0"="Male", "1"="Female")) +
  labs(title="Normalised Tongue Volume vs. Sex", x="Sex", y="Normalized Volume", fill="Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14, face="bold"), 
    axis.title.x = element_text(size=14), 
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14)
  ) +
  guides(color = guide_none()) 
p7
#filter out height
filtered_data <- filtered_data %>%
  filter(!is.na(height) & height > 0)

p4 <- ggplot(filtered_data, aes(x=height, y=`Total.vol`, color=factor(`sex.numerical`))) +
  geom_point(alpha=0.7) +
  scale_color_manual(values=colors, labels=c("0"="Male", "1"="Female")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add correlation line
  labs(title="Non-Normalised Tonogue Volume vs. Height", x="Height (cm)", y="Total Volume (mm³)", color="Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14, face="bold"), 
    axis.title.x = element_text(size=14), 
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14)
  ) +
  guides(color = guide_none()) 
p4
p5 <- ggplot(filtered_data, aes(x=height, y=normalized_vol, color=factor(`sex.numerical`))) +
  geom_point(alpha=0.7) +
  scale_color_manual(values=colors, labels=c("0"="Male", "1"="Female")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add correlation line
  labs(title="Normalised Tongue Volume vs. Height", x="Height (cm)", y="Normalized Volume", color="Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14, face="bold"), 
    axis.title.x = element_text(size=14), 
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14)
  ) +
  guides(color = guide_none()) 
p5
# Arrange the plots into a single figure
#combined_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol=2)
column_plots <- grid.arrange(grobs = list(p2, p3, p6, p7, p4, p5), ncol = 2)

# Combine p1 on top and the two-column layout below
combined_plot <- arrangeGrob(p1, column_plots, ncol = 1, heights = c(1/3, 2/3))

# Save the figure
ggsave("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/combined_plots_exp-3.pdf", combined_plot, width=16, height=12)


