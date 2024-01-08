
# Load necessary libraries
#install.packages(c("readxl", "dplyr", "ggplot2", "gridExtra"))
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Read in the spreadsheet
data <- read_excel("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/main-tongue-data-spreadsheet.xlsx")
###dataset descriptions
library(tidyr)


####

# Overall summary statistics with age
overall_summary <- data %>% 
  summarise(
    Total_Records = n(),
    Total_Patients = n_distinct(`tongue-id`),
    Number_of_Sessions = sum(!is.na(session)),
    Male = sum(`sex-numerical` == 0, na.rm = TRUE),
    Female = sum(`sex-numerical` == 1, na.rm = TRUE),
    Controls = sum(formal_diagnosis_numeric == 0, na.rm = TRUE),
    Patients = sum(formal_diagnosis_numeric != 0, na.rm = TRUE),
    Mean_Age = mean(age_at_scan, na.rm = TRUE),
    Median_Age = median(age_at_scan, na.rm = TRUE),
    Min_Age = min(age_at_scan, na.rm = TRUE),
    Max_Age = max(age_at_scan, na.rm = TRUE)
  )

# Dataset-wise summary statistics with age
dataset_summary <- data %>% 
  group_by(dataset) %>% 
  summarise(
    Total_Records = n(),
    Total_Patients = n_distinct(`tongue-id`),
    Number_of_Sessions = sum(!is.na(session)),
    Male = sum(`sex-numerical` == 0, na.rm = TRUE),
    Female = sum(`sex-numerical` == 1, na.rm = TRUE),
    Controls = sum(formal_diagnosis_numeric == 0, na.rm = TRUE),
    Patients = sum(formal_diagnosis_numeric != 0, na.rm = TRUE),
    Mean_Age = mean(age_at_scan, na.rm = TRUE),
    Median_Age = median(age_at_scan, na.rm = TRUE),
    Min_Age = min(age_at_scan, na.rm = TRUE),
    Max_Age = max(age_at_scan, na.rm = TRUE)
  ) %>% ungroup()

# Save the overall summary to PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Overall_Summary.png", width=2800, height=700, units="px", res=100)
grid.table(overall_summary)
dev.off()

# Save the dataset-wise summary to PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Dataset_Summary.png", width=2800, height=700, units="px", res=100)
grid.table(dataset_summary)
dev.off()

# 1. Bar plot of the number of males and females
male_female_plot <- ggplot(data, aes(x=as.factor(`sex-numerical`))) + 
  geom_bar(aes(fill=as.factor(`sex-numerical`))) +
  scale_x_discrete(labels=c("0"="Male", "1"="Female")) +
  scale_fill_manual(values=c("darkblue", "darkred"), name="Sex") +
  labs(title="Distribution of Males and Females", x="Sex", y="Count") +
  theme_minimal()

# Adjusted Bar plot of the number of patients vs controls
patient_control_plot <- ggplot(data, aes(x=as.factor(formal_diagnosis_numeric))) + 
  geom_bar(aes(fill=as.factor(formal_diagnosis_numeric))) +
  scale_x_discrete(labels=c("0"="Control", "1"="ALS", "2"="PLS", "3"="PBP", "4"="Flail limb/PMA", "6"="Mimic", "7"="Mimic", "8"="Mimic", "9"="Mimic")) +
  scale_fill_manual(values=c("#FF9999", "#66B2FF", "#99E699", "#FFCC99", "#C2C2F0", "#FFD700", "#C71585", "#32CD32", "#8A2BE2"),
                    name="Diagnosis Type",
                    labels=c("Control", "ALS", "PLS", "PBP", "Flail limb/PMA", "Mimic", "Mimic", "Mimic", "Mimic")) +
  labs(title="Distribution of Diagnosis Type", x="Diagnosis", y="Count") +
  theme_minimal()

# Save the adjusted plot to PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Adjusted_Patient_Control_Distribution.png", width=1400, height=700, units="px", res=100)
print(patient_control_plot)
dev.off()

# 3. Bar plot of records per dataset
dataset_plot <- ggplot(data, aes(x=dataset)) + 
  geom_bar(aes(fill=dataset)) +
  labs(title="Distribution of Records per Dataset", x="Dataset", y="Count") +
  theme_minimal()

# 4. Histogram of age
age_plot <- ggplot(data, aes(x=age_at_scan)) + 
  geom_histogram(binwidth=5, fill="lightblue", color="darkblue") +
  labs(title="Distribution of Age", x="Age", y="Count") +
  theme_minimal()

# Save plots to PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Male_Female_Distribution.png", width=1400, height=700, units="px", res=100)
print(male_female_plot)
dev.off()

png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Dataset_Distribution.png", width=1400, height=700, units="px", res=100)
print(dataset_plot)
dev.off()

png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Age_Distribution.png", width=1400, height=700, units="px", res=100)
print(age_plot)
dev.off()

# Bar plot of the number of patients with bulbar vs non-bulbar onset
bulbar_plot <- ggplot(data, aes(x=onset_type)) + 
  geom_bar(aes(fill=onset_type)) +
  scale_fill_manual(values=c("bulbar"="#FF9999", "non-bulbar"="#66B2FF"),
                    name="Onset Type",
                    labels=c("bulbar"="Bulbar", "non-bulbar"="Non-Bulbar")) +
  labs(title="Distribution of Bulbar vs Non-Bulbar Onset", x="Onset Type", y="Count") +
  theme_minimal()

# Save the plot to PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Bulbar_vs_NonBulbar_Distribution.png", width=1400, height=700, units="px", res=100)
print(bulbar_plot)
dev.off()

# Exclude controls and transform the ALSFRS-R-bulbar scores into factor groups
data_non_control <- data %>% filter(!is.na(`ALSFRS-R-bulbar`))

data_non_control$score_group <- cut(data_non_control$`ALSFRS-R-bulbar`, 
                                    breaks=c(-Inf, 4.5, 8.5, 11.5, Inf), 
                                    labels=c("0-4", "5-8", "9-11", "12"))

# Bar plot of the ALSFRS-R-bulbar score distribution
score_plot <- ggplot(data_non_control, aes(x=score_group)) + 
  geom_bar(aes(fill=score_group)) +
  scale_fill_manual(values=c("0-4"="#FF9999", "5-8"="#FFCC99", "9-11"="#FFFF99", "12"="#99E699"),
                    name="Score Range") +
  labs(title="Distribution of ALSFRS-R-bulbar Scores", x="Score Range", y="Count") +
  theme_minimal()

# Save the plot to PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/ALSFRS_R_Bulbar_Score_Distribution.png", width=1400, height=700, units="px", res=100)
print(score_plot)
dev.off()


##########################
###Segmentation results###
##########################

library(ggplot2)
library(dplyr)
library(readxl)

# Filter for controls
control_data <- data %>% filter(formal_diagnosis_numeric == 0)

# Gather the volume data for a tidy format
control_data_melted <- control_data %>% 
  select(`Trans-vol`, `SLong-vol`, `Genio-vol`, `ILong-vol`) %>%
  gather(key="Volume_Type", value="Volume")

# Generate the violin plot
plot <- ggplot(control_data_melted, aes(x=Volume_Type, y=Volume, fill=Volume_Type)) +
  geom_violin(width=0.8) +
  labs(title="Distribution of Tongue Volumes for Control Participants", 
       x="Tongue Volume Type", 
       y="Volume") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

# Save the plot as PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Tongue_Volumes_Control_ViolinPlot.png", width=1200, height=800, units="px", res=100)
print(plot)
dev.off()

####TONGUE NORMALISATION


# Filter to only controls
control_data <- data %>% filter(formal_diagnosis_numeric == 0)

# 1. Check for correlation and print to console
correlation_result <- cor.test(control_data$`sex-numerical`, control_data$`Total-vol`)
print(correlation_result)

# Calculate normalized tongue volume
data$normalized_vol <- data$`Total-vol` / data$`IOC-vol-mm3`

# Check for correlation between sex and normalized tongue volume
correlation_normalized <- cor.test(data$`sex-numerical`, data$normalized_vol)

print(correlation_normalized)


#### WEIGHT AND HEIGHT

# Filter to only controls
control_data <- data %>% filter(formal_diagnosis_numeric == 0)

# 1. Correlation between unnormalized total tongue volume and weight
correlation_unnorm_weight <- cor.test(control_data$`Total-vol`, control_data$weight)
print("Correlation between Unnormalized Volume and Weight:")
print(correlation_unnorm_weight)

# 2. Correlation between unnormalized total tongue volume and height
correlation_unnorm_height <- cor.test(control_data$`Total-vol`, control_data$height)
print("\nCorrelation between Unnormalized Volume and Height:")
print(correlation_unnorm_height)

# 3. Correlation between normalized total tongue volume and weight
correlation_norm_weight <- cor.test(control_data$normalized_vol, control_data$weight)
print("\nCorrelation between Normalized Volume and Weight:")
print(correlation_norm_weight)

# 4. Correlation between normalized total tongue volume and height
correlation_norm_height <- cor.test(control_data$normalized_vol, control_data$height)
print("\nCorrelation between Normalized Volume and Height:")
print(correlation_norm_height)


##### by SEX
# 2. Scatterplot of normalized tongue volume by sex
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Normalized_Tongue_Volume_by_Sex.png", width=1200, height=700, units="px", res=100)
ggplot(control_data, aes(x=as.factor(`sex-numerical`), y=`total-vol-cor`, color=as.factor(`sex-numerical`))) +
  geom_point(size=3, alpha=0.7) +
  scale_color_manual(values=c("0"="blue", "1"="red")) +
  labs(title="Normalised Tongue Volume by Sex in Controls", x="Sex (0 for men, 1 for women)", y="Total corrected Tongue Volume") +
  theme_minimal() +
  theme(legend.position="none")
dev.off()
# Unnormalised volume by sex

png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/UnNormalized_Tongue_Volume_by_Sex.png", width=1200, height=700, units="px", res=100)
ggplot(control_data, aes(x=as.factor(`sex-numerical`), y=`Total-vol`, color=as.factor(`sex-numerical`))) +
  geom_point(size=3, alpha=0.7) +
  scale_color_manual(values=c("0"="blue", "1"="red")) +
  labs(title="Raw Tongue Volume by Sex in Controls", x="Sex (0 for men, 1 for women)", y="Total corrected Tongue Volume") +
  theme_minimal() +
  theme(legend.position="none")
dev.off()


# 3. Basic Volume Statistics
volume_columns <- c('Trans-vol-cor-tongue', 'SLong-vol', 'SLong-vol-cor-IOC', 'SLong-vol-cor-tongue', 'Genio-vol', 
                    'Genio-vol-cor-IOC', 'Genio-vol-cor-tongue', 'ILong-vol', 'ILong-vol-cor-IOC', 'ILong-vol-cor-tongue',
                    'IOC-vol-mm3', 'Total-vol', 'total-vol-cor')
stats <- data %>% summarise(across(all_of(volume_columns), list(mean = mean, median = median, sd = sd, min = min, max = max)))
print(stats)

# Save the statistics to a PDF
pdf("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Volume_Statistics.pdf")
grid.table(stats)
dev.off()

# 4. Histogram for Total-vol
pdf("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Histogram_Total_Volume.pdf")
ggplot(data, aes(x=`Total-vol`)) + 
  geom_histogram(binwidth=10) +
  labs(title="Distribution of Total Tongue Volume", x="Total Volume", y="Count") +
  theme_minimal()
dev.off()

# 5. Scatter plot for IOC vs Total-vol using the correct column name
# Define outliers for IOC-vol-mm3 with na.rm = TRUE
Q1_IOC <- quantile(data$`IOC-vol-mm3`, 0.25, na.rm = TRUE)
Q3_IOC <- quantile(data$`IOC-vol-mm3`, 0.75, na.rm = TRUE)
IQR_IOC <- Q3_IOC - Q1_IOC
outliers_IOC <- (data$`IOC-vol-mm3` < (Q1_IOC - 1.5 * IQR_IOC)) | (data$`IOC-vol-mm3` > (Q3_IOC + 1.5 * IQR_IOC))

# Define outliers for Total-vol with na.rm = TRUE
Q1_Total <- quantile(data$`Total-vol`, 0.25, na.rm = TRUE)
Q3_Total <- quantile(data$`Total-vol`, 0.75, na.rm = TRUE)
IQR_Total <- Q3_Total - Q1_Total
outliers_Total <- (data$`Total-vol` < (Q1_Total - 1.5 * IQR_Total)) | (data$`Total-vol` > (Q3_Total + 1.5 * IQR_Total))

# Filter out outliers
data_filtered <- data %>% filter(!outliers_IOC & !outliers_Total)

# 1. Scatter plot of IOC-vol-mm3 vs. Total-vol (as before)
pdf("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Scatterplot_IOC_vs_Total_Volume.pdf")
ggplot(data_filtered, aes(x=`IOC-vol-mm3`, y=`Total-vol`, color=`sex-numerical`)) +
  geom_point(size=3, alpha=0.7) +
  labs(title="Scatter plot of IOC volume vs Total Tongue Volume", x="IOC Volume", y="Total Volume") +
  theme_minimal()
dev.off()

# Create a new column for combined sex and volume type
data_filtered <- data_filtered %>%
  mutate(volume_sex_IOC = ifelse(`sex-numerical` == 0, "Male IOC", "Female IOC"),
         volume_sex_Total = ifelse(`sex-numerical` == 0, "Male Total Volume", "Female Total Volume"))


# Plot with custom colors and adjusted width
pdf("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Scatterplot_TongueID_vs_Volumes_Ordered_Colored.pdf", width=12, height=7)
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Scatterplot_TongueID_vs_Volumes_Ordered_Colored.png", width=1200, height=700, units="px", res=100)
ggplot(data_filtered, aes(x=tongue_id_ordered, y=`Total-vol`)) +
  geom_point(aes(color=volume_sex_Total), size=3) +
  geom_point(aes(y=`IOC-vol-mm3`, color=volume_sex_IOC), size=3) +
  scale_color_manual(values=c("Male IOC"="darkred", "Female IOC"="lightcoral", "Male Total Volume"="darkblue", "Female Total Volume"="lightblue")) +
  labs(title="Volume by Ordered Tongue ID", x="Tongue ID", y="Volume", color="Volume & Sex Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()
# Check for correlation between Total-vol and IOC-vol-mm3
correlation_result_total_IOC <- cor.test(data_filtered$`Total-vol`, data_filtered$`IOC-vol-mm3`)

# Print the result
print(correlation_result_total_IOC)

# Create columns for combined sex and volume type
control_data_ordered <- control_data_ordered %>%
  mutate(volume_sex_IOC = ifelse(`sex-numerical` == 0, "Male IOC", "Female IOC"),
         volume_sex_Total = ifelse(`sex-numerical` == 0, "Male Total Volume", "Female Total Volume"))

# Function to generate the plot
plot_graph <- function() {
  ggplot(control_data_ordered, aes(x=tongue_id_ordered, y=`Total-vol`)) +
    geom_point(aes(color=volume_sex_Total), size=3) +
    geom_point(aes(y=`IOC-vol-mm3`, color=volume_sex_IOC), size=3) +
    scale_color_manual(values=c("Male IOC"="darkred", "Female IOC"="lightcoral", "Male Total Volume"="darkblue", "Female Total Volume"="lightblue")) +
    labs(title="Volume by Ordered Tongue ID (Controls Only)", x="Tongue ID", y="Volume", color="Volume & Sex Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# Saving as PDF
pdf("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Scatterplot_TongueID_vs_Volumes_Controls_Ordered_Colored.pdf", width=10, height=7)
plot_graph()
dev.off()

# Saving as PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Scatterplot_TongueID_vs_Volumes_Controls_Ordered_Colored.png", width=1000, height=700, units="px", res=100)
plot_graph()
dev.off()

# Subset data for men and compute correlation
correlation_men <- cor.test(data_filtered[data_filtered$`sex-numerical` == 0,]$`Total-vol`, 
                            data_filtered[data_filtered$`sex-numerical` == 0,]$`IOC-vol-mm3`)

# Subset data for women and compute correlation
correlation_women <- cor.test(data_filtered[data_filtered$`sex-numerical` == 1,]$`Total-vol`, 
                              data_filtered[data_filtered$`sex-numerical` == 1,]$`IOC-vol-mm3`)

# Print the results
print("Correlation for Men:")
print(correlation_men)
print("Correlation for Women:")
print(correlation_women)

####NORMALISED volume by sex:
# Order control_data by `Total-vol` and store the order of `tongue-id`
control_data <- control_data %>% 
  arrange(`Total-vol`)
tongue_id_order <- control_data$`tongue-id`

# Remove duplicated tongue-id values, keeping only one instance of each
control_data <- control_data %>% 
  distinct(`tongue-id`, .keep_all = TRUE)

# Plot unnormalized volumes using the order
plot_unnormalized <- ggplot(control_data, aes(x=factor(`tongue-id`, levels=tongue_id_order), y=`Total-vol`, color=as.factor(`sex-numerical`))) +
  geom_point(size=3) +
  labs(title="Unnormalized Volume by Tongue ID", x="Tongue ID", y="Total Volume") +
  scale_color_manual(name="Sex", values=c("0"="darkblue", "1"="darkred"), labels=c("0"="Male", "1"="Female")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Save the unnormalized volumes plot to PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Ordered_Unnormalized_Volumes.png", width=1400, height=700, units="px", res=100)
print(plot_unnormalized)
dev.off()

# Plot normalized volumes using the same order
plot_normalized <- ggplot(control_data, aes(x=factor(`tongue-id`, levels=tongue_id_order), y=normalized_vol, color=as.factor(`sex-numerical`))) +
  geom_point(size=3) +
  labs(title="Normalized Volume by Tongue ID", x="Tongue ID", y="Normalized Volume") +
  scale_color_manual(name="Sex", values=c("0"="darkblue", "1"="darkred"), labels=c("0"="Male", "1"="Female")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Save the normalized volumes plot to PNG
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Ordered_Normalized_Volumes.png", width=1400, height=700, units="px", res=100)
print(plot_normalized)
dev.off()


##################################
##TONGUE POSITION RESULTS ANOVA ##
##################################

#install.packages("ez")
library(ez)
data <- read_excel("~/OneDrive - The University of Queensland/Projects/Tongue_seg/tongue-movement-experiment/data.xlsx")
# Rename columns
names(data) <- c("Label_Id", "Volume_mm3", "dataset")

data$Volume_mm3 <- as.numeric(data$Volume_mm3)

# Remove rows with NA values in the Volume_mm3 column
data <- data[!is.na(data$Volume_mm3), ]

# Run the ANOVA
results <- ezANOVA(
  data = data,
  dv = .(Volume_mm3),     # dependent variable
  wid = .(Label_Id),      # id variable (subject/label id)
  within = .(dataset),    # within-subjects variable (dataset)
  detailed = TRUE         # provides detailed output
)
print(results)

# Install and load the ggplot2 package



# Specify the file path and dimensions
png("~/OneDrive - The University of Queensland/Projects/Tongue_seg/tongue-movement-experiment/anova-results.png", width=1200, height=800, units="px", res=100)

# Calculate mean and standard error for each dataset
data_summary <- data.frame(
  dataset = unique(data$dataset),
  mean = tapply(data$Volume_mm3, data$dataset, mean),
  se = tapply(data$Volume_mm3, data$dataset, function(x) sd(x)/sqrt(length(x)))
)

# Plot
ggplot(data_summary, aes(x=as.factor(dataset), y=mean)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(0.2)) +
  labs(y="Mean Volume (mm^3)", x="Dataset", title="Mean Volume Across Datasets with SE") +
  theme_minimal()

# Close the graphics device
dev.off()




