
##########################
###Segmentation results###
##########################

# Load necessary libraries
#install.packages(c("readxl", "dplyr", "ggplot2", "gridExtra"))
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

# Read in the spreadsheet
data <- read_excel("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/main-tongue-data-spreadsheet.xlsx")

# List of IDs to exclude
exclude_ids <- c(9, 61, 126, 7, 53, 113, 63, 116, 21, 72, 149, 8, 68, 114, 84, 121, 22, 10, 69, 115, 129, 15, 18, 77, 118, 133, 28, 78, 119, 135,
                 29, 82, 120, 138, 30, 96, 126, 161, 32, 97, 141, 168, 33, 98, 143, 176, 39, 144, 182, 40, 147, 185, 42, 149, 186,
                 46, 151, 193, 154, 198, 157, 200, 158, 201, 172, 187, 189, 191, 196, 197)

# Filter out the data
filtered_data <- data %>% 
  filter(!(`segmentation-id` %in% exclude_ids))

# Function to calculate IQR and return a logical vector indicating whether each value is an outlier
calculate_outliers <- function(data, column) {
  # Ensure the column is treated as numeric
  numeric_column <- as.numeric(data[[column]])
  
  Q1 <- quantile(numeric_column, 0.25, na.rm = TRUE)
  Q3 <- quantile(numeric_column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Return a logical vector indicating whether each value is an outlier
  numeric_column < lower_bound | numeric_column > upper_bound
}

# Define all the volume columns including 'Total-vol'
volume_columns <- c('Trans-vol', 'SLong-vol', 'Genio-vol', 'ILong-vol', 'Total-vol')

# Apply the function to each volume column and filter out the outliers
for (column in volume_columns) {
  # Convert the column to numeric inside the loop
  filtered_data[[column]] <- as.numeric(as.character(filtered_data[[column]]))
  
  outliers <- calculate_outliers(filtered_data, column)
  filtered_data <- filtered_data[!outliers, ]
}

# After removing outliers for each volume, recalculate the normalized volumes without outliers
filtered_data$normalized_vol <- filtered_data$`Total-vol` / filtered_data$`IOC-vol-mm3`

# Convert normalized_vol to numeric and remove its outliers
filtered_data$normalized_vol <- as.numeric(filtered_data$normalized_vol)
normalized_outliers <- calculate_outliers(filtered_data, 'normalized_vol')
filtered_data <- filtered_data[!normalized_outliers, ]


# Function to print correlation results
print_correlation <- function(test_result, description) {
  cat(description, "\n")
  print(test_result)
}

# Check for correlation and print to console
correlation_result <- cor.test(filtered_data$`sex_numerical`, filtered_data$`Total-vol`)
print_correlation(correlation_result, "Correlation between Sex and Total Volume:")

# Check for correlation between sex and normalized tongue volume
correlation_normalized <- cor.test(filtered_data$`sex_numerical`, filtered_data$normalized_vol)
print_correlation(correlation_normalized, "Correlation between Sex and Normalized Volume:")

# Correlation between unnormalized total tongue volume and weight
correlation_unnorm_weight <- cor.test(filtered_data$`Total-vol`, filtered_data$weight)
print_correlation(correlation_unnorm_weight, "Correlation between Unnormalized Volume and Weight:")

# Correlation between normalized total tongue volume and weight
correlation_norm_weight <- cor.test(filtered_data$normalized_vol, filtered_data$weight)
print_correlation(correlation_norm_weight, "Correlation between Normalized Volume and Weight:")

# Correlation between unnormalized total tongue volume and height
correlation_unnorm_height <- cor.test(filtered_data$`Total-vol`, filtered_data$height)
print_correlation(correlation_unnorm_height, "Correlation between Unnormalized Volume and Height:")

# Correlation between normalized total tongue volume and height
correlation_norm_height <- cor.test(filtered_data$normalized_vol, filtered_data$height)
print_correlation(correlation_norm_height, "Correlation between Normalized Volume and Height:")

###CHECK WHICH MODEL IS BEST FOR PREDICTING VOLUME###
# Model including weight, height, and sex for Total Volume
model_TotalVol_Whs <- lm(`Total-vol` ~ `sex_numerical` + weight + height, data = filtered_data)
summary(model_TotalVol_Whs)

# Model including weight, height, and sex for Normalized Volume
model_NormVol_Whs <- lm(normalized_vol ~ `sex_numerical` + weight + height, data = filtered_data)
summary(model_NormVol_Whs)

# Model with IOC volume alone for comparison
model_IOC <- lm(`Total-vol` ~ `IOC-vol-mm3`, data = filtered_data)
summary(model_IOC)

#  Scatter plot of IOC-vol-mm3 vs. Total-vol (as before)
pdf("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Scatterplot_IOC_vs_Total_Volume.pdf")
ggplot(filtered_data, aes(x=`IOC-vol-mm3`, y=`Total-vol`, color=`sex_numerical`)) +
  geom_point(size=3, alpha=0.7) +
  labs(title="Scatter plot of IOC volume vs Total Tongue Volume", x="IOC Volume", y="Total Volume") +
  theme_minimal()
dev.off()


# Check for correlation between Total-vol and IOC-vol-mm3
correlation_result_total_IOC <- cor.test(filtered_data$`Total-vol`, filtered_data$`IOC-vol-mm3`)

# Print the result
print(correlation_result_total_IOC)


####ANOVA of tongue muscle and dataset on volume
# Reshape the filtered data to long format for ANOVA analysis
long_format_data <- filtered_data %>%
  pivot_longer(cols = c(`Trans-vol`, `SLong-vol`, `Genio-vol`, `ILong-vol`),
               names_to = "Muscle",
               values_to = "Volume") %>%
  mutate(Muscle = factor(Muscle),
         Dataset = factor(dataset))

# Conduct a two-way ANOVA to investigate the effects of muscle type and dataset
# on tongue volume
anova_result <- aov(Volume ~ Muscle * dataset, data = long_format_data)
summary(anova_result)

# If there's a significant interaction or main effects, consider post-hoc tests
# Tukey's Honest Significant Difference (HSD) test for post-hoc analysis
# (Adjust as necessary based on ANOVA results)
if ("TukeyHSD" %in% rownames(summary(anova_result)$statistics)) {
  post_hoc_result <- TukeyHSD(anova_result)
  print(post_hoc_result)
}

# You might also want to visualize the interaction between muscle type and dataset
ggplot(long_format_data, aes(x=Muscle, y=Volume, color=dataset)) +
  geom_point(position=position_dodge(width=0.8), size=3) +
  stat_summary(fun=mean, geom="line", aes(group=Dataset), position=position_dodge(width=0.8)) +
  theme_minimal() +
  labs(title="Interaction of Muscle Type and Dataset on Tongue Volume",
       x="Muscle Type",
       y="Volume",
       color="Dataset")

# Save the interaction plot
ggsave("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Muscle_Dataset_Interaction.pdf", width=10, height=6)

### do the same thing with the IOC corrected volumes:
####ANOVA of tongue muscle and dataset on volume
# Reshape the filtered data to long format for ANOVA analysis
long_format_data <- filtered_data %>%
  pivot_longer(cols = c(`Trans-vol-cor-IOC`, `SLong-vol-cor-IOC`, `Genio-vol-cor-IOC`, `ILong-vol-cor-IOC`),
               names_to = "Muscle",
               values_to = "Volume") %>%
  mutate(Muscle = factor(Muscle),
         Dataset = factor(dataset))

# Conduct a two-way ANOVA to investigate the effects of muscle type and dataset
# on tongue volume
anova_result <- aov(Volume ~ Muscle * dataset, data = long_format_data)
summary(anova_result)

# If there's a significant interaction or main effects, consider post-hoc tests
# Tukey's Honest Significant Difference (HSD) test for post-hoc analysis
# (Adjust as necessary based on ANOVA results)
if ("TukeyHSD" %in% rownames(summary(anova_result)$statistics)) {
  post_hoc_result <- TukeyHSD(anova_result)
  print(post_hoc_result)
}

# You might also want to visualize the interaction between muscle type and dataset
ggplot(long_format_data, aes(x=Muscle, y=Volume, color=dataset)) +
  geom_point(position=position_dodge(width=0.8), size=3) +
  stat_summary(fun=mean, geom="line", aes(group=Dataset), position=position_dodge(width=0.8)) +
  theme_minimal() +
  labs(title="Interaction of Muscle Type and Dataset on Tongue Volume",
       x="Muscle Type",
       y="Volume",
       color="Dataset")

# Save the interaction plot
ggsave("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/Muscle_Dataset_Interaction-IOC.pdf", width=10, height=6)

####
# Check dataset totals
# Calculate overall average volume and SD
overall_avg_volume <- mean(filtered_data$`Total-vol`, na.rm = TRUE)
overall_sd_volume <- sd(filtered_data$`Total-vol`, na.rm = TRUE)

# Calculate average volume and SD per muscle, per dataset
average_volume_per_muscle_dataset <- filtered_data %>%
  pivot_longer(cols = c(`Trans-vol`, `SLong-vol`, `Genio-vol`, `ILong-vol`),
               names_to = "Muscle",
               values_to = "Volume") %>%
  group_by(Muscle, dataset) %>%
  summarise(AverageVolume = mean(Volume, na.rm = TRUE),
            SDVolume = sd(Volume, na.rm = TRUE))

# Print results
print(paste("Overall average volume:", overall_avg_volume, "SD:", overall_sd_volume))
print(average_volume_per_muscle_dataset)

##
# check if there are differences between controls and patients:

# Assuming 'filtered_data' is your dataframe
# Filter data to include only those rows needed for comparison
filtered_data_for_ttest <- filtered_data %>%
  filter(!is.na(formal_diagnosis_numeric)) # Ensure no NA values in the diagnosis column

# Define the muscle volume columns to test
muscle_volumes <- c('Trans-vol-cor-IOC', 'SLong-vol-cor-IOC', 'Genio-vol-cor-IOC', 'ILong-vol-cor-IOC')

# Function to perform t-test and print results
perform_t_test <- function(data, column) {
  control_group <- data %>% filter(formal_diagnosis_numeric == '0') %>% pull(column)
  case_group <- data %>% filter(formal_diagnosis_numeric != '0') %>% pull(column)
  
  t_test_result <- t.test(control_group, case_group, na.action = na.exclude)
  cat("\nT-test for", column, ":\n")
  print(t_test_result)
}

# Perform t-tests for each muscle volume
for (column in muscle_volumes) {
  perform_t_test(filtered_data_for_ttest, column)
}
### check the effect size:
# Given data for the Genioglossus muscle
mean_control <- 0.01986147
std_control <- 0.0047467178  # Assuming this is the standard deviation for the control group
n_control <- 45  

mean_case <- 0.01708563
std_case <- 0.006258  # Assuming this is the standard deviation for the case group
n_case <- 76  # Assuming sample size for the case group

# Calculate pooled standard deviation for Cohen's d
s_pooled <- sqrt(((n_control - 1) * std_control^2 + (n_case - 1) * std_case^2) / (n_control + n_case - 2))

# Calculate Cohen's d
cohen_d <- (mean_control - mean_case) / s_pooled

cohen_d



##################
##    PLOTS.   ##
##################

# Define colors for the plots
colors <- c("0" = "blue", "1" = "hotpink")

# Convert 'sex_numerical' to a factor with appropriate labels before reshaping
filtered_data$`sex_numerical` <- factor(filtered_data$`sex_numerical`, levels = c("0", "1"), labels = c("Male", "Female"))

# Define full names for the muscles
muscle_names <- c(`SLong-vol` = "Sup. Longitudinal", 
                  `ILong-vol` = "Inf. Longitudinal", 
                  `Trans-vol` = "Transverse", 
                  `Genio-vol` = "Genioglossus")

# Reshape the data to long format for plotting
long_data <- filtered_data %>%
  pivot_longer(cols = c(`Trans-vol`, `SLong-vol`, `Genio-vol`, `ILong-vol`),
               names_to = "Muscle", values_to = "Volume") %>%
  mutate(Muscle = recode(Muscle, !!!muscle_names))

# Define colors for the violin plots
violin_colors <- c("Sup. Longitudinal" = "#fbb4ae", 
                   "Inf. Longitudinal" = "#b3cde3", 
                   "Transverse" = "#ccebc5", 
                   "Genioglossus" = "#decbe4")

# Define colors for the points
point_colors <- c("Male" = "blue", "Female" = "hotpink")

# Define shapes based on the 'dataset' column
shapes <- c("BeLong" = 8, "EATT" = 17, "Sydney" = 16) # Numeric codes for star, triangle, circle

# Create the violin plot for each muscle volume
violin_plot <- ggplot(long_data, aes(x = Muscle, y = Volume, fill = Muscle)) +
  geom_violin(trim = FALSE, alpha = 0.9, width = 0.8) +
  geom_jitter(aes(color = `sex_numerical`, shape = dataset), position = position_jitter(width = 0.4), size = 1.75, alpha = 0.6) +
  scale_fill_manual(values = violin_colors) +
  scale_color_manual(values = point_colors) +
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
# Arbitrary height in inches, adjust as needed
# Display the plot
print(violin_plot)

# Define colors for the points
point_colors <- c("Male" = "blue", "Female" = "hotpink")

# Create the plots
p1 <- ggplot(filtered_data, aes(x=`IOC-vol-mm3`, y=`Total-vol`, color=factor(`sex_numerical`))) +
  geom_point(alpha=0.7) +
  scale_color_manual(values=colors, labels=c("0"="Male", "1"="Female")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add correlation line
  labs(title="Total Non-Normalised Tongue Volume vs. IOC Volume", x="IOC Volume (mm^3)", y="Total Tongue Volume", color="Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14, face="bold"), 
    axis.title.x = element_text(size=14), 
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14)
  )

p2 <- ggplot(filtered_data, aes(x=weight, y=`Total-vol`, color=factor(`sex_numerical`))) +
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

p3 <- ggplot(filtered_data, aes(x=weight, y=normalized_vol, color=factor(`sex_numerical`))) +
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

p6 <- ggplot(filtered_data, aes(x=factor(`sex_numerical`), y=`Total-vol`, fill=factor(`sex_numerical`))) +
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

p7 <- ggplot(filtered_data, aes(x=factor(`sex_numerical`), y=normalized_vol, fill=factor(`sex_numerical`))) +
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

#filter out height
filtered_data <- filtered_data %>%
  filter(!is.na(height) & height > 0)

p4 <- ggplot(filtered_data, aes(x=height, y=`Total-vol`, color=factor(`sex_numerical`))) +
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

p5 <- ggplot(filtered_data, aes(x=height, y=normalized_vol, color=factor(`sex_numerical`))) +
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

# Arrange the plots into a single figure
#combined_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol=2)
column_plots <- grid.arrange(grobs = list(p2, p4, p6, p3, p5, p7), ncol = 3)

# Combine p1 on top and the two-column layout below
combined_plot <- arrangeGrob(p1, column_plots, ncol = 1, heights = c(1/3, 2/3))

# Save the figure
ggsave("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats/combined_plots_exp-3.pdf", combined_plot, width=16, height=12)

