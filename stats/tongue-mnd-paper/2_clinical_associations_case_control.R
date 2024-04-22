# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(readxl)
library(car) # for MANOVA
library(DescTools)
library(dunn.test)
library(plotly)

# Directory to save the plots and load the data
plots_directory <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical"
data_dir <- ("~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/")

# Load the cleaned dataframe from the RDS file
df_clean <- readRDS(file.path(data_dir, "df_clean.rds"))

# Filter the data to include only the desired groups and exclude cases with missing values
data_filtered_complete_cases <- df_clean %>%
  filter(formal_diagnosis_numeric %in% c("ALS", "PLS", "PBP", "Flail limb/PMA", "Control")) %>%
  drop_na(`Trans.vol.cor.IOC`, `SLong.vol.cor.IOC`, `Genio.vol.cor.IOC`, `ILong.vol.cor.IOC`)


# Descriptive Statistics for continuous variables
descriptive_stats_cont <- data_filtered_complete_cases %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    IQR = ~IQR(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  )))

# Descriptive Statistics for categorical variables
descriptive_stats_cat <- data_filtered_complete_cases %>%
  summarise(across(where(is.factor), list(counts = ~table(.x))))

# Output the descriptive statistics
print(descriptive_stats_cont)
print(descriptive_stats_cat)

###
#Visualise

# Assuming df_clean has the necessary columns including segmentation.id
df_clean <- df_clean %>%
  mutate(onset_group = case_when(
    Onset_location_coded == 1 & formal_diagnosis_numeric != "Control" ~ "Bulbar",
    Onset_location_coded != 1 & formal_diagnosis_numeric != "Control" ~ "Non-Bulbar",
    formal_diagnosis_numeric == "Control" ~ "Control"
  )) %>%
  mutate(Total.vol.cor.IOC = SLong.vol.cor.IOC + Genio.vol.cor.IOC + ILong.vol.cor.IOC + Trans.vol.cor.IOC)

# Gather the muscle volume data into a long format for plotting
long_data <- df_clean %>%
  select(segmentation.id, onset_group, SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC, Total.vol.cor.IOC) %>%
  pivot_longer(cols = c(SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC, Total.vol.cor.IOC),
               names_to = "Muscle", values_to = "Volume") %>%
  mutate(label = paste("ID:", segmentation.id, "<br>Group:", onset_group, "<br>Volume:", Volume))

# Create a ggplot object with jitter
p <- ggplot(long_data, aes(x = Muscle, y = Volume, color = onset_group, text = label)) +
  geom_jitter(width = 0.25, height = 0) +  # Adjust jitter width as needed
  labs(title = "Muscle Volume by Onset Group", x = "Muscle", y = "Volume (cor IOC)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert the ggplot object to a plotly object for interactivity
p_interactive <- ggplotly(p, tooltip = "text")

# Render the interactive plot
p_interactive

######
#PERMANOVA for non-parametric multivariate analysis
# Load necessary library
library(vegan)

# Assuming 'data_filtered_complete_cases' is your dataset
# Make sure the diagnostic variable is a factor
data_filtered_complete_cases$formal_diagnosis_numeric <- as.factor(data_filtered_complete_cases$formal_diagnosis_numeric)

# Select relevant columns for the PERMANOVA analysis
df_for_analysis <- data_filtered_complete_cases[, c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC", "formal_diagnosis_numeric")]

# Conducting PERMANOVA
# Euclidean distance is commonly used, but you can choose other distance measures depending on your data structure
permanova_results <- adonis(df_for_analysis[,1:4] ~ formal_diagnosis_numeric, data = df_for_analysis, method = "euclidean")

# Print the results
print(permanova_results)


######
#ANOVA
# Ensure the diagnosis variable is a factor
data_filtered_complete_cases$formal_diagnosis_numeric <- as.factor(data_filtered_complete_cases$formal_diagnosis_numeric)

# Perform the Kruskal-Wallis test for each volume
volume_names <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")
kruskal_results <- list()

for (volume in volume_names) {
  # Fit the Kruskal-Wallis test model for each volume
  kruskal_results[[volume]] <- kruskal.test(reformulate("formal_diagnosis_numeric", response = volume), data = data_filtered_complete_cases)
  
  # Print the Kruskal-Wallis test results
  cat("\nKruskal-Wallis test for", volume, ":\n")
  print(kruskal_results[[volume]])
}

dunn_results <- list()

for (volume in volume_names) {
  # Perform Dunn's post-hoc test
  dunn_results[[volume]] <- dunn.test(x = data_filtered_complete_cases[[volume]], 
                                      g = data_filtered_complete_cases$formal_diagnosis_numeric,
                                      method = "bh")  # Benjamini-Hochberg correction for p-values
  
  # Print the Dunn's test results
  cat("\nDunn's test for", volume, ":\n")
  print(dunn_results[[volume]]$res)
}


#### Case control volume study

# Update the bulbar_severity variable to include controls as a separate group
df_clean <- data_filtered_complete_cases %>%
  mutate(bulbar_severity = case_when(
    formal_diagnosis_numeric == "Control" ~ "Control",
    ALSFRS.R.bulbar <= 3 ~ "Severe",
    ALSFRS.R.bulbar <= 6 ~ "Moderate",
    ALSFRS.R.bulbar <= 9 ~ "Mild",
    ALSFRS.R.bulbar <= 12 ~ "Normal"
  ))

# Muscle volumes to analyze
muscle_volumes <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")
kruskal_results <- list()

for (muscle in muscle_volumes) {
  # Perform the Kruskal-Wallis test comparing muscle volumes across bulbar severity groups and controls
  kruskal_results[[muscle]] <- kruskal.test(reformulate("bulbar_severity", response = muscle), data = df_clean)
  
  # Print the Kruskal-Wallis test results for each muscle volume
  cat("\nKruskal-Wallis test for", muscle, ":\n")
  print(kruskal_results[[muscle]])
}

# If the Kruskal-Wallis test shows significant differences, you may proceed with post-hoc analysis
# For post-hoc analysis, Dunn's test can be used for pairwise comparisons between groups

dunn_results <- list()

for (muscle in muscle_volumes) {
  # Perform Dunn's post-hoc test
  dunn_results[[muscle]] <- dunn.test(x = df_clean[[muscle]], 
                                      g = df_clean$bulbar_severity,
                                      method = "bh")  # Benjamini-Hochberg correction for p-values
  
  # Print the Dunn's test results for each muscle volume
  cat("\nDunn's test for", muscle, ":\n")
  print(dunn_results[[muscle]]$res)
}

# Assuming df_clean is already prepared with 'bulbar_severity' and muscle volumes
muscle_volumes <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")
dunn_results <- list()

for (muscle in muscle_volumes) {
  # Perform Dunn's post-hoc test after Kruskal-Wallis to get pairwise comparisons
  dunn_results[[muscle]] <- dunn.test(x = df_clean[[muscle]], 
                                      g = df_clean$bulbar_severity,
                                      method = "bh")  # Benjamini-Hochberg correction for p-values
  
  # Extracting p-values for comparisons between each bulbar category and controls
  pairwise_comparisons <- dunn_results[[muscle]]$res
  control_comparisons <- pairwise_comparisons[grep("Control", rownames(pairwise_comparisons)),]
  
  # Print the Dunn's test results for each muscle volume with focus on control comparisons
  cat("\nDunn's test for", muscle, "comparisons with control:\n")
  print(control_comparisons)
}


###let's go back and have a look at each disease subtype independently.

library(dplyr)
library(dunn.test)

# Assuming df_clean contains your dataset
# Combine disease subtype and ALSFRS-R bulbar score into a single grouping variable
df_clean <- df_clean %>%
  mutate(combined_group = interaction(formal_diagnosis_numeric, bulbar_severity, drop = TRUE))

# List of tongue muscle volumes to analyze
muscle_volumes <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")
kruskal_results <- list()
dunn_results <- list()

for (muscle in muscle_volumes) {
  # Perform Kruskal-Wallis test comparing muscle volumes across the combined groups
  kruskal_results[[muscle]] <- kruskal.test(reformulate("combined_group", response = muscle), data = df_clean)
  
  # Print the Kruskal-Wallis test results
  cat("\nKruskal-Wallis test for", muscle, ":\n")
  print(kruskal_results[[muscle]])
  
  # Perform Dunn's post-hoc test for pairwise comparisons
  dunn_results[[muscle]] <- dunn.test(x = df_clean[[muscle]], 
                                      g = df_clean$combined_group,
                                      method = "bh")  # Benjamini-Hochberg correction for p-values
  
  # Print the Dunn's test results
  cat("\nDunn's test for", muscle, ":\n")
  print(dunn_results[[muscle]]$res)
}

## now, let's have a look at bulbar onset patients only.
#compare between all cases and control combined, and split the groups by onset location. 
#splitting the group by onset location may show differences between bulbar vs controls, 
#and all other onset vs controls


# Assuming df_clean contains your dataset
# Recode the onset location to a more descriptive factor and combine with diagnosis
df_clean <- df_clean %>%
  mutate(onset_group = case_when(
    Onset_location_coded == 1 & formal_diagnosis_numeric != "Control" ~ "Bulbar",
    Onset_location_coded != 1 & formal_diagnosis_numeric != "Control" ~ "Non-Bulbar",
    formal_diagnosis_numeric == "Control" ~ "Control"
  ))

# List of tongue muscle volumes to analyze
muscle_volumes <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")
kruskal_results <- list()
dunn_results <- list()

for (muscle in muscle_volumes) {
  # Perform Kruskal-Wallis test comparing muscle volumes across onset groups
  kruskal_results[[muscle]] <- kruskal.test(reformulate("onset_group", response = muscle), data = df_clean)
  
  # Print the Kruskal-Wallis test results
  cat("\nKruskal-Wallis test for", muscle, ":\n")
  print(kruskal_results[[muscle]])
  
  # Perform Dunn's post-hoc test if Kruskal-Wallis test is significant
  if (kruskal_results[[muscle]]$p.value < 0.05) {
    cat("\nPerforming post-hoc Dunn's test for", muscle, "\n")
    dunn_test_results <- dunn.test(x = df_clean[[muscle]], 
                                   g = df_clean$onset_group,
                                   method = "bh")  # Benjamini-Hochberg correction for p-values
    dunn_results[[muscle]] <- dunn_test_results$res
    print(dunn_test_results$res)
  }
}


#Then, we can look at each disease subtype and split by bulbar onset. 


# Assuming df_clean contains your dataset and 'Onset_location_coded' is correctly coded
# Recode Onset_location_coded to a more descriptive factor
df_clean <- df_clean %>%
  mutate(onset_type = case_when(
    Onset_location_coded == 1 ~ "Limb",
    Onset_location_coded == 2 ~ "Bulbar",
    TRUE ~ "Other"  # Assuming 3 or other values represent other types
  ))

# Combine disease subtype and onset type into a single grouping variable
df_clean <- df_clean %>%
  mutate(combined_group = interaction(formal_diagnosis_numeric, onset_type, drop = TRUE))

# List of tongue muscle volumes to analyze
muscle_volumes <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")
kruskal_results <- list()
dunn_results <- list()

for (muscle in muscle_volumes) {
  # Perform Kruskal-Wallis test comparing muscle volumes across the combined groups
  kruskal_results[[muscle]] <- kruskal.test(reformulate("combined_group", response = muscle), data = df_clean)
  
  # Print the Kruskal-Wallis test results
  cat("\nKruskal-Wallis test for", muscle, ":\n")
  print(kruskal_results[[muscle]])
  
  # Perform Dunn's post-hoc test for pairwise comparisons
  dunn_results[[muscle]] <- dunn.test(x = df_clean[[muscle]], 
                                      g = df_clean$combined_group,
                                      method = "bh")  # Benjamini-Hochberg correction for p-values
  
  # Print the Dunn's test results
  cat("\nDunn's test for", muscle, ":\n")
  print(dunn_results[[muscle]]$res)
}





######
## need to have more understanding of the differences between the groups and controls. 
#plots


##have a look at bulbar onset patients


