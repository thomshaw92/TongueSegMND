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
library(vegan)
# Check if the package is loaded
if ("package:RVAideMemoire" %in% search()) {
  # Unload the package
  detach("package:RVAideMemoire", unload=TRUE)
  cat("RVAideMemoire has been unloaded.\n")
} else {
  cat("RVAideMemoire is not loaded.\n")
}

# Directory to save the plots and load the data
plots_directory <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical"
data_dir <- ("~/OneDrive - The University of Queensland/Projects/BeLong/clinical_neuropsyc/")

# Load the cleaned dataframe from the RDS file
df_clean <- readRDS(file.path(data_dir, "df.clean.rds"))

# Filter the data to include only the desired groups and exclude cases with missing values
df_clean <- df_clean %>%
  filter(formal.diagnosis.numeric %in% c("ALS", "PLS", "Flail Limb/PMA", "Control")) %>%
  drop_na(`total.vol.cor`,`Trans.vol.cor.IOC`, `SLong.vol.cor.IOC`, `Genio.vol.cor.IOC`, `ILong.vol.cor.IOC`)

muscle_volumes <- c("total.vol.cor","SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")

# Descriptive Statistics for continuous variables
descriptive_stats_cont <- df_clean %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    IQR = ~IQR(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  )))

# Descriptive Statistics for categorical variables
descriptive_stats_cat <- df_clean %>%
  reframe(across(where(is.factor), list(counts = ~table(.x))))

# Output the descriptive statistics
print(descriptive_stats_cont)
print(descriptive_stats_cat)

###
#Visualise
# Assuming df_clean has the necessary columns including segmentation.id
plotting_df <- df_clean %>%
  mutate(
    onset_group = case_when(
      Onset.location.coded == 1 & formal.diagnosis.numeric != "Control" ~ "Bulbar",
      Onset.location.coded != 1 & formal.diagnosis.numeric != "Control" ~ "Non-Bulbar",
      formal.diagnosis.numeric == "Control" ~ "Control"
    ),
    # Ensure total.vol.cor is numeric
    total.vol.cor = as.numeric(total.vol.cor)
  )

# Gather the muscle volume data into a long format for plotting
long_data <- plotting_df %>%
  select(segmentation.id, onset_group, SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC, total.vol.cor) %>%
  pivot_longer(
    cols = c(SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC, total.vol.cor),
    names_to = "Muscle",
    values_to = "Volume"
  ) %>%
  mutate(label = paste("ID:", segmentation.id, "<br>Group:", onset_group, "<br>Volume:", Volume))

# Create a ggplot object with jitter
p <- ggplot(long_data, aes(x = Muscle, y = Volume, color = onset_group, text = label)) +
  geom_jitter(width = 0.4, height = 0) +  # Adjust jitter width as needed
  labs(title = "Muscle Volume by Onset Group", x = "Muscle", y = "Volume (cor IOC)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert the ggplot object to a plotly object for interactivity
p_interactive <- ggplotly(p, tooltip = "text")

# Render the interactive plot
p
p_interactive

######
#PERMANOVA for non-parametric multivariate analysis
# Load necessary library

# Make sure the diagnostic variable is a factor
df_clean$formal.diagnosis.numeric <- as.factor(df_clean$formal.diagnosis.numeric)

# Select relevant columns for the PERMANOVA analysis
df_for_permanova <- df_clean[, c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC", "formal.diagnosis.numeric")]

# Conducting PERMANOVA
# Euclidean distance is commonly used, but you can choose other distance measures depending on your data structure
permanova_results <- adonis2(df_for_permanova[,1:4] ~ formal.diagnosis.numeric, data = df_for_permanova, method = "euclidean")

# Print the results
print(permanova_results)


######
#ANOVA
# Ensure the diagnosis variable is a factor
df_clean$formal.diagnosis.numeric <- as.factor(df_clean$formal.diagnosis.numeric)

# Perform the Kruskal-Wallis test for each volume
kruskal_results <- list()

for (volume in muscle_volumes) {
  # Fit the Kruskal-Wallis test model for each volume
  kruskal_results[[volume]] <- kruskal.test(reformulate("formal.diagnosis.numeric", response = volume), data = df_clean)
  
  # Print the Kruskal-Wallis test results
  cat("\nKruskal-Wallis test for", volume, ":\n")
  print(kruskal_results[[volume]])
}

dunn_results <- list()

for (volume in muscle_volumes) {
  # Perform Dunn's post-hoc test
  cat("\nPerforming Dunn's test for", volume, ":\n")
  dunn_results[[volume]] <- dunn.test(x = df_clean[[volume]], 
                                      g = df_clean$formal.diagnosis.numeric,
                                      method = "bh")  # Benjamini-Hochberg correction for p-values
  
  # Print the Dunn's test results
  cat("\nDunn's test for", volume, ":\n")
  print(dunn_results[[volume]]$res)
}


#### Case control volume study - split out the bulbar and severe bulbar from the normal patients (across all patients and controls)
df_clean <- df_clean %>%
  mutate(across(c(ALSFRS.R.bulbar, ALSFRS.R.Speech, ALSFRS.R.Swallowing, ALSFRS.R.Saliva), 
                ~as.numeric(as.character(.)))) %>%
  drop_na(ALSFRS.R.bulbar, ALSFRS.R.Speech, ALSFRS.R.Swallowing, ALSFRS.R.Saliva)

df_clean <- df_clean %>%
  mutate(bulbar_severity = case_when(
    ALSFRS.R.bulbar <= 6 ~ "Severe Bulbar Involvement",  # Severe if bulbar score is 6 or less
    ALSFRS.R.Swallowing <= 2 | ALSFRS.R.Speech <= 2 ~ "Bulbar Involvement",  # Bulbar symptoms if Swallowing or Speech is 2 or less
    TRUE ~ "Normal"  # All others classified as Normal
  ))

# Print a table of the number of participants in each bulbar_severity category
severity_table <- table(df_clean$bulbar_severity)
print(severity_table)
# Ensure total volume is numeric
df_clean <- df_clean %>%
  mutate(total.vol.cor = as.numeric(as.character(total.vol.cor)))

# Create the violin plot with ggplot2
p <- ggplot(df_clean, aes(x = bulbar_severity, y = total.vol.cor, fill = bulbar_severity)) +
  geom_violin(trim = FALSE) +
  geom_jitter(aes(text = segmentation.id), width = 0.1, alpha = 0.5) +  # Add individual data points with segmentation.id as text
  labs(title = "Distribution of Total Volume by Bulbar Severity",
       x = "Bulbar Severity",
       y = "Total Volume (cor)") +
  theme_minimal() +
  theme(legend.position = "none")

# Convert the ggplot object to an interactive plotly object
p_interactive <- ggplotly(p, tooltip = "text")

# Display the interactive plot
p_interactive

# Display the interactive plot
p_interactive
  theme(legend.position = "none")

#perform KW for each muscle volume to check for differences between bulbar groups
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
