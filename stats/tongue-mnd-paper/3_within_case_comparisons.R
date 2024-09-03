# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(readxl)
library(car)
library(DescTools)
library(dunn.test)
library(plotly)
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
data_dir <- "~/OneDrive - The University of Queensland/Projects/BeLong/clinical_neuropsyc/"

# Load the cleaned dataframe from the RDS file
df_clean <- readRDS(file.path(data_dir, "df.clean.rds"))

# Filter the data to focus on a specific subset for detailed analysis
df_detailed <- df_clean %>%
  filter(formal.diagnosis.numeric %in% c("ALS", "PLS", "PBP", "Flail limb/PMA")) %>%
  drop_na()  # Specify the columns if needed
muscle_volumes <- c("total.vol.cor","SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")

###let's go back and have a look at each disease subtype independently.

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
#make sure is numerical pls
df_clean$months.since.onset <- as.numeric(df_clean$months.since.onset)
#then scale it - 0 mean, 1 SD
df_clean$months.since.onset <- scale(df_clean$months.since.onset)

# Combine disease subtype and ALSFRS-R bulbar score into a single grouping variable
df_clean <- df_clean %>%
  mutate(combined_group = interaction(formal.diagnosis.numeric, bulbar_severity, drop = TRUE))

# List of tongue muscle volumes to analyze
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
# Recode the onset location to a more descriptive factor and combine with diagnosis
df_clean <- df_clean %>%
  mutate(onset_group = case_when(
    Onset.location.coded == 1 ~ "Bulbar",
    Onset.location.coded != 1 ~ "Non-Bulbar"
  ))

# Filter the dataset to exclude any remaining NA values in the muscle volumes
df_filtered <- df_clean %>%
  filter(!is.na(total.vol.cor))

# Perform Wilcoxon rank-sum test for each muscle volume
wilcox_results <- list()

for (muscle in muscle_volumes) {
  # Perform Wilcoxon rank-sum test comparing muscle volumes across onset groups
  wilcox_results[[muscle]] <- wilcox.test(reformulate("onset_group", response = muscle), data = df_filtered)
  
  # Print the Wilcoxon test results
  cat("\nWilcoxon rank-sum test for", muscle, ":\n")
  print(wilcox_results[[muscle]])
}
##the results are NS except for Trans - likely because of the months since onset 
#so do an ANCOVA to control for months since onset
# Load the necessary package
library(RVAideMemoire)

# Loop over each muscle volume
for (muscle in muscle_volumes) {
  # Fit the rank-based ANCOVA model
  ancova_model <- anova(lm(rank(df_clean[[muscle]]) ~ onset_group + months.since.onset, data = df_clean))
  
  # Print the summary of the ANCOVA
  cat("\nRank-based ANCOVA for", muscle, ":\n")
  print(ancova_model)
  
}
###The ANCOVA results indicate that after adjusting for months since onset, 
#the “Bulbar” and “Non-Bulbar” groups show significant differences#
#in total muscle volume and transverse muscle volume, with no significant 
#differences observed for the other muscle volumes. for BOTH months since onset and without. *************


#############################################################
###### Looking at the difference between UMN and LMND pred.##
#############################################################

# Perform ANCOVA for each muscle volume
ancova_results <- list()

for (muscle in muscle_volumes) {
  # Fit the ANCOVA model
  formula <- as.formula(paste(muscle, "~ onset_group + months.since.onset"))
  ancova_model <- lm(formula, data = df_clean)
  ancova_summary <- summary(aov(ancova_model))  # Summary of ANCOVA
  
  # Save and print the ANCOVA summary results
  ancova_results[[muscle]] <- ancova_summary
  cat("\nANCOVA for", muscle, ":\n")
  print(ancova_summary)
}



#############################
### Examining the relationship between time since onset and corrected muscle volumes
# Regression Analysis
# Mapping of muscle volume variable names to their full names
muscle_full_names <- list(
  "SLong.vol.cor.IOC" = "Superior Longitudinal Volume (IOC Corrected)",
  "Genio.vol.cor.IOC" = "Genioglossus Volume (IOC Corrected)",
  "ILong.vol.cor.IOC" = "Inferior Longitudinal Volume (IOC Corrected)",
  "Trans.vol.cor.IOC" = "Transverse/Vertical Volume (IOC Corrected)"
)

# Function to perform and print linear regression results
perform_regression <- function(data, dependent_vars, independent_var) {
  regression_results <- list()
  
  for (var in dependent_vars) {
    formula <- as.formula(paste(var, "~", independent_var))
    model <- lm(formula, data = data)
    regression_summary <- summary(model)
    regression_results[[var]] <- regression_summary
    
    # Print the summary to console
    cat("\nRegression results for", var, "vs.", "Months since onset", ":\n")
    print(regression_summary)
    
    # Get the full name for the current muscle volume variable
    full_name <- muscle_full_names[[var]]
    
    # Enhanced plot aesthetics with specified adjustments
    p <- ggplot(data, aes_string(x = independent_var, y = var)) +
      geom_point(color = "#E75480", shape = 16, size = 3, alpha = 0.6) +  # Soft pink points
      geom_smooth(method = "lm", color = "#C71585", fill = "#FFC0CB", se = TRUE) +  # Darker pink line with light pink CI
      labs(title = paste(full_name, "vs. Months since onset"), x = "Months since onset", y = full_name) +
      theme_minimal(base_size = 14) +  # Cleaner and minimal theme
      theme(
        plot.background = element_rect(fill = "#FFF0F5", color = NA),  # Very light pink background
        panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Hide major grid lines
        panel.grid.minor = element_blank(),  # Hide minor grid lines
        plot.title = element_text(color = "#C71585", size = 16, face = "bold"),  # Title in darker pink
        axis.title = element_text(color = "#C71585", size = 14),  # Axis titles in darker pink
        axis.text = element_text(color = "#D87093"),  # Axis texts in medium pink
        axis.ticks = element_line(color = "#D87093")  # Ticks in medium pink
      )
    print(p)
  }
  
  return(regression_results)
}

# Perform regression analysis with full names in titles and "Months since onset" for x-axis label
regression_results_absolute <- perform_regression(case_data, c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC"), "months.since.onset")

######################### Let's look at a multiple linear regression ########

# Load necessary libraries
library(dplyr)
library(car)

# Filter for ALS patients only and remove rows with NA in predominance.numerical
case_data <- df_clean %>%
  filter(formal.diagnosis.numeric == "ALS", !is.na(predominance.numerical)) %>%
  mutate(
    onset_location = ifelse(Onset.location.coded == 1, "Bulbar", "Limb"),
    age.at.scan = as.numeric(age.at.scan)
  ) %>%
  select(
    months.since.onset,
    age.at.scan,
    ALSFRS.R.bulbar,
    predominance.numerical,
    onset_location,
    SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC
  )

# Ensure onset_location is a factor
case_data$onset_location <- factor(case_data$onset_location, levels = c("Bulbar", "Limb"))

# Check for multicollinearity using VIF for one muscle volume (e.g., SLong.vol.cor.IOC)
model_vif <- lm(SLong.vol.cor.IOC ~ age.at.scan + months.since.onset + ALSFRS.R.bulbar + predominance.numerical + onset_location, data = case_data)
vif_values <- vif(model_vif)
print(vif_values)

# Interpretation:
# • VIF < 5: All of the VIF values are well below 5, indicating that there is no serious multicollinearity
# among the predictors in your model.

# Perform multivariate multiple linear regression for all muscle volumes simultaneously
model_multivariate <- lm(cbind(SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC) ~ 
                           age.at.scan + months.since.onset + ALSFRS.R.bulbar + predominance.numerical + onset_location, 
                         data = case_data)

# View the summary of the multivariate model
summary(model_multivariate)