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


# Directory to save the plots and load the data
plots_directory <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical"
data_dir <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/"

# Load the cleaned dataframe from the RDS file
df_clean <- readRDS(file.path(data_dir, "df_clean.rds"))

# Filter the data to focus on a specific subset for detailed analysis
df_detailed <- df_clean %>%
  filter(formal_diagnosis_numeric %in% c("ALS", "PLS", "PBP", "Flail limb/PMA")) %>%
  drop_na()  # Specify the columns if needed

# Group data by disease subtype and severity if bulbar scores are available
df_grouped <- df_detailed %>%
  mutate(bulbar_severity = case_when(
    ALSFRS.R.bulbar <= 3 ~ "Severe",
    ALSFRS.R.bulbar <= 6 ~ "Moderate",
    ALSFRS.R.bulbar <= 9 ~ "Mild",
    ALSFRS.R.bulbar <= 12 ~ "Normal",
    TRUE ~ "Unknown"
  )) %>%
  group_by(formal_diagnosis_numeric, bulbar_severity)


#Regression Analysis

# Filter out control participants and prepare the data
case_data <- df_clean %>%
  filter(formal_diagnosis_numeric != "Control",
    SLong.vol.cor.IOC > 0, Genio.vol.cor.IOC > 0, ILong.vol.cor.IOC > 0, Trans.vol.cor.IOC > 0,
    SLong.vol > 0, Genio.vol > 0, ILong.vol > 0, Trans.vol > 0) %>%
  select(
    months_since_onset,
    SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC,  # Corrected volumes
    SLong.vol, Genio.vol, ILong.vol, Trans.vol  # Non-corrected volumes
  )


# Define the muscle volumes to analyze
muscle_volumes <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")

# Initialize a list to store regression results
regression_results <- list()

# Function to perform and print linear regression results
perform_regression <- function(data, dependent_vars, independent_var) {
  regression_results <- list()
  for (var in dependent_vars) {
    formula <- as.formula(paste(var, "~", independent_var))
    model <- lm(formula, data = data)
    regression_results[[var]] <- summary(model)
    
    # Enhanced plot aesthetics with specified adjustments
    p <- ggplot(data, aes_string(x = independent_var, y = var)) +
      geom_point(color = "#E75480", shape = 16, size = 3, stroke = 0.5, alpha = 0.6) +  # Soft pink points
      geom_smooth(method = "lm", color = "#C71585", fill = "#FFC0CB", se = TRUE) +  # Darker pink line with light pink CI
      labs(title = paste(var, "vs.", independent_var), x = independent_var, y = var) +
      theme_minimal(base_size = 14) +  # Cleaner and minimal theme
      theme(
        plot.background = element_rect(fill = "#FFF0F5", color = "#FFF0F5"),  # Very light pink background
        panel.background = element_rect(fill = "#FFF0F5", color = "#FFF0F5"),
        panel.border = element_blank(),  # Remove panel border
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

# Perform regression analysis
regression_results_absolute <- perform_regression(case_data, muscle_volumes, "months_since_onset")

# Calculate the total volume and each muscle's volume ratio to total volume
case_data_ratio <- case_data %>%
  mutate(Total.vol = SLong.vol + Genio.vol + ILong.vol + Trans.vol,
         SLong.vol.ratio = SLong.vol / Total.vol,
         Genio.vol.ratio = Genio.vol / Total.vol,
         ILong.vol.ratio = ILong.vol / Total.vol,
         Trans.vol.ratio = Trans.vol / Total.vol)

# Define the muscle volume ratios to analyze
muscle_volume_ratios <- c("SLong.vol.ratio", "Genio.vol.ratio", "ILong.vol.ratio", "Trans.vol.ratio")

# Perform regression analysis on volume ratios
regression_results_ratios <- perform_regression(case_data_ratio, muscle_volume_ratios, "months_since_onset")

regression_results <- perform_regression(case_data, muscle_volumes, "months_since_onset")

# Access individual regression results like this, e.g., for SLong.vol.cor.IOC
regression_results[["SLong.vol.cor.IOC"]]
regression_results[["Genio.vol.cor.IOC"]]
regression_results[["ILong.vol.cor.IOC"]]
regression_results[["Trans.vol.cor.IOC"]]
#all of em

# Add a new column for the total volume
case_data <- case_data %>%
  mutate(Total.vol.cor.IOC = SLong.vol.cor.IOC + Genio.vol.cor.IOC + ILong.vol.cor.IOC + Trans.vol.cor.IOC)

# Define the total volume column to analyze
total_volume_columns <- c("Total.vol.cor.IOC")

# Perform regression analysis using the perform_regression function
regression_results_total <- perform_regression(case_data, total_volume_columns, "months_since_onset")

# Access individual regression results
regression_results_total[["Total.vol.cor.IOC"]]

##############
## ALS ONLY ##
##############

# filter
# Data preparation with total volume included
# Data preparation with additional total corrected volume
als_data <- df_clean %>%
  filter(
    formal_diagnosis_numeric == "ALS",
    SLong.vol.cor.IOC > 0, Genio.vol.cor.IOC > 0, ILong.vol.cor.IOC > 0, Trans.vol.cor.IOC > 0,
    SLong.vol > 0, Genio.vol > 0, ILong.vol > 0, Trans.vol > 0
  ) %>%
  mutate(
    Total.vol = SLong.vol + Genio.vol + ILong.vol + Trans.vol,  # Non-corrected total volume
    Total.vol.cor.IOC = SLong.vol.cor.IOC + Genio.vol.cor.IOC + ILong.vol.cor.IOC + Trans.vol.cor.IOC,  # Corrected total volume
    bulbar_severity = case_when(
      ALSFRS.R.bulbar <= 3 ~ "Severe",
      ALSFRS.R.bulbar <= 6 ~ "Moderate",
      ALSFRS.R.bulbar <= 9 ~ "Mild",
      ALSFRS.R.bulbar <= 12 ~ "Normal",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(
    months_since_onset,
    bulbar_severity,
    SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC,  # Corrected volumes
    Total.vol.cor.IOC,  # Included total corrected volume
    Total.vol  # Non-corrected total volume
  )

# Prepare data for plotting
als_data_long <- als_data %>%
  pivot_longer(
    cols = c(SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC, Total.vol.cor.IOC, Total.vol),
    names_to = "variable",
    values_to = "volume"
  )
# Define the muscle volumes to analyze
muscle_volumes <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC")

# Initialize a list to store regression results
regression_results <- list()

# Perform regression analysis
regression_results_absolute <- perform_regression(als_data, muscle_volumes, "months_since_onset")


# Define the muscle volume ratios to analyze
muscle_volume_ratios <- c("SLong.vol.ratio", "Genio.vol.ratio", "ILong.vol.ratio", "Trans.vol.ratio")

regression_results <- perform_regression(als_data, muscle_volumes, "months_since_onset")

# Access individual regression results like this, e.g., for SLong.vol.cor.IOC
regression_results[["SLong.vol.cor.IOC"]]
regression_results[["Genio.vol.cor.IOC"]]
regression_results[["ILong.vol.cor.IOC"]]
regression_results[["Trans.vol.cor.IOC"]]
#all of em

# Add a new column for the total volume
als_data <- als_data %>%
  mutate(Total.vol.cor.IOC = SLong.vol.cor.IOC + Genio.vol.cor.IOC + ILong.vol.cor.IOC + Trans.vol.cor.IOC)

# Define the total volume column to analyze
total_volume_columns <- c("Total.vol.cor.IOC")

# Perform regression analysis using the perform_regression function
regression_results_total <- perform_regression(als_data, total_volume_columns, "months_since_onset")

# Access individual regression results
regression_results_total[["Total.vol.cor.IOC"]]


##look at their interx with bulbar severity

# List of volumes to model
volume_types <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", 
                  "Trans.vol.cor.IOC", "Total.vol")

# Fit models for each volume type with interaction terms
models <- list()
for (vol in volume_types) {
  model_formula <- as.formula(paste(vol, "~ months_since_onset * bulbar_severity"))
  models[[vol]] <- lm(model_formula, data = als_data)
  print(summary(models[[vol]]))  # Printing summary of each model
}

# Gather all volume data for easy plotting
als_data_long <- als_data %>%
  pivot_longer(cols = c(SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, 
                        Trans.vol.cor.IOC, Total.vol, Total.vol.cor.IOC), 
               names_to = "variable", values_to = "volume")



als_data_long$bulbar_severity <- factor(als_data_long$bulbar_severity,
                                        levels = c("Normal", "Mild", "Moderate", "Severe"))

# Define a color palette from cool to hot colors
severity_colors <- c("Normal" = "#1f77b4",  # Blue
                     "Mild" = "#2ca02c",    # Green
                     "Moderate" = "#ff7f0e",  # Orange
                     "Severe" = "#d62728")  # Red

# Plotting with ordered severity and custom color palette
ggplot(als_data_long, aes(x = months_since_onset, y = volume, color = bulbar_severity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = interaction(bulbar_severity, variable))) +
  facet_wrap(~variable, scales = "free_y") +
  scale_color_manual(values = severity_colors) +
  labs(title = "Muscle Volume and Total Volume Change Over Time by Bulbar Severity",
       x = "Months Since Onset", y = "Volume",
       color = "Bulbar Severity") +
  theme_minimal() +
  theme(legend.position = "bottom")



