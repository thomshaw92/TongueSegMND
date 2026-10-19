library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)

# Directory to save the plots and load the data
plots_directory <- file.path(Sys.getenv("TONGUE_ROOT", "."), "Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical")
data_dir <- file.path(Sys.getenv("TONGUE_ROOT", "."), "Projects/BeLong/clinical_neuropsyc")

# Load the cleaned dataframe from the RDS file
df_clean <- readRDS(file.path(data_dir, "df.clean.rds"))

df_clean <- df_clean %>%
  filter(formal.diagnosis.numeric == "ALS" & MND.IMAGING.session == "ses-01" & dataset != "Sydney" & dataset != "SEVENTEA")

cat("Number of participants included after filtering: ", nrow(df_clean), "\n")

# Muscle volumes of interest
muscle_volumes <- c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "Trans.vol.cor.IOC", "ILong.vol.cor.IOC")
# Check the structure of the relevant columns
cat("Structure of relevant columns:\n")
str(df_clean %>% select(months.since.onset, all_of(muscle_volumes)))

# Ensure all relevant columns are numeric
df_clean <- df_clean %>%
  mutate(across(c(months.since.onset, all_of(muscle_volumes)), as.numeric))

cat("Summary after converting to numeric:\n")
print(summary(df_clean %>% select(months.since.onset, all_of(muscle_volumes))))

# Check for missing values
cat("Checking for NAs:\n")
na_counts <- colSums(is.na(df_clean %>% select(months.since.onset, all_of(muscle_volumes))))
print(na_counts)

# Remove rows with missing values
df_clean <- df_clean %>%
  filter(!is.na(months.since.onset) & rowSums(is.na(select(., all_of(muscle_volumes)))) == 0)

# Step 1: Calculate residuals of all muscle volumes over months.since.onset
calculate_residuals <- function(df, muscle_volume) {
  non_missing_data <- df %>%
    filter(!is.na(months.since.onset) & !is.na(.data[[muscle_volume]]))
  
  model <- lm(as.formula(paste(muscle_volume, "~ months.since.onset")), data = non_missing_data)
  residuals <- residuals(model)
  
  # Create a full-length vector with NA for rows that were filtered out
  full_residuals <- rep(NA, nrow(df))
  full_residuals[!is.na(df$months.since.onset) & !is.na(df[[muscle_volume]])] <- residuals
  
  return(full_residuals)
}

# Add the residuals as new columns
for (muscle in muscle_volumes) {
  df_clean[[paste("residual", muscle, sep = "_")]] <- calculate_residuals(df_clean, muscle)
}

cat("Residuals for each muscle volume:\n")
print(df_clean %>% select(starts_with("residual")))

# Step 2: Z-score the residuals
z_score_residuals <- function(df, prefix = "residual_") {
  df %>%
    mutate(across(starts_with(prefix), ~ {
      (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)
    }, .names = "z_{col}"))
}

df_clean <- z_score_residuals(df_clean)

cat("Z-scores for each residual muscle volume:\n")
print(df_clean %>% select(starts_with("z_residual")))


# Step 3: Perform weighted k-means clustering
weights <- c(7, 1, 5, 1)

# Select the Z-scored residuals for clustering
z_scored_columns <- c("z_residual_SLong.vol.cor.IOC", "z_residual_Genio.vol.cor.IOC", "z_residual_Trans.vol.cor.IOC", "z_residual_ILong.vol.cor.IOC")

# Prepare the data for clustering
weighted_data <- df_clean %>%
  select(all_of(z_scored_columns)) %>%
  mutate(across(everything(), ~ . * weights[match(cur_column(), z_scored_columns)]))

# Replace any NA/NaN/Inf in the weighted data
weighted_data <- weighted_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), mean(., na.rm = TRUE), .)))

# Perform k-means clustering
set.seed(123)
kmeans_result <- kmeans(weighted_data, centers = 3)

# Add the cluster assignments to the original data frame
df_clean <- df_clean %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Check the cluster distribution
cat("Cluster distribution:\n")
print(table(df_clean$cluster))
# Convert census.minus.onset from days to months
df_clean <- df_clean %>%
  mutate(census.minus.onset = census.minus.onset / 30.44)

cat("Structure after converting census.minus.onset to months:\n")
str(df_clean$census.minus.onset)
# Step 4: Survival analysis based on clusters
# Create the survival object
mnd.surv <- Surv(time = df_clean$census.minus.onset, event = df_clean$event)

# Fit the survival curves for the clusters
surv_fit <- survfit(mnd.surv ~ cluster, data = df_clean)
# Plot the survival curves by cluster
ggsurvplot(
  surv_fit, 
  data = df_clean, 
  pval = TRUE,
  title = "Survival Curves by Weighted K-means Clusters",
  xlab = "Months since symptom onset", 
  ylab = "Survival Probability",
  ggtheme = theme_minimal(), 
  palette = c("#D92E40", "#4682B4", "#73A857"),
  risk.table = TRUE,  # Show the number of patients at risk
  risk.table.col = "strata",
  risk.table.height = 0.25,
  conf.int = TRUE,
  linetype = "solid",
  size = 1.2,
  censor.size = 4,
  censor.shape = 124,  # Shape for censor marks
  alpha = 1
)

# Fit a Cox proportional hazards model to compare the clusters
cox_model_cluster <- coxph(Surv(time = df_clean$census.minus.onset, event = df_clean$event) ~ cluster, data = df_clean)
summary(cox_model_cluster)


library(dplyr)
library(ggplot2)

# Plot muscle volumes by cluster
for (muscle in muscle_volumes) {
  p <- ggplot(df_clean, aes(x = cluster, y = .data[[muscle]], color = cluster)) +
    geom_boxplot(outlier.shape = NA) +  # Hide outliers in the boxplot
    geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
    labs(title = paste("Volumes by Cluster for", muscle),
         x = "Cluster",
         y = muscle,
         color = "Cluster") +
    scale_color_manual(values = c("#fbb4ae", "#b3cde3", "#ccebc5")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right"
    )
  print(p)
  
  ggsave(filename = paste0(plots_directory, "/Volumes_by_Cluster_for_", muscle, ".png"), plot = p, width = 8, height = 6)
}