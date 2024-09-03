##########################
### Segmentation results ###
##########################

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

# Load the data (assuming you have saved it as an Excel file)
# Replace "your_file_path.xlsx" with the actual path to your file
# Load the data from the specified directory
data_path <- "/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Projects/Tongue_seg/data/SciDataReleaseData/control-to-release-data-SS.xlsx"
data <- read_excel(data_path)

# Rename columns for muscle names
data <- data %>%
  rename(
    `Transverse/Vertical` = Trans.vol,
    `Superior Longitudinal` = SLong.vol,
    `Genioglossus` = Genio.vol,
    `Inferior Longitudinal` = ILong.vol
  )

# Melt the data for plotting
melted_data <- data %>%
  select(`Transverse/Vertical`, `Superior Longitudinal`, `Genioglossus`, `Inferior Longitudinal`) %>%
  pivot_longer(cols = everything(), names_to = "Muscle", values_to = "Volume")

# Violin plot with custom colors
violin_plot <- ggplot(melted_data, aes(x = Muscle, y = Volume, fill = Muscle)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c(
    "Transverse/Vertical" = "#b3e2cd",
    "Superior Longitudinal" = "#fdcdac",
    "Genioglossus" = "#cbd5e8",
    "Inferior Longitudinal" = "#f4cae4"
  )) +
  theme_minimal() +
  labs(title = "Violin Plots of Tongue Muscle Volumes",
       x = "Muscle",
       y = "Volume")

# Display the plot
print(violin_plot)


# Function to detect outliers using IQR method
detect_outliers <- function(df, muscle_col) {
  Q1 <- quantile(df[[muscle_col]], 0.25)
  Q3 <- quantile(df[[muscle_col]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- df %>%
    filter(df[[muscle_col]] < lower_bound | df[[muscle_col]] > upper_bound) %>%
    select(segmentation.id, muscle_col)
  return(outliers)
}

# Apply the outlier detection function to each muscle
outliers_transverse_vertical <- detect_outliers(data, "Transverse/Vertical")
outliers_superior_longitudinal <- detect_outliers(data, "Superior Longitudinal")
outliers_genioglossus <- detect_outliers(data, "Genioglossus")
outliers_inferior_longitudinal <- detect_outliers(data, "Inferior Longitudinal")

# Combine all outliers into one data frame for easier inspection
all_outliers <- bind_rows(
  outliers_transverse_vertical %>% mutate(Muscle = "Transverse/Vertical"),
  outliers_superior_longitudinal %>% mutate(Muscle = "Superior Longitudinal"),
  outliers_genioglossus %>% mutate(Muscle = "Genioglossus"),
  outliers_inferior_longitudinal %>% mutate(Muscle = "Inferior Longitudinal")
)

# Display the detected outliers
print(all_outliers)

#### Normality ####

# Check normality using histograms and Q-Q plots
normality_plot <- function(var) {
  p1 <- ggplot(data, aes(x = !!sym(var))) + 
    geom_histogram(bins = 30, fill = "lightblue", color = "black") + 
    ggtitle(paste("Histogram for", var))
  
  p2 <- ggplot(data, aes(sample = !!sym(var))) + 
    stat_qq() + 
    stat_qq_line() + 
    ggtitle(paste("Q-Q Plot for", var))
  
  grid.arrange(p1, p2, ncol = 2)
}

normality_plots <- lapply(c("weight", "height", "age.at.scan", "Transverse/Vertical", "Superior Longitudinal", "Genioglossus", "Inferior Longitudinal"), normality_plot)

#### Differences Between Dataset ####

# ANOVA for group differences
anova_results <- aov(weight ~ dataset, data = data)
summary(anova_results)

# Perform t-tests for pairwise comparisons
t_test_results <- list(
  weight_vs_sex = t.test(weight ~ sex, data = data),
  height_vs_sex = t.test(height ~ sex, data = data),
  age_vs_sex = t.test(age.at.scan ~ sex, data = data)
)

#### Correlations Between Tongue Volumes ####

# Correlation matrix for tongue volumes
tongue_volumes <- data %>% select(Transverse/Vertical, Superior Longitudinal, Genioglossus, Inferior Longitudinal)
correlation_matrix <- cor(tongue_volumes, use = "complete.obs")

# Plot the correlation matrix
corr_plot <- ggplot(melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  ggtitle("Correlation Matrix for Tongue Volumes")

print(corr_plot)