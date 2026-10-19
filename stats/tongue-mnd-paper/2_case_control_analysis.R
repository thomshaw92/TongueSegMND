library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(readxl)
library(car)
library(DescTools)
library(dunn.test)
library(plotly)
library(vegan)
# Check if the package is loaded
if ("package:RVAideMemoire" %in% search()) {
  detach("package:RVAideMemoire", unload=TRUE)
  cat("RVAideMemoire has been unloaded.\n")
} else {
  cat("RVAideMemoire is not loaded.\n")
}

# Directory to save the plots and load the data
plots_directory <- file.path(Sys.getenv("TONGUE_ROOT", "."), "Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical")
data_dir <- file.path(Sys.getenv("TONGUE_ROOT", "."), "Projects/BeLong/clinical_neuropsyc")

# Load the cleaned dataframe from the RDS file
df_clean <- readRDS(file.path(data_dir, "df.clean.rds"))

# Filter groups and exclude missing values
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

print(descriptive_stats_cont)
print(descriptive_stats_cat)

###
#Visualise
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

# Reshape to long format for plotting
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
  geom_jitter(width = 0.4, height = 0) +
  labs(title = "Muscle Volume by Onset Group", x = "Muscle", y = "Volume (cor IOC)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to plotly for interactivity
p_interactive <- ggplotly(p, tooltip = "text")

p
p_interactive

######
#PERMANOVA

# Make sure the diagnostic variable is a factor
df_clean$formal.diagnosis.numeric <- as.factor(df_clean$formal.diagnosis.numeric)

# Select relevant columns for the PERMANOVA analysis
df_for_permanova <- df_clean[, c("SLong.vol.cor.IOC", "Genio.vol.cor.IOC", "ILong.vol.cor.IOC", "Trans.vol.cor.IOC", "formal.diagnosis.numeric")]

# Conducting PERMANOVA
permanova_results <- adonis2(df_for_permanova[,1:4] ~ formal.diagnosis.numeric, data = df_for_permanova, method = "euclidean")

print(permanova_results)


######
#ANOVA
# Ensure the diagnosis variable is a factor
df_clean$formal.diagnosis.numeric <- as.factor(df_clean$formal.diagnosis.numeric)

# Perform the Kruskal-Wallis test for each volume
kruskal_results <- list()

for (volume in muscle_volumes) {
  kruskal_results[[volume]] <- kruskal.test(reformulate("formal.diagnosis.numeric", response = volume), data = df_clean)
  
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
  
  cat("\nDunn's test for", volume, ":\n")
  print(dunn_results[[volume]]$res)
}


#### Case-control volumes by bulbar severity
df_clean <- df_clean %>%
  mutate(across(c(ALSFRS.R.bulbar, ALSFRS.R.Speech, ALSFRS.R.Swallowing, ALSFRS.R.Saliva), 
                ~as.numeric(as.character(.)))) %>%
  drop_na(ALSFRS.R.bulbar, ALSFRS.R.Speech, ALSFRS.R.Swallowing, ALSFRS.R.Saliva)

df_clean <- df_clean %>%
  mutate(bulbar_severity = case_when(
    ALSFRS.R.bulbar <= 6 ~ "Severe Bulbar Involvement",
    ALSFRS.R.Swallowing <= 2 | ALSFRS.R.Speech <= 2 ~ "Bulbar Involvement",
    TRUE ~ "Normal"
  ))

severity_table <- table(df_clean$bulbar_severity)
print(severity_table)
# Ensure total volume is numeric
df_clean <- df_clean %>%
  mutate(total.vol.cor = as.numeric(as.character(total.vol.cor)))


# Reorder bulbar_severity levels
df_clean$bulbar_severity <- factor(df_clean$bulbar_severity, 
                                   levels = c("Normal", "Bulbar Involvement", "Severe Bulbar Involvement"))

# Check for NAs and ensure ALSFRS.R.bulbar is numeric
df_clean <- df_clean %>%
  mutate(ALSFRS.R.bulbar = as.numeric(as.character(ALSFRS.R.bulbar))) %>%
  filter(!is.na(ALSFRS.R.bulbar))

# Violin plot with points colored by ALSFRS.R.bulbar
library(plotly)

# Check for NAs and ensure ALSFRS.R.bulbar is numeric
df_clean <- df_clean %>%
  mutate(ALSFRS.R.bulbar = as.numeric(as.character(ALSFRS.R.bulbar))) %>%
  filter(!is.na(ALSFRS.R.bulbar))

p <- ggplot(df_clean, aes(x = bulbar_severity, y = total.vol.cor, fill = bulbar_severity)) +
  geom_violin(trim = FALSE) +
  geom_jitter(aes(color = ALSFRS.R.bulbar, text = segmentation.id),  # Add segmentation.id for hover
              width = 0.2,
              size = 2,
              alpha = 0.7) +
  scale_fill_manual(values = c("Severe Bulbar Involvement" = "#006d2c", 
                               "Bulbar Involvement" = "#66c2a4", 
                               "Normal" = "#ccece6")) +
  scale_color_gradient(low = "red", high = "blue", limits = c(0, 12)) +  # Color gradient for bulbar score
  labs(title = "Distribution of Total Volume by Bulbar Severity",
       x = "Bulbar Severity Category",
       y = "Total Volume (cor)",
       fill = "Bulbar Severity",
       color = "ALSFRS.R Bulbar Score") +
  theme_minimal() +
  theme(legend.position = "right") +  # Show legend for the color gradient
  guides(fill = "none")  # Hide the fill legend

print(p)
# Convert to plotly with segmentation.id as hover text
p_interactive <- ggplotly(p, tooltip = c("text"))

p_interactive




# Kruskal-Wallis across bulbar groups
kruskal_results <- list()
for (muscle in muscle_volumes) {
  kruskal_results[[muscle]] <- kruskal.test(reformulate("bulbar_severity", response = muscle), data = df_clean)
  
  cat("\nKruskal-Wallis test for", muscle, ":\n")
  print(kruskal_results[[muscle]])
}


dunn_results <- list()

for (muscle in muscle_volumes) {
  # Perform Dunn's post-hoc test
  dunn_results[[muscle]] <- dunn.test(x = df_clean[[muscle]], 
                                      g = df_clean$bulbar_severity,
                                      method = "bh")  # Benjamini-Hochberg correction for p-values
  
  cat("\nDunn's test for", muscle, ":\n")
  print(dunn_results[[muscle]]$res)
}

dunn_results <- list()

for (muscle in muscle_volumes) {
  # Perform Dunn's post-hoc test
  dunn_results[[muscle]] <- dunn.test(x = df_clean[[muscle]], 
                                      g = df_clean$bulbar_severity,
                                      method = "bh")  # Benjamini-Hochberg correction for p-values
  
  # Extract comparisons with controls
  pairwise_comparisons <- dunn_results[[muscle]]$res
  control_comparisons <- pairwise_comparisons[grep("Control", rownames(pairwise_comparisons)),]
  
  cat("\nDunn's test for", muscle, "comparisons with control:\n")
  print(control_comparisons)
}
