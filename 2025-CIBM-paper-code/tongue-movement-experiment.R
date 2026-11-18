# Install necessary packages if not already installed
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

source(file.path(Sys.getenv("TONGUE_ROOT", "."), "Projects/Tongue_seg/movement-volumes.R"))  # defines movement_volumes (participant data, kept with the data)

# Muscle volume data
muscle_data <- data.frame(
  Muscle = rep(c("Superior Longitudinal", "Transverse", "Inferior Longitudinal", "Genioglossus"), each = 5),
  Position = factor(rep(1:5, times = 4)),
  Volume = movement_volumes
)

# Calculate percentage change for each muscle
percentage_changes <- muscle_data %>%
  group_by(Muscle) %>%
  mutate(Change = (Volume - lag(Volume)) / lag(Volume) * 100) %>%
  summarise(AverageChange = mean(Change, na.rm = TRUE))

print(percentage_changes)

# Repeated Measures ANOVA
# Install the necessary package if not already installed
if (!require("ez")) {
  install.packages("ez", dependencies = TRUE)
}
library(ez)

# Run the Repeated Measures ANOVA using ezANOVA
rm_anova_results <- ezANOVA(data = muscle_data,
                            dv = .(Volume),
                            wid = .(Muscle),
                            within = .(Position),
                            detailed = TRUE)

print(rm_anova_results)

