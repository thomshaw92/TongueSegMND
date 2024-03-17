# Install necessary packages if not already installed
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

# Muscle volume data
muscle_data <- data.frame(
  Muscle = rep(c("Superior Longitudinal", "Transverse", "Inferior Longitudinal", "Genioglossus"), each = 5),
  Position = factor(rep(1:5, times = 4)),
  Volume = c(18631.7, 18321.9, 18474, 18410, 18885.6,
             11732, 12370.9, 12999.2, 14458.4, 12683.8,
             4505.09, 3431.94, 3720.7, 3777.02, 4025.34,
             5533.18, 5081.09, 5310.46, 5272.06, 5184)
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

# Print the results
print(rm_anova_results)

# Note: Please check for assumptions like sphericity and normality before interpreting the results
