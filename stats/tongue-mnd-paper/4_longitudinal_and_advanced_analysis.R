library(dplyr)
library(ggplot2)
library(lme4)
library(survival)
library(lubridate)
library(tidyr)

# Directory to save the plots and load the data
plots_directory <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/figures-stats-clinical"
data_dir <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/clinical-demo-data/"

# Load the cleaned dataframe from the RDS file
df_clean <- readRDS(file.path(data_dir, "df_clean.rds"))

# Calculate Total Volume for each session if not already included
als_data <- df_clean %>%
  mutate(Total.vol = SLong.vol.cor.IOC + Genio.vol.cor.IOC + ILong.vol.cor.IOC + Trans.vol.cor.IOC) %>%
  group_by(subjid) %>%
  arrange(subjid, session) %>%
  # Ensure only subjects with both ses-01 and ses-02 are included
  filter(length(unique(session)) == 2) %>%
  filter(!is.na(SLong.vol.cor.IOC) & !is.na(Genio.vol.cor.IOC) & !is.na(ILong.vol.cor.IOC) & !is.na(Trans.vol.cor.IOC)) %>%
  filter(formal_diagnosis_numeric == "ALS") %>% 
  select(subjid, session, SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC, Total.vol)
# Reshape data for easier plotting (from wide to long format)
als_data_long <- als_data %>%
  pivot_longer(cols = c(SLong.vol.cor.IOC, Genio.vol.cor.IOC, ILong.vol.cor.IOC, Trans.vol.cor.IOC, Total.vol),
               names_to = "Muscle", values_to = "Volume")

# Create ggplot object
p <- ggplot(als_data_long, aes(x = session, y = Volume, group = subjid, color = subjid, shape = Muscle)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), size = 3) +
  geom_line(aes(group = interaction(subjid, Muscle)), alpha = 0.5) +  # Connect ses-01 and ses-02 with lines
  scale_color_manual(values = rainbow(length(unique(als_data_long$subjid)))) + # Assign a unique color to each subject ID
  scale_shape_manual(values = 0:4) +  # Distinct shapes for different muscles
  facet_wrap(~Muscle, scales = "free_y") +  # Separate plots for each muscle and total volume
  labs(title = "Longitudinal Changes in Tongue Muscle Volumes Between Sessions",
       x = "Session",
       y = "Volume (corrected)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the ggplot
p




######### same thing without IOC for sanity


# Calculate Total Volume for each session if not already included
als_data <- df_clean %>%
  mutate(Total.vol = SLong.vol + Genio.vol + ILong.vol + Trans.vol) %>%
  group_by(subjid) %>%
  arrange(subjid, session) %>%
  # Ensure only subjects with both ses-01 and ses-02 are included
  filter(length(unique(session)) == 2) %>%
  filter(!is.na(SLong.vol) & !is.na(Genio.vol) & !is.na(ILong.vol) & !is.na(Trans.vol)) %>%
  filter(formal_diagnosis_numeric == "ALS") %>% 
  select(subjid, session, SLong.vol, Genio.vol, ILong.vol, Trans.vol, Total.vol)
# Reshape data for easier plotting (from wide to long format)
als_data_long <- als_data %>%
  pivot_longer(cols = c(SLong.vol, Genio.vol, ILong.vol, Trans.vol, Total.vol),
               names_to = "Muscle", values_to = "Volume")

# Create ggplot object
p <- ggplot(als_data_long, aes(x = session, y = Volume, group = subjid, color = subjid, shape = Muscle)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), size = 3) +
  geom_line(aes(group = interaction(subjid, Muscle)), alpha = 0.5) +  # Connect ses-01 and ses-02 with lines
  scale_color_manual(values = rainbow(length(unique(als_data_long$subjid)))) + # Assign a unique color to each subject ID
  scale_shape_manual(values = 0:4) +  # Distinct shapes for different muscles
  facet_wrap(~Muscle, scales = "free_y") +  # Separate plots for each muscle and total volume
  labs(title = "Longitudinal Changes in Tongue Muscle Volumes Between Sessions",
       x = "Session",
       y = "Volume (corrected)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the ggplot
p

### results are fucked - need to look at new longitudinal processing for tongue volumes. 20240422