library(readxl)
library(dplyr)
library(tidyr)

# Load and filter data
data.dir <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/"
file.name <- "BeLong-aggregate-clin-neuropsyc-demographics_2025-for-tongue-paper-1.xlsx"
sheet.name <- "MND.Aggregate"

data <- read_excel(file.path(data.dir, file.name), sheet = sheet.name) %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  filter(!is.na(segmentation.id), segmentation.id != "NA") %>%
  mutate(segmentation.id = as.numeric(segmentation.id))


# ---- Identify ALSFRS-R-based bulbar exclusions early ----
alsfrsr_bulbar_exclusions <- data %>%
  filter(ALSFRS.R.Speech <= 2 | ALSFRS.R.Swallowing <= 2) %>%
  pull(segmentation.id) %>%
  unique()

# Store for later comparison
print("ALSFRS-R bulbar exclusions (raw):")
print(alsfrsr_bulbar_exclusions)

# Summary helpers
mean_sd <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return("NA (NA)")
  sprintf("%.2f (%.2f)", mean(x), sd(x))
}

mean_sd_decimals <- function(x, digits = 5) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return("NA (NA)")
  fmt <- paste0("%.", digits, "f (%.", digits, "f)")
  sprintf(fmt, mean(x), sd(x))
}

get_sex_counts <- function(sex_col) {
  m <- sum(sex_col == 0, na.rm = TRUE)
  f <- sum(sex_col == 1, na.rm = TRUE)
  sprintf("%dM / %dF", m, f)
}


############first part of the exclusion chart:
# Diagnosis and sex summary per dataset
dataset_summary <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  filter(!is.na(segmentation.id), segmentation.id != "NA") %>%
  mutate(
    diagnosis_group = case_when(
      formal.diagnosis.numeric == 0 ~ "Control",
      formal.diagnosis.numeric == 1 ~ "ALS",
      TRUE ~ "Other"
    ),
    sex_label = case_when(
      sex.numerical == 0 ~ "Male",
      sex.numerical == 1 ~ "Female",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(dataset, diagnosis_group, sex_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = c(diagnosis_group, sex_label),
    values_from = n,
    values_fill = 0
  )

print(dataset_summary)
dataset_counts <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  filter(!is.na(segmentation.id), segmentation.id != "NA") %>%
  mutate(segmentation.id = as.numeric(segmentation.id)) %>%
  group_by(dataset) %>%
  summarise(n_total = n(), .groups = "drop")

print(dataset_counts)

#second part, find out which participant failed the segmentation
# Identify participants in session 1 with missing tongue volume fields
missing_volumes <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  filter(!is.na(segmentation.id)) %>%
  mutate(segmentation.id = as.numeric(segmentation.id)) %>%
  filter(
    is.na(Trans.vol) |
      is.na(SLong.vol) |
      is.na(Genio.vol) |
      is.na(ILong.vol) |
      is.na(IOC.vol.mm3) |
      is.na(Total.vol)
  ) %>%
  select(dataset, segmentation.id, subjid,
         Trans.vol, SLong.vol, Genio.vol, ILong.vol, IOC.vol.mm3, Total.vol)

print(missing_volumes)

# Segmentation QC summary (session 1 only)
segmentation_check <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  mutate(
    missing_Total = is.na(Total.vol),
    missing_Trans  = is.na(Trans.vol),
    missing_SLong  = is.na(SLong.vol),
    missing_Genio  = is.na(Genio.vol),
    missing_ILong  = is.na(ILong.vol),
    missing_IOC    = is.na(IOC.vol.mm3)
  ) %>%
  group_by(dataset) %>%
  summarise(
    n_total = n(),
    n_missing_Total = sum(missing_Total),
    n_missing_any_volume = sum(missing_Trans | missing_SLong | missing_Genio | missing_ILong | missing_IOC),
    .groups = "drop"
  )

print(segmentation_check)



# ---- Master exclusion list for segmentation validation ----
# # Reconstruct the commented-out master exclusion list
# legacy_exclude_ids <- c(
#   0, 6, 7, 8, 9, 10, 15, 17, 18, 21, 22, 28, 29, 30, 32, 33, 38, 39, 40, 41, 42, 43, 44,
#   46, 47, 48, 51, 53, 61, 63, 67, 68, 69, 71, 72, 77, 78, 82, 84, 89, 90, 92, 96, 97, 98, 102, 109, 113,
#   114, 115, 116, 118, 119, 120, 121, 126, 129, 133, 135, 138, 141, 143, 144, 147, 149,
#   151, 154, 157, 158, 161, 168, 172, 176, 182, 185, 186, 187, 189, 191, 193, 196, 197,
#   198, 199, 200, 201, 194
# )

# === EXCLUSION LOGIC ===

# ---- Define exclusion categories ----
seg_fail_ids <- c(
  0, 7, 8, 10, 15, 18, 21, 22, 28, 29, 30, 33, 40, 41, 42, 46, 47, 48, 61, 63, 71, 72, 84, 96, 102, 116,
  119, 121, 126, 129, 133, 135, 138, 143, 144, 157, 161, 168, 176, 182, 185, 186, 193, 194, 198, 199, 200, 201
)
weight_outliers  <- c(89, 67) 
low_vol_outliers <- c(90, 6, 43, 44, 92, 51, 109, 38) 
##these are ones that havent already been excluded by segmentation failure or weight outliers
manual_bulbar_ids <- c(
  9, 17, 32, 39, 53, 68, 69, 77, 78, 82, 97, 98, 113, 114, 115, 118,
  120, 141, 147, 149, 151, 154, 158, 172, 187, 189, 191, 196, 197
)

# clin-val-outliers <- cseg_fail_ids <- c(0, 41, 46, 47, 48, 194, 71, 102, 7, 8, 10, 15, 18, 21, 22, 
#                                         8, 29, 30, 33, 40, 42, 61, 63, 84, 96, 116, 119, 121, 129,
#                                         133, 135, 138, 143, 144, 157, 161, 168, 176, 182, 185, 186,
#                                         193, 198, 199, 200, 201)

# ---- Step 1: segmentation-related exclusions ----
segmentation_related_exclusions <- union(seg_fail_ids, union(weight_outliers, low_vol_outliers))

# ---- Step 2: bulbar exclusions (ALSFRS-R ≤ 2) ----
bulbar_cutoff_ids <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  filter(!is.na(segmentation.id)) %>%
  mutate(segmentation.id = as.numeric(segmentation.id)) %>%
  filter(ALSFRS.R.Speech <= 2 | ALSFRS.R.Swallowing <= 2) %>%
  pull(segmentation.id) %>%
  unique()

print("Bulbar exclusions based on ALSFRS-R scores:")
print(bulbar_cutoff_ids)

# ---- Step 3: define manual bulbar list ----
# manual_bulbar_ids <- c(
#   9, 17, 32, 39, 53, 68, 69, 71, 72, 77, 78, 82, 97, 98, 102, 113, 114, 115, 118,
#   120, 126, 141, 147, 149, 151, 154, 158, 172, 187, 189, 191, 196, 197
# )
# ---- Step 4: Dataset-level breakdown of exclusion categories ----

# Ensure segmentation.id is numeric
data <- data %>%
  mutate(segmentation.id = as.numeric(segmentation.id))

# Helper to summarise counts per dataset
count_exclusions <- function(ids, label) {
  data %>%
    filter(MND.IMAGING.session == "ses-01") %>%
    filter(segmentation.id %in% ids) %>%
    group_by(dataset) %>%
    summarise(!!label := n(), .groups = "drop")
}

# Generate summaries
# Bulbar exclusions (manual)

seg_fail_summary       <- count_exclusions(seg_fail_ids, "n_seg_fail")
weight_outlier_summary <- count_exclusions(weight_outliers, "n_weight_outlier")
low_vol_summary        <- count_exclusions(low_vol_outliers, "n_low_vol")
manual_bulbar_summary <- count_exclusions(manual_bulbar_ids, "n_bulbar_manual")

# Combine into one table
exclusion_breakdown <- full_join(seg_fail_summary, weight_outlier_summary, by = "dataset") %>%
  full_join(low_vol_summary, by = "dataset") %>%
  full_join(manual_bulbar_summary, by = "dataset") %>%
  replace_na(list(
    n_seg_fail = 0,
    n_weight_outlier = 0,
    n_low_vol = 0,
    n_bulbar_manual = 0
  ))
print("Exclusion breakdown by dataset, note that n_seg_fail is a combination of poor QA and poor segmentation - see spreadsheet for breakdown")
print(exclusion_breakdown)

# ---- Detailed exclusion info ----

exclude_ids <- unique(c(
  seg_fail_ids,
  weight_outliers,
  low_vol_outliers,
  manual_bulbar_ids
))
exclusion_info <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  filter(!is.na(segmentation.id)) %>%
  mutate(segmentation.id = as.numeric(segmentation.id)) %>%
  filter(segmentation.id %in% exclude_ids) %>%
  mutate(
    Reason = case_when(
      segmentation.id %in% seg_fail_ids ~ "Segmentation error",
      segmentation.id %in% weight_outliers ~ "Weight outlier",
      segmentation.id %in% low_vol_outliers ~ "Low tongue volume",
      segmentation.id %in% manual_bulbar_ids ~ "Bulbar involvement (manual list)",
      TRUE ~ "Unclassified"
    )
  ) %>%
  select(dataset, segmentation.id, subjid,
         ALSFRS.R.Speech, ALSFRS.R.Swallowing, ALSFRS.R.Saliva,
         Reason) %>%
  arrange(dataset, Reason)

print(exclusion_info)
# === STEP 5: Dataset-level manual bulbar summary ===
manual_bulbar_summary <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  mutate(segmentation.id = as.numeric(segmentation.id)) %>%
  filter(segmentation.id %in% manual_bulbar_ids) %>%
  count(dataset, name = "n_manual_bulbar")

print(manual_bulbar_summary)

# === COMPILE EXCLUSIONS ===
all_exclude_ids <- union(seg_fail_ids, union(weight_outliers, union(low_vol_outliers, manual_bulbar_ids)))
non_bulbar_exclude_ids <- setdiff(all_exclude_ids, manual_bulbar_ids)

# === Add diagnosis grouping ===
add_diagnosis_category <- function(df) {
  df %>%
    mutate(
      diagnosis_group = case_when(
        formal.diagnosis.numeric == 0 ~ "Control",
        formal.diagnosis.numeric == 1 ~ "ALS",
        TRUE ~ "Mimics"
      )
    )
}

# === FILTERED DATASETS ===

# (1) Segmentation validation: exclude all flagged IDs
data_full_excluded <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  mutate(
    segmentation.id = as.numeric(segmentation.id),
      Trans.vol = as.numeric(na_if(Trans.vol, "NA")),
      SLong.vol = as.numeric(na_if(SLong.vol, "NA")),
      Genio.vol = as.numeric(na_if(Genio.vol, "NA")),
      ILong.vol = as.numeric(na_if(ILong.vol, "NA")),
      IOC.vol.mm3 = as.numeric(na_if(IOC.vol.mm3, "NA"))
  ) %>%
  filter(!is.na(segmentation.id), segmentation.id != "NA") %>%
  filter(!segmentation.id %in% all_exclude_ids) %>%
  add_diagnosis_category()

# (2) Clinical validation: exclude only non-bulbar segmentation-related participants
data_bulbar_included <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  mutate(
    segmentation.id = as.numeric(segmentation.id),
      Trans.vol = as.numeric(na_if(Trans.vol, "NA")),
      SLong.vol = as.numeric(na_if(SLong.vol, "NA")),
      Genio.vol = as.numeric(na_if(Genio.vol, "NA")),
      ILong.vol = as.numeric(na_if(ILong.vol, "NA")),
      IOC.vol.mm3 = as.numeric(na_if(IOC.vol.mm3, "NA"))
  ) %>%
  filter(!is.na(segmentation.id), segmentation.id != "NA") %>%
  filter(!segmentation.id %in% non_bulbar_exclude_ids) %>%
  add_diagnosis_category()

# === SUMMARY FUNCTION (by dataset AND diagnosis) ===
summarise_dataset <- function(df) {
  df %>%
    group_by(dataset, diagnosis_group) %>%
    summarise(
      `Age years (SD)` = mean_sd(age.at.scan),
      `Height cm (SD)` = mean_sd(height),
      `Weight Kg (SD)` = mean_sd(weight),
      `Sex (M/F)` = get_sex_counts(sex.numerical),
      `Trans. vol. IOC cor. mm3 (SD)` = mean_sd_decimals(Trans.vol / IOC.vol.mm3),
      `Trans. vol. mm3 (SD)` = mean_sd(Trans.vol),
      `S. long. vol IOC cor. mm3 (SD)` = mean_sd_decimals(SLong.vol / IOC.vol.mm3),
      `S. long. vol. mm3 (SD)` = mean_sd(SLong.vol),
      `Genio. vol. IOC cor. mm3 (SD)` = mean_sd_decimals(Genio.vol / IOC.vol.mm3),
      `Genio. vol. mm3 (SD)` = mean_sd(Genio.vol),
      `I. long vol. IOC cor. mm3 (SD)` = mean_sd_decimals(ILong.vol / IOC.vol.mm3),
      `I. long vol. mm3 (SD)` = mean_sd(ILong.vol),
      `IOC vol mm3 (SD)` = mean_sd_decimals(IOC.vol.mm3),
      `ALSFRS-R total (SD)` = mean_sd(ALSFRS.R.tot),
      `ALSFRS-R bulbar (SD)` = mean_sd(ALSFRS.R.bulbar),
      `Bulbar onset count` = sum(Onset.location.coded == 1, na.rm = TRUE),
      `Months since onset (SD)` = mean_sd(months.since.onset),
      .groups = "drop"
    )
}

# === OUTPUT TABLES ===
library(flextable)

# === Summary tables (long format, sorted) ===
summary_table_full_exclusions <- summarise_dataset(data_full_excluded) %>%
  arrange(dataset, diagnosis_group)

summary_table_bulbar_included <- summarise_dataset(data_bulbar_included) %>%
  arrange(dataset, diagnosis_group)

# === Write CSV outputs ===
write.csv(summary_table_full_exclusions,
          "/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Publications/2024_Tongue_segmentation_IOC/2025-CIBM/R1/summary_table_full_exclusions.csv",
          row.names = FALSE)

write.csv(summary_table_bulbar_included,
          "/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Publications/2024_Tongue_segmentation_IOC/2025-CIBM/R1/summary_table_bulbar_included.csv",
          row.names = FALSE)

# === Optional: Preview as flextables ===
ft_full <- flextable(summary_table_full_exclusions)
ft_bulbar <- flextable(summary_table_bulbar_included)

print(ft_full)
print(ft_bulbar)

# === STEP 6: Detailed participant-level exclusion info ===
exclusion_info <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  mutate(segmentation.id = as.numeric(segmentation.id)) %>%
  filter(segmentation.id %in% all_exclude_ids) %>%
  mutate(
    Reason = case_when(
      segmentation.id %in% seg_fail_ids ~ "Segmentation error",
      segmentation.id %in% weight_outliers ~ "Weight outlier",
      segmentation.id %in% low_vol_outliers ~ "Low tongue volume",
      segmentation.id %in% manual_bulbar_ids ~ "Bulbar involvement (manual list)",
      TRUE ~ "Unclassified"
    )
  ) %>%
  select(dataset, segmentation.id, subjid,
         ALSFRS.R.Speech, ALSFRS.R.Swallowing, ALSFRS.R.Saliva,
         Reason) %>%
  arrange(dataset, Reason)

#print(exclusion_info)

# === STEP 7: Confirm which bulbar cases were retained in clinical validation ===
bulbar_readded <- data %>%
  filter(MND.IMAGING.session == "ses-01") %>%
  mutate(segmentation.id = as.numeric(segmentation.id)) %>%
  filter(segmentation.id %in% manual_bulbar_ids) %>%
  filter(!segmentation.id %in% non_bulbar_exclude_ids) %>%
  select(segmentation.id, subjid, dataset,
         ALSFRS.R.Speech, ALSFRS.R.Swallowing, ALSFRS.R.Saliva,
         Trans.vol, SLong.vol, Genio.vol, ILong.vol, IOC.vol.mm3)

#print(bulbar_readded)

