packages <- c("oro.nifti", "ggplot2", "gridExtra", "dplyr", "tidyr", "psych")
sapply(packages, function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
})
library(psych)
base_folder <- "~/OneDrive - The University of Queensland/Projects/Tongue_seg/test-retest-experiment/"
subjects <- c("009", "139")
runs <- c("01", "02")
raters <- c("FLR", "TBS", "MSH", "XZ")
labels <- c(1, 2, 3, 4)

calculate_dice <- function(segmentation1, segmentation2, label) {
  intersection = sum(segmentation1 == label & segmentation2 == label)
  size1 = sum(segmentation1 == label)
  size2 = sum(segmentation2 == label)
  dice = (2 * intersection) / (size1 + size2)
  return(dice)
}

generate_file_path <- function(subject, run, rater) {
  sprintf("%ssub-%s_ses-01_run-%s_T2w_labels_%s.nii.gz", base_folder, subject, run, rater)
}

# Placeholder to store results
results <- list()

# Iterate over subjects, runs, and raters to calculate Dice coefficients
for (subject in subjects) {
  for (run in runs) {
    for (label in labels) {
      segmentations <- list()
      
      # Read segmentations for all raters
      for (rater in raters) {
        file_path <- generate_file_path(subject, run, rater)
        if (file.exists(file_path)) {
          segmentations[[rater]] <- readNIfTI(file_path, reorient = FALSE)
        } else {
          cat("File not found:", file_path, "\n")  # Print a message if the file is not found
        }
      }
      
      # Inter-rater reliability
      for (i in 1:length(raters)) {
        for (j in (i+1):length(raters)) {
          # Ensure both segmentations exist before calculating Dice score
          if (!is.null(segmentations[[raters[i]]]) && !is.null(segmentations[[raters[j]]])) {
            dice_score <- calculate_dice(segmentations[[raters[i]]], segmentations[[raters[j]]], label)
            result_key <- paste("Inter", subject, run, "Label", label, raters[i], "vs", raters[j], sep="_")
            results[[result_key]] <- dice_score
          }
        }
      }
      
      # Intra-rater reliability (between runs for the same subject)
      if (length(runs) > 1) {
        for (rater in raters) {
          file_path_run1 <- generate_file_path(subject, runs[1], rater)
          file_path_run2 <- generate_file_path(subject, runs[2], rater)
          if (file.exists(file_path_run1) && file.exists(file_path_run2)) {
            segmentation_run1 <- readNIfTI(file_path_run1, reorient = FALSE)
            segmentation_run2 <- readNIfTI(file_path_run2, reorient = FALSE)
            dice_score <- calculate_dice(segmentation_run1, segmentation_run2, label)
            result_key <- paste("Intra", subject, "Runs", runs[1], "vs", runs[2], "Label", label, rater, sep="_")
            results[[result_key]] <- dice_score
          }
        }
      }
    }
  }
}


results_df <- do.call(rbind, lapply(names(results), function(x) {
  parts <- unlist(strsplit(x, "_"))
  type <- parts[1]
  subject <- parts[2]
  
  # Common adjustments for both types
  run <- ifelse(type == "Inter", parts[3], NA)
  label <- as.numeric(parts[which(parts == "Label") + 1])
  
  # Adjusting the construction of the Comparison column
  if (type == "Inter") {
    # Identify indices for raters involved in the comparison
    rater_indices <- which(parts %in% c("FLR", "TBS", "MSH", "XZ"))
    if(length(rater_indices) == 2) { # Ensure there are exactly two raters identified
      comparison <- paste(parts[rater_indices[1]], "vs", parts[rater_indices[2]])
    } else {
      comparison <- "Rater Comparison Error" # Placeholder in case of unexpected format
    }
  } else { # Intra
    comparison <- "Session 1 vs Session 2" # Assuming a standard comparison for Intra
  }
  
  dice_score <- results[[x]]
  
  data.frame(
    Type = type,
    Subject = subject,
    Run = run,
    Label = label,
    Comparison = comparison,
    Dice_Score = dice_score,
    stringsAsFactors = FALSE
  )
}))
rownames(results_df) <- NULL

# Filter out rows with Label 0
results_df <- results_df %>% 
  dplyr::filter(Label != 0)


# Save results to CSV
write.csv(results_df, "dice_scores_reliability.csv", row.names = FALSE)
# Assuming 'results_df' has already been filtered to exclude Label 0

# Adjusted calculation for Image One and Image Two based on 'Subject'
# Here, it's assumed '009' and '139' represent two different "images" or subjects for intra-rater analysis
table_ready_df <- results_df %>%
  filter(Type == "Intra") %>%
  group_by(Label) %>%
  summarise(`Subject 009 Avg Dice` = mean(Dice_Score[Subject == "009"]),
            `Subject 139 Avg Dice` = mean(Dice_Score[Subject == "139"]), .groups = 'drop') %>%
  arrange(Label)

# Since this table is intended for visualization, let's rename the columns to match your desired output
colnames(table_ready_df) <- c("Label", "Image One Avg Dice", "Image Two Avg Dice")
library(gridExtra)

# Define file path for saving the PNG
file_path <- paste0(base_folder, "intra_rater_reliability_table.png")

# Save the table as a PNG file
png(file_path, width = 800, height = 400)  # Adjust dimensions as needed
grid.table(table_ready_df)
dev.off()


### Add more detailed intra-rater reliability analysis
# Filter for Intra-rater reliability scores
intra_rater_df <- results_df %>%
  filter(Type == "Intra") %>%
  select(Subject, Label, Comparison, Dice_Score)
file_path <- paste0(base_folder, "intra_rater_all_dice.csv")
write.csv(intra_rater_df, file_path, row.names = FALSE)

##I manually added an extra column for Rater in the csv...

# Read the CSV
df <- read.csv(file_path)

# Assuming 'df' is already loaded with your data
# If 'Image' needs to be treated as a factor for ordering or other purposes, ensure it's done correctly:
df$Image <- factor(df$image, levels = unique(df$image))

# Round the Dice scores (assuming this is already done as per your snippet)
df$Dice_Score <- round(df$Dice_Score, 2) # Already rounded based on your data snippet

# Reshape the data for summary, assuming 'df' is your current dataframe
df_summary <- df %>%
  group_by(Image, Label) %>%
  pivot_wider(names_from = Rater, values_from = Dice_Score, names_prefix = "Rater ") %>%
  arrange(Image, Label) %>%
  ungroup()

# Ensure correct rounding (if additional rounding needed)
df_summary <- df_summary %>%
  mutate(across(starts_with("Rater "), round, 2))

# Convert 'Image' back to a factor if it was manipulated or if ordering needs to be enforced
df_summary$Image <- factor(df_summary$Image, levels = unique(df_summary$Image))

# Generate the table for visualization
table <- tableGrob(df_summary, rows = NULL)
file_path <- paste0(base_folder, "intra_rater_reliability_summary_table.png")
# Save or display the table
png(file_path, width = 12, height = 6, units = "in", res = 120)
grid.draw(table)
dev.off()

library(oro.nifti)

# Function to read in NIfTI data for a given subject, session, and rater
read_nifti_data <- function(subject, session, rater) {
  file_path <- generate_file_path(subject, session, rater)
  if (file.exists(file_path)) {
    return(readNIfTI(file_path, reorient = FALSE))
  } else {
    warning("File not found:", file_path)
    return(NULL)
  }
}

# Function to generate file path
generate_file_path <- function(subject, session, rater) {
  file.path(base_folder, sprintf("sub-%s_ses-01_run-%s_T2w_labels_%s.nii.gz", subject, session, rater))
}

# Initialize a dataframe to store the results
dice_comparisons <- data.frame(
  Comparison = character(),
  Label = integer(),
  Image = character(),
  Session = character(),
  DiceScore = numeric(),
  stringsAsFactors = FALSE
)

# Iterate over the combinations and calculate Dice overlap
for (subject in subjects) {
  for (session in runs) {
    for (rater1 in raters) {
      for (rater2 in raters) {
        if (rater1 != rater2) { # Ensure we don't compare the same rater
          # Read in the data for the two raters for the same session
          segmentation1 <- read_nifti_data(subject, session, rater1)
          segmentation2 <- read_nifti_data(subject, session, rater2)
          
          # Calculate Dice score for each label if both segmentations are successfully read
          if (!is.null(segmentation1) && !is.null(segmentation2)) {
            for (label in labels) {
              dice_score <- calculate_dice(segmentation1, segmentation2, label)
              comparison_name <- paste(rater1, "vs", rater2, sep="_")
              session_name <- paste("Session", session, sep="_")
              
              # Store the results in the dataframe
              dice_comparisons <- rbind(dice_comparisons, data.frame(
                Comparison = comparison_name,
                Label = label,
                Image = subject,
                Session = session_name,
                DiceScore = dice_score
              ))
            }
          }
        }
      }
    }
  }
}

# View the results
print(dice_comparisons)

# Optionally save to CSV
write.csv(dice_comparisons, file.path(base_folder, "dice_comparisons.csv"), row.names = FALSE)
