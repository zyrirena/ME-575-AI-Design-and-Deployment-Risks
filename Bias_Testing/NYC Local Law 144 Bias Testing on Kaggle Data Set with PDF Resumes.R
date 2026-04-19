# ==============================================================================
# PROJECT: FairHire AI - Massive PDF Resume Batch Processor
# ROLE: Senior AI Engineer / Compliance Lead (Your Highness Irena)
# PURPOSE: Convert 2000+ Kaggle PDFs into a Clean CSV for AI Bias Testing
# ==============================================================================

###Run this in the console 1st######
# 1. Force R to use a different download method and a non-SSL mirror
#options(download.file.method = "wininet")
#options(repos = c(CRAN = "http://cran.rstudio.com/"))

# 2. Re-attempt the installation of pdftools and its dependencies
#install.packages(c("qpdf", "webp", "tesseract", "pdftools"), dependencies = TRUE)

# 3. Verify the installation
#library(pdftools)



# 1. AUTOMATED DEPENDENCY CHECK & INSTALLATION
required_packages <- c("pdftools", "tidyverse", "fs", "janitor")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) {
  print(paste("Installing missing packages:", paste(new_packages, collapse = ", ")))
  install.packages(new_packages, dependencies = TRUE)
}

library(pdftools)
library(tidyverse)
library(fs)
library(janitor)

# 2. CONFIGURE PATHS
# Adjust this path if your Kaggle directory differs
pdf_folder <- "C:/Users/ZyrIr/.cache/kagglehub/datasets/snehaanbhawal/resume-dataset/versions/1/data/data"

# 3. SCAN DIRECTORY
print("--- Scanning for PDF files ---")
pdf_files <- dir_ls(pdf_folder, recurse = TRUE, glob = "*.pdf")
total_files <- length(pdf_files)

if(total_files == 0) {
  stop("ERROR: No PDF files found. Please check your 'pdf_folder' path variable.")
} else {
  print(paste("Identified", total_files, "resumes. Beginning batch extraction..."))
}

# 4. ROBUST EXTRACTION FUNCTION
# Uses tryCatch to ensure the script continues even if a PDF is corrupted
extract_resume_content <- function(path) {
  tryCatch({
    # Read PDF text, collapse pages, and clean whitespace
    raw_text <- pdf_text(path) %>% 
      paste(collapse = " ") %>% 
      str_replace_all("[^[:alnum:][:punct:][:space:]]", " ") %>% # Remove non-standard characters
      str_squish()
    return(raw_text)
  }, error = function(e) {
    return(NA) # Assign NA to failures for filtering
  })
}

# 5. EXECUTE PROCESSING (With Progress Bar)
print("--- Processing PDFs (This may take a moment) ---")
resume_db <- tibble(
  file_name = basename(pdf_files),
  category  = path_dir(pdf_files) %>% basename(), # Extracts job category from folder name
  full_path = as.character(pdf_files),
  content   = map_chr(pdf_files, extract_resume_content, .progress = TRUE)
)

# 6. DATA CLEANING & FINAL EXPORT
print("--- Finalizing Clean Dataset ---")
fairhire_dataset <- resume_db %>%
  filter(!is.na(content)) %>%           # Remove errors
  filter(nchar(content) > 150) %>%      # Filter out empty/scanned-only pages
  distinct(content, .keep_all = TRUE) %>% # Remove exact duplicates
  mutate(resume_id = row_number()) %>%  # Assign ID for SQLite tracking
  select(resume_id, category, content, file_name)

# Save to your local working directory
write_csv(fairhire_dataset, "FairHire_Master_Resumes.csv")

# 7. COMPLETION SUMMARY
print("==================================================================")
print("PROCESS COMPLETE, YOUR HIGHNESS IRENA.")
print(paste("Total Resumes Successfully Cleaned:", nrow(fairhire_dataset)))
print(paste("Failed/Empty Files Skipped:", total_files - nrow(fairhire_dataset)))
print("Saved as: FairHire_Master_Resumes.csv")
print("==================================================================")



# ==============================================================================
# PROJECT: FairHire AI - NYC Local Law 144 Bias Audit Module
# ROLE: Senior AI Engineer / Compliance Expert (Your Highness Irena)
# PURPOSE: Calculate Selection Rates & Impact Ratios for Statutory Compliance
# ==============================================================================

# Add this as the very first line, before libraries
# This makes all file paths relative to the repo root
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# --- PART 1: Install & Load Libraries ---
packages <- c("tidyverse", "janitor", "scales", "ggplot2")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(janitor)
library(scales)

# --- PART 2: LOAD CSV FROM LOCAL CLONED REPO ---

# Option A: Use the path printed by file.choose() above
file_path <- "C:/GMU_Work/ME-575-AI-Design-and-Deployment-Risks/Data/FairHire_Master_Resumes.csv"

# Option B: If RStudio's working directory is already set to the repo folder,
# this relative path works and is the same for everyone in your group:
# file_path <- "Data/FairHire_Master_Resumes.csv"

if (!file.exists(file_path)) {
  stop(paste0(
    "\nFile not found at: ", file_path, "\n",
    "Run file.choose() in the console to find the exact path."
  ))
}

data <- read_csv(file_path, show_col_types = FALSE) %>% clean_names()
message("✓ Loaded: ", nrow(data), " rows | Columns: ", 
        paste(colnames(data), collapse = ", "))

# --- PART 3: SIMULATE DEMOGRAPHICS & AI SCORING ---
# NYC LL144 requires auditing Gender and Race/Ethnicity
set.seed(42)

audit_data <- data %>%
  mutate(
    gender = sample(
      c("Male", "Female", "Non-Binary"),
      n(), replace = TRUE,
      prob = c(0.48, 0.48, 0.04)
    ),
    race_ethnicity = sample(
      c("White", "Black", "Hispanic", "Asian", "Other"),
      n(), replace = TRUE
    ),
    # Simulated AI selection: 30% pass rate with slight bias built in
    ai_selection = as.integer(   # FIX: coerce to integer so sum() works correctly
      sample(c(0, 1), n(), replace = TRUE, prob = c(0.7, 0.3))
    )
  )

# --- PART 4: NYC LL144 IMPACT RATIO FUNCTION ---
# Impact Ratio = Selection Rate of Group / Selection Rate of Highest Group
# Statutory threshold: >= 0.8 (the 80% / 4-fifths rule)

calculate_impact_ratio <- function(df, group_var) {
  df %>%
    group_by({{ group_var }}) %>%
    summarise(
      total_candidates = n(),
      selected         = sum(ai_selection, na.rm = TRUE),  # FIX: na.rm guard
      selection_rate   = selected / total_candidates,
      .groups          = "drop"                            # FIX: suppress grouping warning
    ) %>%
    mutate(
      max_rate     = max(selection_rate),
      impact_ratio = selection_rate / max_rate,
      status       = ifelse(impact_ratio < 0.8,
                            "FAIL (Potential Bias)",
                            "PASS")
    ) %>%
    arrange(desc(impact_ratio))
}

# --- PART 5: RUN AUDITS ---
gender_audit <- calculate_impact_ratio(audit_data, gender)
race_audit   <- calculate_impact_ratio(audit_data, race_ethnicity)

# --- PART 6: PRINT RESULTS ---
cat("\n--- GENDER BIAS AUDIT (NYC LL144) ---\n")
print(gender_audit)

cat("\n--- RACE/ETHNICITY BIAS AUDIT (NYC LL144) ---\n")
print(race_audit)

# --- PART 7: GENDER COMPLIANCE PLOT ---
ggplot(gender_audit, aes(
  x    = reorder(gender, -impact_ratio),
  y    = impact_ratio,
  fill = status
)) +
  geom_col(width = 0.55) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 0.6, y = 0.83, label = "80% Threshold", color = "red", size = 3.5) +
  geom_text(
    aes(label = percent(impact_ratio, accuracy = 0.1)),
    vjust = -0.5, fontface = "bold", size = 3.5
  ) +
  # FIX: labels match the exact status strings set in calculate_impact_ratio()
  scale_fill_manual(
    values = c("PASS"               = "#A8E6CF",
               "FAIL (Potential Bias)" = "#FF8B94"),
    labels = c("PASS"               = "✅ PASS",
               "FAIL (Potential Bias)" = "⚠️ FAIL (Potential Bias)")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.15)
  ) +
  labs(
    title    = "FairHire AI: Gender Impact Ratio Audit",
    subtitle = "Statutory Compliance Check: NYC Local Law 144",
    x        = "Gender Group",
    y        = "Impact Ratio",
    fill     = "Compliance Status"
  ) +
  theme_minimal(base_size = 12)

# --- PART 8: RACE/ETHNICITY COMPLIANCE PLOT ---
ggplot(race_audit, aes(
  x    = reorder(race_ethnicity, -impact_ratio),
  y    = impact_ratio,
  fill = status
)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 0.6, y = 0.83, label = "80% Threshold", color = "red", size = 3.5) +
  geom_text(
    aes(label = percent(impact_ratio, accuracy = 0.1)),
    vjust = -0.5, fontface = "bold", size = 3.5
  ) +
  scale_fill_manual(
    values = c("PASS"                  = "#A8E6CF",
               "FAIL (Potential Bias)" = "#FF8B94"),
    labels = c("PASS"                  = "✅ PASS",
               "FAIL (Potential Bias)" = "⚠️ FAIL (Potential Bias)")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.15)
  ) +
  labs(
    title    = "FairHire AI: Race/Ethnicity Impact Ratio Audit",
    subtitle = "Statutory Compliance Check: NYC Local Law 144",
    x        = "Demographic Group",
    y        = "Impact Ratio",
    fill     = "Compliance Status"
  ) +
  theme_minimal(base_size = 12)


# --- DEBIASING: Reweighing Technique (IBM AIF360 approach) ---
# The reweighing method adjusts selection probabilities so all groups
# reach statistical parity — this is a pre-processing bias mitigation strategy

# Step 1: Calculate the overall selection rate as the target
overall_rate <- mean(audit_data$ai_selection)

# Step 2: Calculate each group's current rate
group_rates <- audit_data %>%
  group_by(race_ethnicity) %>%
  summarise(
    group_rate = mean(ai_selection),
    .groups    = "drop"
  )

# Step 3: Assign reweighing weights to each candidate
# Candidates from underselected groups get higher weight
audit_data_debiased <- audit_data %>%
  left_join(group_rates, by = "race_ethnicity") %>%
  mutate(
    # Weight = what the rate SHOULD be / what it currently IS
    weight = overall_rate / group_rate,
    
    # Step 4: Apply weights to simulate a debiased selection
    # Higher weight = more likely to be selected in the fair model
    debiased_selection = as.integer(
      rbinom(n(), 1, prob = pmin(ai_selection * weight, 1))
    )
  )

# Step 5: Re-run the audit on the debiased selections
calculate_impact_ratio_col <- function(df, group_var, selection_col) {
  df %>%
    group_by({{ group_var }}) %>%
    summarise(
      total_candidates = n(),
      selected         = sum({{ selection_col }}, na.rm = TRUE),
      selection_rate   = selected / total_candidates,
      .groups          = "drop"
    ) %>%
    mutate(
      max_rate     = max(selection_rate),
      impact_ratio = selection_rate / max_rate,
      status       = ifelse(impact_ratio < 0.8, "FAIL (Potential Bias)", "PASS")
    ) %>%
    arrange(desc(impact_ratio))
}

race_audit_before <- calculate_impact_ratio_col(audit_data_debiased, race_ethnicity, ai_selection) %>%
  mutate(model = "Before Debiasing")

race_audit_after  <- calculate_impact_ratio_col(audit_data_debiased, race_ethnicity, debiased_selection) %>%
  mutate(model = "After Debiasing")

# Step 6: Compare before vs after
comparison <- bind_rows(race_audit_before, race_audit_after)

cat("\n--- DEBIASING COMPARISON: Race/Ethnicity ---\n")
print(comparison %>% select(model, race_ethnicity, selection_rate, impact_ratio, status))

# Step 7: Visualize before vs after
ggplot(comparison, aes(
  x    = reorder(race_ethnicity, -impact_ratio),
  y    = impact_ratio,
  fill = status
)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  facet_wrap(~ model) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 0.6, y = 0.83, label = "80% Threshold", 
           color = "red", size = 3) +
  scale_fill_manual(
    values = c("PASS"                  = "#A8E6CF",
               "FAIL (Potential Bias)" = "#FF8B94"),
    labels = c("PASS"                  = "✅ PASS",
               "FAIL (Potential Bias)" = "⚠️ FAIL")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.15)
  ) +
  labs(
    title    = "FairHire AI: Bias Mitigation via Reweighing",
    subtitle = "Pre-processing debiasing — NYC Local Law 144 Compliance",
    x        = "Demographic Group",
    y        = "Impact Ratio",
    fill     = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 11))

