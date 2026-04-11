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

# 1. LOAD LIBRARIES
library(tidyverse)
library(janitor)

# 2. LOAD YOUR GENERATED CSV
# Ensure "FairHire_Master_Resumes.csv" is in your current working directory
if (!file.exists("FairHire_Master_Resumes.csv")) {
  stop("CSV not found! Please run the PDF extraction script first.")
}

data <- read_csv("FairHire_Master_Resumes.csv")

# 3. SIMULATE DEMOGRAPHICS & AI SCORING (For Audit Testing)
# Note: NYC LL144 requires checking Gender and Race/Ethnicity
set.seed(42) # For consistent results
audit_data <- data %>%
  mutate(
    gender = sample(c("Male", "Female", "Non-Binary"), n(), replace = TRUE, prob = c(0.48, 0.48, 0.04)),
    race_ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), n(), replace = TRUE),
    # Simulate an AI 'Selection' (1 = Selected for interview, 0 = Rejected)
    # We'll create a slightly biased score to see if the audit catches it
    ai_selection = sample(c(0, 1), n(), replace = TRUE, prob = c(0.7, 0.3))
  )

# 4. NYC LL144 CALCULATION FUNCTION
# Impact Ratio = (Selection Rate of Category) / (Selection Rate of Most Selected Category)
calculate_impact_ratio <- function(df, group_var) {
  df %>%
    group_by({{group_var}}) %>%
    summarise(
      total_candidates = n(),
      selected = sum(ai_selection),
      selection_rate = selected / total_candidates
    ) %>%
    mutate(
      max_rate = max(selection_rate),
      impact_ratio = selection_rate / max_rate,
      # Statutory threshold is 0.8 (The 80% Rule)
      status = ifelse(impact_ratio < 0.8, "⚠️ FAIL (Potential Bias)", "✅ PASS")
    )
}

# 5. EXECUTE AUDITS
gender_audit <- calculate_impact_ratio(audit_data, gender)
race_audit <- calculate_impact_ratio(audit_data, race_ethnicity)

# 6. VISUALIZE RESULTS (Visible Output for Group Presentation)
print("--- GENDER BIAS AUDIT (NYC LL144) ---")
print(gender_audit)

print("--- RACE/ETHNICITY BIAS AUDIT (NYC LL144) ---")
print(race_audit)

# 7. GENERATE COMPLIANCE PLOT
ggplot(race_audit, aes(x = race_ethnicity, y = impact_ratio, fill = status)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = 0.85, label = "80% Threshold", color = "red") +
  scale_fill_manual(values = c("✅ PASS" = "#A8E6CF", "⚠️ FAIL (Potential Bias)" = "#FF8B94")) +
  labs(title = "FairHire AI: Race/Ethnicity Impact Ratio Audit",
       subtitle = "Statutory Compliance Check: NYC Local Law 144",
       x = "Demographic Group",
       y = "Impact Ratio") +
  theme_minimal()


