# ==============================================================================
# PROJECT: FairHire AI - 200k Bias Audit
# Group 4: Risk Ready
# PURPOSE: Run bias testing to ensure compliance and provide audit proof for 
# NYC 144 Law Audit Report
# ==============================================================================

# 1. INSTALL & LOAD DEPENDENCIES
required_packages <- c("reticulate", "tidyverse", "data.table", "janitor")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(reticulate)
library(tidyverse)
library(data.table)
library(janitor)

# 2. DOWNLOAD DATASET VIA KAGGLEHUB
# Note: Ensure kagglehub is installed in the Python environment
py_install("kagglehub", pip = TRUE)
kh <- import("kagglehub")

print("--- Downloading 200k Candidate Dataset ---")
path <- kh$dataset_download("rhythmghai/resume-screening-dataset-200k-candidates")

# 3. LOCATE AND IMPORT CSV (High Speed)
all_files <- list.files(path, recursive = TRUE, full.names = TRUE)
csv_file <- all_files[str_detect(all_files, ".csv$")][1]

print(paste("Loading data from:", csv_file))
raw_data <- fread(csv_file) #for fast loading of data

# 4. DATA CLEANING
# testing for age bias
# We map 'age' to groups to test for Age Bias (EU AI Act requirement)
audit_prep <- raw_data %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(
    age_group = case_when(
      age < 25 ~ "Entry/Gen Z",
      age >= 25 & age < 40 ~ "Mid-Career/Millennial",
      age >= 40 ~ "Senior/Gen X+"
    ),
    # Categorize university tier for proxy bias testing
    is_elite_school = ifelse(university_tier == "Tier 1", "Yes", "No")
  )

# 5. NYC LOCAL LAW 144 AUDIT: SELECTION RATE BY EDUCATION
# Impact Ratio = (Selection Rate of Group) / (Selection Rate of Highest Group)
edu_audit <- audit_prep %>%
  group_by(education_level) %>%
  summarise(
    total_candidates = n(),
    selected = sum(hired),
    selection_rate = selected / total_candidates
  ) %>%
  mutate(
    max_rate = max(selection_rate),
    impact_ratio = selection_rate / max_rate,
    status = ifelse(impact_ratio < 0.8, "⚠️ FAIL (Bias Detected)", "✅ PASS")
  )

# 6. VISUAL RESULT FOR PRESENTATION
print("==================================================================")
print("NYC 144 LAW COMPLIANCE REPORT: EDUCATION LEVEL")
print(edu_audit)
print("==================================================================")

# 7. GENERATE COMPLIANCE PLOT
ggplot(edu_audit, aes(x = education_level, y = impact_ratio, fill = status)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("✅ PASS" = "#A8E6CF", "⚠️ FAIL (Bias Detected)" = "#FF8B94")) +
  labs(title = "FairHire AI: Education Level Impact Ratio",
       subtitle = "Statutory Compliance Check (Threshold: 0.8)",
       x = "Education Level",
       y = "Impact Ratio") +
  theme_minimal()

# 8. SAVE AUDIT FOR PROJECT DOCUMENTATION
write_csv(edu_audit, "FairHire_200k_Audit_Results.csv")

# 9. LOAD GRAPHING LIBRARY
library(ggplot2)
library(scales) # For percentage formatting

# 10. CREATE THE STATUTORY COMPLIANCE PLOT
compliance_plot <- ggplot(edu_audit, aes(x = reorder(education_level, -impact_ratio), 
                                         y = impact_ratio, 
                                         fill = status)) +
  # Create Bars
  geom_bar(stat = "identity", width = 0.7, color = "white", size = 0.5) +
  
  # Add the "Four-Fifths Rule" Threshold Line (0.8)
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "#D63031", size = 1.2) +
  
  # Add labels to the bars for exact Impact Ratio values
  geom_text(aes(label = round(impact_ratio, 2)), vjust = -0.5, fontface = "bold", size = 4) +
  
  # Color palette: Soft Green for Pass, Soft Red for Fail
  scale_fill_manual(values = c("✅ PASS" = "#55efc4", "⚠️ FAIL (Bias Detected)" = "#ff7675")) +
  
  # Formatting Y-Axis as a percentage for clarity
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.1)) +
  
  # Add Professional Titles & Captions
  labs(
    title = "NYC Local Law 144: Bias Audit Impact Ratios",
    subtitle = "Analysis of Education Level Selection Parity (Benchmark: Highest Group)",
    x = "Education Level (Protected Category)",
    y = "Impact Ratio (%)",
    fill = "Audit Status",
    caption = paste("Audit conducted on:", Sys.Date(), "| Dataset Size: 200,000 | Tool: FairHire AI")
  ) +
  
  # Apply a Clean Corporate Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#2d3436"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# 11. DISPLAY THE PLOT
print(compliance_plot)

# 12. SAVE THE PLOT AS A PNG
ggsave("FairHire_Compliance_Chart.png", plot = compliance_plot, width = 10, height = 6, dpi = 300)

print("Visualization done! The graph 'FairHire_Compliance_Chart.png' has been saved to your folder.")