# ==============================================================================
# PROJECT: FairHire AI - Comprehensive Bias Audit Portfolio
# Group 4: Risk Ready
# PURPOSE: Statutory Compliance Proof (NYC LL 144 & EU AI Act)
# DATASET: 200k Candidate Resumes (Rhythm Ghai)
# ==============================================================================

# 1. INSTALL & LOAD DEPENDENCIES
required_packages <- c("reticulate", "tidyverse", "data.table", "janitor", "scales")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(reticulate)
library(tidyverse)
library(data.table)
library(janitor)
library(scales)

# 2. DOWNLOAD DATASET VIA KAGGLEHUB
py_install("kagglehub", pip = TRUE)
kh <- import("kagglehub")

print("--- Downloading 200k Candidate Dataset ---")
path <- kh$dataset_download("rhythmghai/resume-screening-dataset-200k-candidates")

# 3. LOCATE AND IMPORT CSV
all_files <- list.files(path, recursive = TRUE, full.names = TRUE)
csv_file <- all_files[str_detect(all_files, ".csv$")][1]

print(paste("Loading data from:", csv_file))
raw_data <- fread(csv_file) 

# 4. DATA CLEANING & STATUTORY FEATURE ENGINEERING
audit_prep <- raw_data %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(
    # EU AI ACT: Categorizing age to test for ageism
    age_group = case_when(
      age < 25 ~ "Entry/Gen Z",
      age >= 25 & age < 40 ~ "Mid-Career/Millennial",
      age >= 40 ~ "Senior/Gen X+"
    ),
    # PROXY BIAS: Categorizing university tier
    is_elite_school = ifelse(university_tier == "Tier 1", "Yes", "No")
  )

# 5. AUDIT 1: EDUCATION LEVEL (NYC LL 144 Compliance)
edu_audit <- audit_prep %>%
  group_by(education_level) %>%
  summarise(
    total = n(),
    selected = sum(hired),
    selection_rate = selected / total
  ) %>%
  mutate(
    impact_ratio = selection_rate / max(selection_rate),
    status = ifelse(impact_ratio < 0.8, "⚠️ FAIL (Bias Detected)", "✅ PASS")
  )

# 6. AUDIT 2: AGE GROUP (EU AI Act Compliance)
age_audit <- audit_prep %>%
  group_by(age_group) %>%
  summarise(
    total = n(),
    selected = sum(hired),
    selection_rate = selected / total
  ) %>%
  mutate(
    impact_ratio = selection_rate / max(selection_rate),
    status = ifelse(impact_ratio < 0.8, "⚠️ FAIL", "✅ PASS")
  )

# 7. VISUAL 1: EDUCATION IMPACT CHART
compliance_plot <- ggplot(edu_audit, aes(x = reorder(education_level, -impact_ratio), 
                                         y = impact_ratio, fill = status)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "#D63031", size = 1.2) +
  geom_text(aes(label = round(impact_ratio, 2)), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("✅ PASS" = "#55efc4", "⚠️ FAIL (Bias Detected)" = "#ff7675")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.1)) +
  labs(title = "NYC Law 144: Education Level Impact Ratio",
       subtitle = "Benchmark: Highest Group | Statutory Threshold: 0.8",
       x = "Education Level", y = "Impact Ratio", fill = "Audit Status") +
  theme_minimal(base_size = 14)

# 8. VISUAL 2: AGE SELECTION DENSITY
age_dist_plot <- ggplot(audit_prep, aes(x = age, fill = factor(hired))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("0" = "#dfe6e9", "1" = "#0984e3"), 
                    labels = c("Rejected", "Hired")) +
  labs(title = "FairHire AI: Selection Density by Age",
       subtitle = "Audit for Age Discrimination (EU AI Act)",
       x = "Candidate Age", y = "Density", fill = "Outcome") +
  theme_minimal(base_size = 14)

# 9. VISUAL 3: PROXY BIAS (UNIVERSITY TIER)
tier_plot <- ggplot(audit_prep %>% group_by(university_tier) %>% 
                      summarise(rate = mean(hired)), 
                    aes(x = university_tier, y = rate, color = university_tier)) +
  geom_point(size = 6) +
  geom_segment(aes(x=university_tier, xend=university_tier, y=0, yend=rate), size = 1.2) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Proxy Bias Check: University Tier Impact",
       subtitle = "Analyzing Selection Rates by School Ranking",
       x = "University Tier", y = "Selection Rate") +
  theme_minimal(base_size = 14) + theme(legend.position = "none")

# 10. DISPLAY AND SAVE EVERYTHING
print(compliance_plot)
print(age_dist_plot)
print(tier_plot)

# Save results to the current folder
write_csv(edu_audit, "FairHire_Education_Audit.csv")
write_csv(age_audit, "FairHire_Age_Audit.csv")
ggsave("Audit_Education_Impact.png", compliance_plot, width = 10, height = 6, dpi = 300)
ggsave("Audit_Age_Density.png", age_dist_plot, width = 10, height = 6, dpi = 300)
ggsave("Audit_University_Tier.png", tier_plot, width = 10, height = 6, dpi = 300)

print("--- ALL AUDITS COMPLETE AND SAVED TO FOLDER ---")