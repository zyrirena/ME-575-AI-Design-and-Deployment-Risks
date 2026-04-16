# =============================================================
# IBM AIF360 BIAS AUDIT - RSTUDIO (RETICULATE APPROACH)
# No broken R wrapper — uses Python aif360 directly via reticulate
# IMPORTANT: Run in a fresh R session (Ctrl+Shift+F10 first)
# =============================================================

# --- PART 1: Load reticulate first, before anything else ---
library(reticulate)

# --- PART 2: Confirm virtualenv is set (from your .Renviron) ---
# If you completed the one-time setup from the previous script,
# RETICULATE_PYTHON is already locked in via .Renviron.
# If not, run this one-time setup block in the console first:
#
library(reticulate)
install_python(version = "3.11:latest")
virtualenv_create("resume-audit", python = install_python("3.11:latest"))
virtualenv_install("resume-audit", c("kagglehub", "aif360", "pandas", "numpy", "scipy"))
writeLines(
paste0("RETICULATE_PYTHON=", virtualenv_python("resume-audit")),
con = file.path(Sys.getenv("HOME"), ".Renviron")
)
#   # Then restart R with Ctrl+Shift+F10

reticulate_python <- Sys.getenv("RETICULATE_PYTHON")

if (reticulate_python == "" || !file.exists(reticulate_python)) {
  stop(paste0(
    "\nRETICULATE_PYTHON is not set or points to a missing file.\n",
    "Please run the one-time setup block above, then restart R.\n",
    "Current value: '", reticulate_python, "'"
  ))
}

message("Python locked to: ", reticulate_python)

# --- PART 3: Install aif360 into the virtualenv if not already there ---
tryCatch({
  import("aif360")
  message("✓ aif360 already installed.")
}, error = function(e) {
  message("Installing aif360 and dependencies into virtualenv...")
  virtualenv_install(
    "resume-audit",
    packages = c("aif360", "pandas", "numpy", "scipy", "matplotlib"),
    pip_options = "--quiet"
  )
  message("✓ aif360 installed.")
})

# --- PART 4: Load all R libraries ---
suppressPackageStartupMessages({
  packages <- c("tidyverse", "janitor", "data.table", "scales", "knitr", "kableExtra")
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(new_packages)) install.packages(new_packages)
  
  library(tidyverse)
  library(janitor)
  library(data.table)
  library(scales)
  library(knitr)
  library(kableExtra)
})

message("✓ R libraries loaded.")

# --- PART 5: Import Python AIF360 modules via reticulate ---
# This replaces the broken R wrapper entirely
pd          <- import("pandas")
np          <- import("numpy")
aif_metrics <- import("aif360.metrics")
aif_data_mod <- import("aif360.datasets")

message("✓ IBM AIF360 Python engine loaded via reticulate.")

# --- PART 6: Prepare audit_prep data ---
# If audit_prep doesn't exist yet (running standalone), build it here.
# If it already exists from your main script, this block is skipped.
if (!exists("audit_prep")) {
  message("audit_prep not found — building from dataset...")
  
  kh   <- import("kagglehub")
  path <- kh$dataset_download("rhythmghai/resume-screening-dataset-200k-candidates")
  
  all_files <- list.files(path, recursive = TRUE, full.names = TRUE)
  csv_file  <- all_files[str_detect(all_files, "\\.csv$")][1]
  if (is.na(csv_file)) stop("No CSV found in dataset path.")
  
  raw_data <- fread(csv_file)
  
  audit_prep <- raw_data %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(
      age_group = case_when(
        age < 25             ~ "Entry / Gen Z",
        age >= 25 & age < 40 ~ "Mid-Career / Millennial",
        age >= 40            ~ "Senior / Gen X+",
        TRUE                 ~ "Unknown"
      ),
      is_elite_school = ifelse(university_tier == "Tier 1", "Yes", "No"),
      hired = as.integer(hired)
    )
  
  message("✓ audit_prep built: ", nrow(audit_prep), " rows.")
} else {
  message("✓ Using existing audit_prep: ", nrow(audit_prep), " rows.")
}

# --- PART 7: Convert to AIF360-compatible format via Python ---
# AIF360 needs a pandas DataFrame with numeric protected attributes

# Map age_group to numeric codes for AIF360
age_group_map <- c(
  "Entry / Gen Z"           = 0L,
  "Mid-Career / Millennial" = 1L,   # privileged group
  "Senior / Gen X+"         = 2L,
  "Unknown"                 = -1L
)

aif_input <- audit_prep %>%
  filter(age_group != "Unknown") %>%
  mutate(
    age_group_code = as.integer(age_group_map[age_group]),
    hired          = as.integer(hired)
  ) %>%
  select(hired, age_group_code) %>%
  drop_na()

# Convert R data frame to pandas DataFrame
pdf <- r_to_py(aif_input)

# Build AIF360 BinaryLabelDataset
BinaryLabelDataset <- import("aif360.datasets")$BinaryLabelDataset

aif_dataset <- BinaryLabelDataset(
  df                       = pdf,
  label_names              = list("hired"),
  protected_attribute_names = list("age_group_code"),
  favorable_label          = 1,
  unfavorable_label        = 0
)

message("✓ AIF360 BinaryLabelDataset created.")

# --- PART 8: Calculate Fairness Metrics ---
# Privileged = Mid-Career/Millennial (code 1)
# Unprivileged groups to test against privileged

BinaryLabelDatasetMetric <- aif_metrics$BinaryLabelDatasetMetric

privileged_groups   <- list(list(age_group_code = 1L))  # Mid-Career

# Test each unprivileged group separately
unprivileged_groups_list <- list(
  "Entry / Gen Z"   = list(list(age_group_code = 0L)),
  "Senior / Gen X+" = list(list(age_group_code = 2L))
)

results <- map_dfr(names(unprivileged_groups_list), function(group_name) {
  metric_obj <- BinaryLabelDatasetMetric(
    dataset             = aif_dataset,
    unprivileged_groups = unprivileged_groups_list[[group_name]],
    privileged_groups   = privileged_groups
  )
  
  spd  <- metric_obj$statistical_parity_difference()
  disp <- metric_obj$disparate_impact()
  
  tibble(
    unprivileged_group        = group_name,
    privileged_group          = "Mid-Career / Millennial",
    statistical_parity_diff   = round(spd,  4),
    disparate_impact_ratio    = round(disp, 4),
    spd_status  = ifelse(abs(spd)  < 0.1,  "✅ PASS", "⚠️ FAIL"),
    di_status   = ifelse(disp >= 0.8,       "✅ PASS", "⚠️ FAIL")
  )
})

# --- PART 9: Print Results ---
print("--- IBM AIF360 FAIRNESS AUDIT RESULTS ---")
print(results)

cat("\nInterpretation:\n")
cat("  Statistical Parity Difference (SPD): Should be close to 0 (threshold: ±0.1)\n")
cat("  Disparate Impact Ratio (DIR):        Should be >= 0.8 (the 4/5ths rule)\n")

# --- PART 10: Visualize Results ---
results_long <- results %>%
  select(unprivileged_group, statistical_parity_diff, disparate_impact_ratio) %>%
  pivot_longer(
    cols      = c(statistical_parity_diff, disparate_impact_ratio),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(metric,
                    "statistical_parity_diff"  = "Statistical Parity Difference",
                    "disparate_impact_ratio"   = "Disparate Impact Ratio"
    ),
    passes = case_when(
      metric == "Statistical Parity Difference" ~ abs(value) < 0.1,
      metric == "Disparate Impact Ratio"        ~ value >= 0.8,
      TRUE ~ TRUE
    )
  )

ggplot(results_long, aes(x = unprivileged_group, y = value, fill = passes)) +
  geom_col(width = 0.6) +
  facet_wrap(~ metric, scales = "free_y") +
  geom_hline(
    data = tibble(
      metric    = c("Statistical Parity Difference", "Disparate Impact Ratio"),
      threshold = c(0, 0.8)
    ),
    aes(yintercept = threshold),
    linetype = "dashed", color = "red", linewidth = 0.9
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#55efc4", "FALSE" = "#ff7675"),
    labels = c("TRUE" = "✅ PASS",  "FALSE" = "⚠️ FAIL")
  ) +
  labs(
    title    = "IBM AIF360 Fairness Metrics by Age Group",
    subtitle = "Privileged group: Mid-Career / Millennial | Red line = legal threshold",
    x        = "Unprivileged Group",
    y        = "Metric Value",
    fill     = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))