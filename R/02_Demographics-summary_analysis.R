```r
# =============================================================================
# PROGRAM      : demographics_table.R
#
# PURPOSE      : Generate a demographic and baseline characteristics table
#                for the Full Analysis Set (FAS).
#
# AUTHOR       : Javan Mukunzi
#
# ANALYSIS SET : Full Analysis Set (FAS)
#
# OUTPUT       : Publication-style demographic characteristics table



# =============================================================================
# 1. LOAD REQUIRED PACKAGES
# =============================================================================
#
# haven     : Read SAS datasets
# dplyr     : Data manipulation
# tidyr     : Data reshaping
# stringr   : String manipulation
# janitor   : Table/data cleaning utilities
# gt        : Publication-style table generation
# docorator : Document/PDF formatting
# tidyverse : Collection of tidy data tools
#
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(tidyverse)
library(gt)
library(docorator)


# =============================================================================
# 2. READ ADSL
# =============================================================================
#
# ADSL is the subject-level ADaM dataset.
#
# In a real clinical trial environment, the location would point to the
# validated project data directory.
# =============================================================================

adsl <- read_sas("data/adsl.sas7bdat")


# =============================================================================
# 3. SELECT THE FULL ANALYSIS SET
# =============================================================================
#
# FASFL = "Y" identifies subjects included in the Full Analysis Set.
#
# Filtering the analysis population before generating summaries ensures that
# all downstream demographic statistics are based on the correct population.
#
# =============================================================================

adsl_fas <- adsl %>%
  filter(FASFL == "Y")


# =============================================================================
# 4. SUMMARIZE AGE BY TREATMENT
# =============================================================================

# Statistics:
#   - N
#   - Mean
#   - Standard deviation
#   - Median
#   - Minimum
#   - Maximum

# =============================================================================

age_summary <- adsl_fas %>%
  
  group_by(TRT01P) %>%
  
  summarise(
    n      = n_distinct(USUBJID),
    Mean   = mean(AGE, na.rm = TRUE),
    SD     = sd(AGE, na.rm = TRUE),
    Median = median(AGE, na.rm = TRUE),
    Min    = min(AGE, na.rm = TRUE),
    Max    = max(AGE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Add an overall population row
  adorn_totals(
    where = "row",
    name = "Total"
  ) %>%
  
  # Apply reporting precision
  mutate(
    Mean   = round(Mean, 1),
    SD     = round(SD, 1),
    n      = round(n, 0),
    Min    = round(Min, 0),
    Max    = round(Max, 0)
  ) %>%
  
  # Convert statistics into rows
  pivot_longer(
    cols = -TRT01P
  ) %>%
  
  # Convert treatment groups into columns
  pivot_wider(
    id_cols = "name",
    names_from = "TRT01P",
    values_from = "value"
  ) %>%
  
  mutate(
    across(everything(), as.character),
    col2 = "Age (years)",
    ord = 3
  )


# =============================================================================
# 5. AGE GROUP SUMMARY
# =============================================================================
#
# Generate overall counts and percentages for each age category.
#
# Percentage:
#
#       Percentage = Category count / Overall N × 100
#
# =============================================================================

age_group_total <- adsl_fas %>%
  
  select(
    USUBJID,
    TRT01P,
    AGEGR1
  ) %>%
  
  group_by(AGEGR1) %>%
  
  summarise(
    N = n(),
    .groups = "drop"
  ) %>%
  
  mutate(
    NT = sum(N),
    p = round(N / NT * 100, 1),
    
    # Create display format: n (%)
    Total = sprintf("%3d (%5.1f)", N, p)
  ) %>%
  
  select(
    AGEGR1,
    Total
  )


# =============================================================================
# 6. AGE GROUP BY TREATMENT
# =============================================================================
#
# Calculate age-group counts and percentages separately for each treatment
# group.
#
# =============================================================================

age_group_by_trt <- adsl_fas %>%
  
  select(
    USUBJID,
    TRT01P,
    AGEGR1
  ) %>%
  
  group_by(
    AGEGR1,
    TRT01P
  ) %>%
  
  summarise(
    N = n(),
    .groups = "drop"
  ) %>%
  
  group_by(TRT01P) %>%
  
  mutate(
    NT = sum(N),
    p = round(N / NT * 100, 1),
    
    # Create n (%) display
    Total = sprintf("%3d (%5.1f)", N, p)
  ) %>%
  
  select(
    AGEGR1,
    TRT01P,
    Total
  ) %>%
  
  pivot_wider(
    id_cols = "AGEGR1",
    names_from = "TRT01P",
    values_from = "Total"
  )


# =============================================================================
# 7. COMBINE AGE-GROUP RESULTS
# =============================================================================

age_grp <- left_join(
  age_group_by_trt,
  age_group_total,
  by = "AGEGR1"
) %>%
  
  rename(
    name = AGEGR1
  ) %>%
  
  mutate(
    col2 = "Age group (years) n (%)",
    ord = 4
  )


# =============================================================================
# 8. SEX SUMMARY
# =============================================================================

# =============================================================================

adsl_fas_total <- adsl_fas %>%
  mutate(TRT01P = "Total")


adsl_sex <- bind_rows(
  adsl_fas,
  adsl_fas_total
)


# Add an overall category for the reporting structure

adsl_sex_total <- adsl_sex %>%
  mutate(SEX = "Total")


adsl_sex_reporting <- bind_rows(
  adsl_sex,
  adsl_sex_total
)


# Calculate denominators

trt_total <- adsl_sex_reporting %>%
  
  group_by(TRT01P) %>%
  
  summarise(
    N = n_distinct(USUBJID),
    .groups = "drop"
  )


# Calculate sex counts and percentages

sex_tab <- adsl_sex_reporting %>%
  
  group_by(
    TRT01P,
    SEX
  ) %>%
  
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  
  left_join(
    trt_total,
    by = "TRT01P"
  ) %>%
  
  mutate(
    pct = round((n / N) * 100, 1),
    
    # Display format: n (%)
    value = paste0(
      n,
      " (",
      sprintf("%4.1f", pct),
      "%)"
    )
  ) %>%
  
  select(
    TRT01P,
    SEX,
    value
  ) %>%
  
  pivot_wider(
    names_from = TRT01P,
    values_from = value,
    values_fill = "0"
  ) %>%
  
  mutate(
    across(
      everything(),
      ~ str_replace_all(
        .x,
        "\\(100\\.0\\%\\)",
        "(100%)"
      )
    )
  ) %>%
  
  rename(
    name = SEX
  ) %>%
  
  mutate(
    col2 = "Sex n (%)",
    ord = 5
  )


# =============================================================================
# 9. RACE SUMMARY
# =============================================================================
#
# Define the desired display order for race categories.
#
# =============================================================================

race_order <- c(
  "White",
  "Black or African American",
  "Asian"
)


# Add overall treatment category

adsl_race_total <- adsl_fas %>%
  mutate(TRT01P = "Total")


adsl_race <- bind_rows(
  adsl_fas,
  adsl_race_total
)


# Add overall race category

adsl_race_reporting <- bind_rows(
  adsl_race,
  adsl_race %>%
    mutate(RACE = "Total")
)


# Calculate treatment denominators

race_total <- adsl_race_reporting %>%
  
  group_by(TRT01P) %>%
  
  summarise(
    N = n_distinct(USUBJID),
    .groups = "drop"
  )


# Calculate race counts and percentages

race_table <- adsl_race_reporting %>%
  
  group_by(
    TRT01P,
    RACE
  ) %>%
  
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  
  left_join(
    race_total,
    by = "TRT01P"
  ) %>%
  
  mutate(
    pct = round((n / N) * 100, 1),
    
    percent = paste0(
      n,
      " (",
      sprintf("%4.1f", pct),
      "%)"
    )
  ) %>%
  
  select(
    TRT01P,
    RACE,
    percent
  ) %>%
  
  pivot_wider(
    names_from = TRT01P,
    values_from = percent,
    values_fill = "0"
  ) %>%
  
  # Standardize race display labels
  mutate(
    RACE = recode(
      RACE,
      "WHITE" = "White",
      "BLACK OR AFRICAN AMERICAN" = "Black or African American",
      "ASIAN" = "Asian"
    )
  ) %>%
  
  mutate(
    across(
      everything(),
      ~ str_replace_all(
        .x,
        "\\(100\\.0\\%\\)",
        "(100%)"
      )
    )
  ) %>%
  
  rename(
    name = RACE
  ) %>%
  
  # Apply predefined category ordering
  arrange(
    match(name, race_order)
  ) %>%
  
  mutate(
    col2 = "Race n (%)",
    ord = 6
  )


# =============================================================================
# 10. ETHNICITY SUMMARY
# =============================================================================
#
# Generate ethnicity counts and percentages by treatment group.
#
# =============================================================================

adsl_ethnic_total <- adsl_fas %>%
  mutate(TRT01P = "Total")


adsl_ethnic <- bind_rows(
  adsl_fas,
  adsl_ethnic_total
)


# Add overall ethnicity category

adsl_ethnic_reporting <- bind_rows(
  adsl_ethnic,
  adsl_ethnic %>%
    mutate(ETHNIC = "Total")
)


# Calculate denominators

ethnic_total <- adsl_ethnic_reporting %>%
  
  group_by(TRT01P) %>%
  
  summarise(
    N = n_distinct(USUBJID),
    .groups = "drop"
  )


# Calculate ethnicity counts and percentages

ethnic_table <- adsl_ethnic_reporting %>%
  
  group_by(
    TRT01P,
    ETHNIC
  ) %>%
  
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  
  left_join(
    ethnic_total,
    by = "TRT01P"
  ) %>%
  
  mutate(
    pct = round((n / N) * 100, 1),
    
    percent = paste0(
      n,
      " (",
      sprintf("%4.1f", pct),
      "%)"
    )
  ) %>%
  
  select(
    TRT01P,
    ETHNIC,
    percent
  ) %>%
  
  pivot_wider(
    names_from = TRT01P,
    values_from = percent,
    values_fill = "0"
  ) %>%
  
  mutate(
    ETHNIC = recode(
      ETHNIC,
      "HISPANIC OR LATINO" = "Hispanic or Latino",
      "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino"
    )
  ) %>%
  
  mutate(
    across(
      everything(),
      ~ str_replace_all(
        .x,
        "\\(100\\.0\\%\\)",
        "(100%)"
      )
    )
  ) %>%
  
  rename(
    name = ETHNIC
  ) %>%
  
  mutate(
    col2 = "Ethnic group n (%)",
    ord = 7
  )


# =============================================================================
# 11. COMBINE ALL DEMOGRAPHIC COMPONENTS
# =============================================================================
#
# Combine the individual reporting datasets into a single dataset.
#
# This creates the long-form reporting dataset that will subsequently be
# transformed into the final presentation structure.
#
# =============================================================================

demo_table <- bind_rows(
  age_summary,
  age_grp,
  sex_tab,
  race_table,
  ethnic_table
)


# =============================================================================
# 12. STANDARDIZE TREATMENT LABELS
# =============================================================================
#
# Replace project-specific treatment names with generic labels suitable for
# public portfolio demonstration.
#
# Treatment A = first treatment group
# Treatment B = second treatment group
# Total      = combined population
#
# =============================================================================

names(demo_table) <- str_replace(
  names(demo_table),
  "DRUG_A_Original",
  "DRUG A"
)

names(demo_table) <- str_replace(
  names(demo_table),
  "DRUG_B_Original",
  "DRUG B"
)


# =============================================================================
# 13. DEFINE EXPECTED TREATMENT ORDER
# =============================================================================
#
# Explicit ordering prevents treatment columns from appearing in an
# unintended order in the final report.
#
# =============================================================================

expected_trt <- c(
  "DRUG A",
  "DRUG B",
  "Total"
)


# =============================================================================
# 14. CREATE REPORTING DENOMINATORS
# =============================================================================
#
# Derive the number of unique subjects in each treatment group.
#
# These values will be displayed in the final table header as:
#
# DRUG A (N=XX)
# DRUG B (N=XX)
# Total (N=XX)
#
# =============================================================================

adsl_reporting <- adsl_fas %>%
  
  mutate(
    TRT01P = "Total"
  )


report_population <- bind_rows(
  adsl_fas,
  adsl_reporting
)


bigN_hdr <- report_population %>%
  
  distinct(
    USUBJID,
    TRT01P
  ) %>%
  
  count(
    TRT01P,
    name = "BIG_N"
  ) %>%
  
  mutate(
    TRT01P = as.character(TRT01P)
  )


# Create named denominator vector

denom_vec <- bigN_hdr$BIG_N

names(denom_vec) <- bigN_hdr$TRT01P


# =============================================================================
# 15. IDENTIFY TREATMENT COLUMNS
# =============================================================================
#
# Identify treatment columns programmatically rather than hard-coding every
# column name.
#
# This approach is useful when the number of treatment groups can change.
#
# =============================================================================

qc_trt_cols <- names(demo_table)[
  grepl(
    "^Treatment",
    names(demo_table)
  )
]


# Stop execution if treatment columns cannot be identified.

if (length(qc_trt_cols) == 0) {
  
  stop(
    "No treatment columns were identified in the reporting dataset."
  )
}


# =============================================================================
# 16. MAP TREATMENT COLUMNS
# =============================================================================
#
# Create a mapping between treatment columns and their display labels.
#
# =============================================================================

trt_map <- tibble(
  
  qc_col = qc_trt_cols,
  
  TRT01P = c(
    "DRUG A",
    "DRUG B"
  )
)


# =============================================================================
# 17. CREATE REPORTING DATASET
# =============================================================================
#
# The final table requires:
#
#   Section
#      Category
#      Category
#
# The code below creates section header rows and demographic body rows.
#
# =============================================================================

indent <- "  "


# -----------------------------------------------------------------------------
# Body rows
# -----------------------------------------------------------------------------

body_rows <- demo_table %>%
  
  mutate(
    
    # Indent category-level rows
    STUB = paste0(
      indent,
      name
    ),
    
    # Preserve section ordering
    seg = ord
    
  ) %>%
  
  select(
    seg,
    col2,
    STUB,
    all_of(qc_trt_cols)
  )


# -----------------------------------------------------------------------------
# Section header rows
# -----------------------------------------------------------------------------

header_rows <- demo_table %>%
  
  distinct(
    ord,
    col2
  ) %>%
  
  rename(
    seg = ord
  ) %>%
  
  arrange(seg) %>%
  
  transmute(
    seg,
    col2,
    STUB = col2
  )


# Populate treatment columns with blank values for section headers

for (cc in qc_trt_cols) {
  
  header_rows[[cc]] <- ""
}


# =============================================================================
# 18. COMBINE HEADER AND BODY ROWS
# =============================================================================

report_df <- bind_rows(
  
  header_rows %>%
    mutate(row_id = 0L),
  
  body_rows %>%
    group_by(seg) %>%
    mutate(
      row_id = row_number()
    ) %>%
    ungroup()
  
) %>%
  
  arrange(
    seg,
    row_id
  ) %>%
  
  select(
    STUB,
    all_of(qc_trt_cols)
  )


# =============================================================================
# 19. PRESERVE TABLE INDENTATION
# =============================================================================
#
# Some PDF rendering engines can remove leading spaces.
#
# This helper function converts leading spaces into non-breaking spaces so
# that indentation is preserved in the final PDF.
#
# =============================================================================

make_leading_spaces_visible <- function(x) {
  
  x <- ifelse(
    is.na(x) | x == "",
    "\u00A0",
    x
  )
  
  
  m <- regexpr(
    "^\\s+",
    x
  )
  
  
  has <- m > 0
  
  
  lead <- ifelse(
    has,
    regmatches(x, m),
    ""
  )
  
  
  rest <- ifelse(
    has,
    substring(
      x,
      attr(m, "match.length") + 1
    ),
    x
  )
  
  
  lead_nbsp <- vapply(
    
    lead,
    
    function(s) {
      
      paste(
        rep(
          "\u00A0",
          nchar(s)
        ),
        collapse = ""
      )
      
    },
    
    character(1)
  )
  
  
  paste0(
    lead_nbsp,
    rest
  )
}


# Apply display-safe formatting

report_df <- report_df %>%
  
  mutate(
    
    STUB = make_leading_spaces_visible(STUB),
    
    across(
      all_of(qc_trt_cols),
      ~ ifelse(
        is.na(.x) | .x == "",
        "\u00A0",
        as.character(.x)
      )
    )
    
  )


# =============================================================================
# 20. CREATE TABLE HEADER LABELS
# =============================================================================
#
# Treatment names are displayed generically for the public portfolio.
#
# Example:
#
# DRUG A (N=XX)
# DRUG B (N=XX)
# Total (N=XXX)
#
# =============================================================================

hdr_labels <- setNames(
  
  paste0(
    
    c(
      "DRUG A",
      "DRUG B",
      "Total"
    ),
    
    " (N=",
    
    as.integer(
      denom_vec[
        c(
          "DRUG A",
          "DRUG B",
          "Total"
        )
      ]
    ),
    
    ")"
    
  ),
  
  qc_trt_cols
)


# =============================================================================
# 21. CREATE PUBLICATION-STYLE TABLE USING gt
# =============================================================================
#
# gt provides flexible formatting for publication-style clinical tables.
#
# =============================================================================

demographic_table <- gt(report_df) %>%
  
  cols_label(
    
    .list = c(
      
      list(
        STUB = "\u00A0"
      ),
      
      as.list(hdr_labels)
    )
    
  ) %>%
  
  # Left-align demographic descriptions
  cols_align(
    align = "left",
    columns = "STUB"
  ) %>%
  
  # Center treatment columns
  cols_align(
    align = "center",
    columns = all_of(qc_trt_cols)
  ) %>%
  
  # Center treatment headers
  tab_style(
    
    style = cell_text(
      align = "center"
    ),
    
    locations = cells_column_labels(
      columns = all_of(qc_trt_cols)
    )
    
  ) %>%
  
  # Set header font size
  tab_style(
    
    style = cell_text(
      size = gt::px(14)
    ),
    
    locations = cells_column_labels(
      columns = all_of(qc_trt_cols)
    )
    
  ) %>%
  
  # Set body font size
  tab_options(
    table.font.size = gt::px(9)
  ) %>%
  
  # Define column widths
  cols_width(
    
    STUB ~ gt::px(240),
    
    all_of(qc_trt_cols) ~ gt::px(190)
    
  )


# =============================================================================
# 22. RENDER FINAL PDF
# =============================================================================
#
# The final output is written to the local "output" directory.
#
# No protocol number, sponsor name, compound name, patient information,
# internal directory, or confidential project metadata is included.
#
# =============================================================================

demographic_table %>%
  
  as_docorator(
    
    display_name = "demographic_characteristics",
    
    display_loc = "output",
    
    header = fancyhead(
      
      fancyrow(
        left = "Clinical Programming Portfolio",
        center = NA,
        right = doc_pagenum()
      ),
      
      fancyrow(
        left = "Population: Full Analysis Set",
        center = NA,
        right = "Public Portfolio Version"
      ),
      
      fancyrow(
        left = NA,
        center = "Demographic and Baseline Characteristics",
        right = NA
      )
      
    ),
    
    footer = fancyfoot(
      
      fancyrow(
        left = "R Clinical Programming",
        center = NA,
        right = "Portfolio Demonstration"
      )
      
    )
    
  ) %>%
  
  render_pdf()


# =============================================================================
# 23. PROGRAM COMPLETION MESSAGE
# =============================================================================

message(
  "Demographic table generation completed successfully."
)

message(
  "Output: output/demographic_characteristics.pdf"
)


# =============================================================================
# END OF PROGRAM
# =============================================================================
