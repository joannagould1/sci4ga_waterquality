library(readr)
library(dplyr)
library(stringr)

# 1. Read raw CSVs
sample_results <- read.csv(
  "./data/raw/1980_2025_turbidity/resultphyschem.csv",
  stringsAsFactors = FALSE
)
site_data <- read.csv(
  "./data/raw/1980_2025_turbidity/station.csv",
  stringsAsFactors = FALSE
)

# 2. Merge lat/long into the sample results
merged_data <- merge(
  sample_results,
  site_data[, c("MonitoringLocationIdentifier", "LatitudeMeasure", "LongitudeMeasure")],
  by = "MonitoringLocationIdentifier",
  all.x = TRUE
)

# 3. Write out the full merged table
write.csv(
  merged_data,
  "./data/cleaned/turbidity/merged_full_turbidity.csv",
  row.names = FALSE)

# 4. Read it back in, forcing the value column to character
df <- read_csv(
  "./data/cleaned/turbidity/merged_full_turbidity.csv",
  col_types = cols(
    ResultMeasureValue = col_character(),
    .default           = col_guess()))

# 5. (Optional) keep only 2016+  
# df <- df %>% filter(ActivityStartDate >= "2016-01-01")

# 6. Select only the columns you need
df_clean <- df %>%
  select(
    MonitoringLocationIdentifier,
    ActivityStartDate,
    ResultMeasureValue,
    CharacteristicName,
    ResultMeasure.MeasureUnitCode,
    LatitudeMeasure,
    LongitudeMeasure)

# 7. Drop rows missing lat/lon or the result value
df_clean <- df_clean %>%
  filter(
    !is.na(LatitudeMeasure),
    !is.na(LongitudeMeasure),
    !is.na(ResultMeasureValue))

# 8. Remove exact duplicates
df_clean <- distinct(df_clean)
# 9. Clean up result values, coerce types, then normalize everything to NTU
df_clean <- df_clean %>%
  mutate(
    # map VOID & Not Reported to NA
    ResultMeasureValue    = na_if(ResultMeasureValue, "VOID"),
    ResultMeasureValue    = na_if(ResultMeasureValue, "Not Reported"),
    # strip any "< " prefix from non-detects
    ResultMeasureValue    = str_remove(ResultMeasureValue, "^<\\s*"),
    # convert to numeric
    ResultMeasureValue    = as.numeric(ResultMeasureValue),
    LatitudeMeasure       = as.numeric(LatitudeMeasure),
    LongitudeMeasure      = as.numeric(LongitudeMeasure)
  ) %>%
  # keep only turbidity samples
  filter(str_detect(CharacteristicName, regex("turbidity", ignore_case = TRUE))) %>%
  # unify any NTU/FNU/FTU code into plain "NTU"
  mutate(
    ResultMeasure.MeasureUnitCode = case_when(
      str_detect(ResultMeasure.MeasureUnitCode, regex("ntu|fnu|ftu", ignore_case = TRUE)) ~ "NTU",
      TRUE ~ ResultMeasure.MeasureUnitCode
    )
  ) %>%
  # drop anything that's not NTU, and any negative or missing values
  filter(
    ResultMeasure.MeasureUnitCode == "NTU",
    !is.na(ResultMeasureValue),
    ResultMeasureValue >= 0
  )

df_clean <- df_clean %>%
  filter(ResultMeasureValue <= 30000)

# 10. Write the ESRI-ready CSV
write_csv(
  df_clean,
  "./data/cleaned/turbidity/merged_1980_2025_turbidity_esri.csv")

# 11. Quick sanity-check
merged_1980_2025_turbidity_esri <- read.csv(
  "./data/cleaned/turbidity/merged_1980_2025_turbidity_esri.csv",
  stringsAsFactors = FALSE
)

View(merged_1980_2025_turbidity_esri)
range(merged_1980_2025_turbidity_esri$ResultMeasureValue, na.rm = TRUE) # make sure range makes sense for variable
str(merged_1980_2025_turbidity_esri$ResultMeasureValue) # should be numeric
