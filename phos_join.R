library(readr)
library(dplyr)
library(stringr)

# 1. Read raw CSVs
sample_results <- read.csv(
  "./data/raw/2016_2025_phos/resultphyschem.csv",
  stringsAsFactors = FALSE
)
site_data <- read.csv(
  "./data/raw/2016_2025_phos/station.csv",
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
  "./data/cleaned/phos/merged_full_phos.csv",
  row.names = FALSE)

# 4. Read it back in, forcing the value column to character
df <- read_csv(
  "./data/cleaned/phos/merged_full_phos.csv",
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

# 9. Clean up the result values, coerce types, then normalize everything to mg/L
df_clean <- df_clean %>%
  mutate(
    # map VOID & Not Reported to NA
    ResultMeasureValue = na_if(ResultMeasureValue, "VOID"),
    ResultMeasureValue = na_if(ResultMeasureValue, "Not Reported"),
    # strip any "< " prefix from non-detects
    ResultMeasureValue = str_remove(ResultMeasureValue, "^<\\s*"),
    # convert to numeric
    ResultMeasureValue = as.numeric(ResultMeasureValue),
    LatitudeMeasure  = as.numeric(LatitudeMeasure),
    LongitudeMeasure = as.numeric(LongitudeMeasure)
  ) %>%
  # ---- mutate to normalize units ----
mutate(
  # if the original unit was in ug/L, divide the number by 1000
  ResultMeasureValue = case_when(
    str_detect(ResultMeasure.MeasureUnitCode, regex("^\\s*ug", ignore_case = TRUE)) ~ ResultMeasureValue / 1000,
    TRUE ~ ResultMeasureValue
  ),
  # and change those "ug/L" codes into plain "mg/L"
  ResultMeasure.MeasureUnitCode = case_when(
    str_detect(ResultMeasure.MeasureUnitCode, regex("^\\s*ug", ignore_case = TRUE)) ~ "mg/L",
    TRUE ~ ResultMeasure.MeasureUnitCode
  )
)

df_clean <- df_clean %>%
  # your existing conversions…
  mutate(
    ResultMeasureValue = case_when(
      str_detect(ResultMeasure.MeasureUnitCode, regex("^\\s*ug", ignore_case=TRUE)) ~ ResultMeasureValue/1000,
      TRUE ~ ResultMeasureValue
    ),
    ResultMeasure.MeasureUnitCode = case_when(
      str_detect(ResultMeasure.MeasureUnitCode, regex("^\\s*ug", ignore_case=TRUE)) ~ "mg/L",
      TRUE ~ ResultMeasure.MeasureUnitCode
    )
  ) %>%
  # now filter to only true‐per‐liter mg/L codes, non‐negative, and plausible values
  filter(
    str_detect(ResultMeasure.MeasureUnitCode, "/L$"),     # e.g. “mg/L”, “mg/l as N”, “mg/l asNO3”
    ResultMeasureValue >= 0,
  )

# filter out extremely high values that make no sense (outliers)
df_clean <- df_clean %>%
  filter(
    ResultMeasureValue >= 0,
    ResultMeasureValue < 20
  )


# 10. Write the ESRI-ready CSV
write_csv(
  df_clean,
  "./data/cleaned/phos/merged_2016_2025_phos_esri.csv")

# 11. Quick sanity-check
merged_2016_2025_phos_esri <- read.csv(
  "./data/cleaned/phos/merged_2016_2025_phos_esri.csv",
  stringsAsFactors = FALSE
)

View(merged_2016_2025_phos_esri)
range(merged_2016_2025_phos_esri$ResultMeasureValue, na.rm = TRUE) # make sure range makes sense for variable
str(merged_2016_2025_phos_esri$ResultMeasureValue) # should be numeric

merged_2016_2025_phos_esri %>%
  arrange(desc(ResultMeasureValue)) %>%
  slice(1:10)
