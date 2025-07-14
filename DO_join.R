library(readr)
library(dplyr)
library(stringr)

# 1. Read raw CSVs
sample_results <- read.csv(
  "./data/raw/2016_2025_DO/resultphyschem_2.csv",
  stringsAsFactors = FALSE
)
site_data <- read.csv(
  "./data/raw/2016_2025_DO/station_3.csv",
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
  "./data/cleaned/DO/merged_full_DO.csv",
  row.names = FALSE
)

# 4. Read it back in, forcing the value column to character
df <- read_csv(
  "./data/cleaned/DO/merged_full_DO.csv",
  col_types = cols(
    ResultMeasureValue = col_character(),
    .default           = col_guess()
  )
)

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
    LongitudeMeasure
  )

# 7. Drop rows missing lat/lon or the result value
df_clean <- df_clean %>%
  filter(
    !is.na(LatitudeMeasure),
    !is.na(LongitudeMeasure),
    !is.na(ResultMeasureValue)
  )

# 8. Remove exact duplicates
df_clean <- distinct(df_clean)

# 9. Clean up the result values and coerce types
df_clean <- df_clean %>%
  mutate(
    # map VOID & Not Reported to NA
    ResultMeasureValue = na_if(ResultMeasureValue, "VOID"),
    ResultMeasureValue = na_if(ResultMeasureValue, "Not Reported"),
    # strip any "< " prefix from non-detects
    ResultMeasureValue = str_remove(ResultMeasureValue, "^<\\s*"),
    # convert to numeric
    ResultMeasureValue = as.numeric(ResultMeasureValue),
    # ensure lat/long are numeric
    LatitudeMeasure  = as.numeric(LatitudeMeasure),
    LongitudeMeasure = as.numeric(LongitudeMeasure)
  )

# 10. Write the ESRI-ready CSV
write_csv(
  df_clean,
  "./data/cleaned/DO/merged_2016_2025_DO_esri.csv"
)

# 11. Quick sanity-check
merged_2016_2025_DO_esri <- read.csv(
  "./data/cleaned/merged_2016_2025_DO_esri.csv",
  stringsAsFactors = FALSE
)
View(merged_2016_2025_DO_esri)
range(merged_2016_2025_DO$ResultMeasureValue, na.rm = TRUE)
str(merged_2016_2025_DO$ResultMeasureValue)
