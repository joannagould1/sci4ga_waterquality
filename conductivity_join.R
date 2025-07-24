library(readr)
library(dplyr)
library(stringr)

# 1. Read raw CSVs
sample_results <- read.csv(
  "./data/raw/conductivity/resultphyschem.csv",
  stringsAsFactors = FALSE
)
site_data <- read.csv(
  "./data/raw/conductivity/station.csv",
  stringsAsFactors = FALSE
)

# 2. Merge lat/long into the sample results
merged_data <- merge(
  sample_results,
  site_data[, c("MonitoringLocationIdentifier", "LatitudeMeasure", "LongitudeMeasure")],
  by = "MonitoringLocationIdentifier",
  all.x = TRUE
)

# 3. (Optionally) write out the full merged table
write.csv(
  merged_data,
  "./data/cleaned/conductivity/merged_full_conductivity.csv",
  row.names = FALSE
)

# 4. Read it back in, forcing the value columns to character
df <- read_csv(
  "./data/cleaned/conductivity/merged_full_conductivity.csv",
  col_types = cols(
    ResultMeasureValue              = col_character(),
    ResultMeasure.MeasureUnitCode = col_character(),
    .default                        = col_guess()
  )
)

# 5. (keep all years — no date filter)

# 6. Select only the columns you need
df_clean <- df %>%
  select(
    MonitoringLocationIdentifier,
    ActivityStartDate,
    CharacteristicName,
    ResultMeasureValue,
    ResultMeasure.MeasureUnitCode,
    LatitudeMeasure,
    LongitudeMeasure
  )

# 7. Drop rows missing lat/lon or the raw result value
df_clean <- df_clean %>%
  filter(
    !is.na(LatitudeMeasure),
    !is.na(LongitudeMeasure),
    !is.na(ResultMeasureValue)
  )

# 8. Remove exact duplicates
df_clean <- distinct(df_clean)

# 9. Clean up values, coerce types, then normalize to µS/cm
df_clean <- df_clean %>%
  mutate(
    # map VOID & Not Reported → NA
    ResultMeasureValue = na_if(ResultMeasureValue, "VOID"),
    ResultMeasureValue = na_if(ResultMeasureValue, "Not Reported"),
    # strip any “< ” non-detect prefix
    ResultMeasureValue = str_remove(ResultMeasureValue, "^<\\s*"),
    # convert to numeric
    ResultMeasureValue = as.numeric(ResultMeasureValue),
    LatitudeMeasure    = as.numeric(LatitudeMeasure),
    LongitudeMeasure   = as.numeric(LongitudeMeasure),
    # unify unit codes
    ResultMeasure.MeasureUnitCode = case_when(
      str_detect(ResultMeasure.MeasureUnitCode, regex("mS/cm",  ignore_case = TRUE)) ~ "mS/cm",
      str_detect(ResultMeasure.MeasureUnitCode, regex("µS/cm|us/cm|µmho/cm|umho/cm", ignore_case = TRUE)) ~ "uS/cm",
      TRUE ~ ResultMeasure.MeasureUnitCode
    )
  ) %>%
  # keep only conductivity measurements
  filter(
    str_detect(CharacteristicName, regex("conduct", ignore_case = TRUE)),
    ResultMeasureValue >= 0,
    ResultMeasure.MeasureUnitCode %in% c("mS/cm", "uS/cm")
  ) %>%
  # convert all to µS/cm
  mutate(
    ResultMeasureValue = case_when(
      ResultMeasure.MeasureUnitCode == "mS/cm" ~ ResultMeasureValue * 1000,
      TRUE                                        ~ ResultMeasureValue
    ),
    ResultMeasure.MeasureUnitCode = "uS/cm"
  )

df_clean <- df_clean %>%
  filter(ResultMeasureValue <= 449000)

# 10. Write the ESRI–ready CSV (all years, no cap)
write_csv(
  df_clean,
  "./data/cleaned/conductivity/merged_conductivity_esri.csv"
)

# 11. Quick sanity-check
merged_cond <- read.csv(
  "./data/cleaned/conductivity/merged_conductivity_esri.csv",
  stringsAsFactors = FALSE
)

# eyeball it in RStudio’s viewer:
View(merged_cond)

# check that it’s numeric and in the expected range:
range(merged_cond$ResultMeasureValue, na.rm = TRUE)
str(merged_cond$ResultMeasureValue)  # should be numeric

library(dplyr)
library(readr)

library(dplyr)
library(readr)

# (re-load your cleaned df, or just pipe from the end of your previous script)
df_fixed <- df_clean %>%
  rename(
    StationID  = MonitoringLocationIdentifier,
    Date       = ActivityStartDate,
    CharName   = CharacteristicName,
    Value_uS   = ResultMeasureValue,
    Unit       = ResultMeasure.MeasureUnitCode,
    Latitude   = LatitudeMeasure,
    Longitude  = LongitudeMeasure
  ) %>%
  select(StationID, Date, CharName, Value_uS, Unit, Latitude, Longitude)

# write the portal-ready CSV
write_csv(
  df_fixed,
  "./data/cleaned/conductivity/merged_conductivity_for_portal.csv"
)

