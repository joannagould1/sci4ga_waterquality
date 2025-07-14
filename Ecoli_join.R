
library(readr)
library(dplyr)
library(stringr)

sample_results <- read.csv("./data/raw/1980_2025_ecoli/resultphyschem.csv")
site_data      <- read.csv("./data/raw/1980_2025_ecoli/station.csv")

merged_data <- merge(
  sample_results,
  site_data[, c("MonitoringLocationIdentifier", "LatitudeMeasure", "LongitudeMeasure")],
  by = "MonitoringLocationIdentifier",
  all.x = TRUE
)

write.csv(merged_data, "./data/cleaned/merged_full_ecoli.csv", row.names = FALSE)

df <- read_csv("./data/cleaned/merged_full_ecoli.csv")

df <- df %>%
  filter(ActivityStartDate >= "2016-01-01")

# 3. Select only the essential columns
df_clean <- df %>%
  select(
    MonitoringLocationIdentifier,
    ActivityStartDate,
    ResultMeasureValue,
    CharacteristicName,
    ResultMeasure.MeasureUnitCode,
    LatitudeMeasure,
    LongitudeMeasure,
  )

# 4. Drop any rows missing lat/lon or result value
df_clean <- df_clean %>%
  filter(
    !is.na(LatitudeMeasure),
    !is.na(LongitudeMeasure),
    !is.na(ResultMeasureValue)
  )

# 5. Remove exact duplicates
df_clean <- distinct(df_clean)

# 6. Clean Data
df_clean <- df_clean %>%
  mutate(
    
    # map VOID & Not Reported to NA
    ResultMeasureValue = na_if(ResultMeasureValue, "VOID"),
    ResultMeasureValue = na_if(ResultMeasureValue, "Not Reported"),
    
    # remove any '< ' prefix (non-detects) and convert to numeric
    ResultMeasureValue = str_remove(ResultMeasureValue, "^<\\s*"),
    ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
  
  # Ensure lat/long are numeric
  mutate(
    LatitudeMeasure  = as.numeric(LatitudeMeasure),
    LongitudeMeasure = as.numeric(LongitudeMeasure))

# 6. Write out the cleaned CSV
write_csv(df_clean, "./data/cleaned/merged_2015_2025_ecoli_esri.csv")

merged_2015_2025_ecoli_esri <- read.csv("./data/cleaned/merged_2015_2025_ecoli_esri.csv")

View(merged_2015_2025_ecoli_esri)


range(merged_2015_2025_ecoli_esri$ResultMeasureValue, na.rm = TRUE)
str(merged_2015_2025_ecoli_esri$ResultMeasureValue)

df_clean %>%
  filter(ResultMeasureValue > 90000) %>%
  select(MonitoringLocationIdentifier,
         ActivityStartDate,
         ResultMeasureValue,
         ResultMeasure.MeasureUnitCode) %>%
  print(n = Inf)
