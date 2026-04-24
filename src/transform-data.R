library(tidyverse)
library(janitor)

tango1evt <- read_tsv("https://github.com/biodiversity-aq/TANGO_1/raw/refs/heads/main/data/03_output/tango_1_events.tsv")
tango2evt <- read_tsv("https://github.com/biodiversity-aq/TANGO_2/raw/refs/heads/main/data/03_output/tango_2_events.tsv")

rov1evt <- tango1evt %>% filter(gearType == "Remotely Operated Vehicle") 
rov2evt <- tango2evt %>% filter(gearType == "ROV")

# tango1
rov1 <- rov1evt %>% 
  mutate(
    # make eventID unique across datasets
    eventID = paste0("TANGO1_", eventID)
  )

# rov2 has no decimalLatitude and decimalLongitude
rov2 <- rov2evt %>% rename(
  higherGeographyID = higherGeographyId
) %>%
  mutate(
    # make eventID unique across datasets
    eventID = paste0("TANGO2_", eventId)
  ) %>%
  select(-eventId)


rov <- bind_rows(rov1, rov2)
