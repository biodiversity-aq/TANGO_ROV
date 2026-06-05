library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(readr)
library(stringr)
library(geosphere)
library(jsonlite)
library(glue)
library(worrms)
library(xml2)


## Load files
evt <- read_excel(here("data", "01_raw", "TANGO_metadata_EVENTS.xlsx"))
tango1_abd <- read_tsv(here("data", "02_interim", "tango1_abundance_remove-cr.tsv"), na = c("", "NA"))
tango2_abd <- read_tsv(here("data", "02_interim", "tango2_abundance_remove-cr.tsv"), na = c("", "NA"))


# EVENT CORE

# Read Darwin Core Event XML
dwc_event <- read_xml("https://rs.gbif.org/core/dwc_event_2025-07-10.xml")

# Extract field names
# will be used to select ONLY the columns that are in the DwC Event core when 
# writing the event table (non Event core fields are also in dynamicProperties 
# so that the information is preserved)
ns <- xml_ns(dwc_event)
dwc_event_fields <- dwc_event %>%
  xml_find_all(".//d1:property", ns = ns) %>%
  xml_attr("name")


# Decimal degrees cleaner
clean_dd <- function(x) {
  x %>%
    na_if("NA") %>%
    parse_number()
}

# Degree decimal minutes cleaner, e.g. "65° 59.805 S"
dmd_to_dd <- function(x) {
  x <- na_if(x, "NA")
  deg <- str_extract(x, "^\\d+") %>% as.numeric()
  min <- str_extract(x, "\\d+\\.\\d+") %>% as.numeric()
  hemi <- str_extract(x, "[NSEW]$")
  dd <- deg + min / 60
  case_when(
    hemi %in% c("S", "W") ~ -dd,
    hemi %in% c("N", "E") ~ dd,
    TRUE ~ NA_real_
  )
}

# buffer for coordinate uncertainty in meters to account for GPS error
gps_buffer_m <- 10


event <- evt %>%
  rowwise() %>%
  rename(
    rovID = event_ID,
    # trailing "_" was manually removed from source file in transect_name to create cleaner eventIDs
    eventID = transect_name, 
    verbatimEventDate = date,
    verbatimLocality = station_full_name) %>%
  mutate(
    parentEventID = case_when(
      campaign == "TANGO1" ~ "https://www.wikidata.org/entity/Q119843670",
      campaign == "TANGO2" ~ "https://www.wikidata.org/entity/Q137398578",
      TRUE ~ NA_character_
    ),
    eventType = case_when(
      campaign == "TANGO1" & str_starts(transect, "q") ~ "quadrat",
      campaign == "TANGO1" & str_starts(transect, "t") ~ "transect",
      campaign == "TANGO2" ~ "transect",
      TRUE ~ NA_character_
      ),
    # single digit day is not zero-padded, so we need to pad it
    eventDate = dmy(str_pad(as.character(verbatimEventDate), 8, pad = "0")),
    higherGeographyID = case_when(
      str_detect(verbatimLocality, "Omega") ~ "https://www.wikidata.org/entity/Q925409",
      verbatimLocality == "Hovgaard Islands" ~ "https://www.wikidata.org/entity/Q260210",
      verbatimLocality == "Dodman Island" ~ "https://www.wikidata.org/entity/Q5287955",
      verbatimLocality == "Foyn Harbor" ~ "https://www.wikidata.org/entity/Q60678",
      verbatimLocality == "Entreprise Island" ~ "https://www.wikidata.org/entity/Q3593069",
      verbatimLocality == "Blaiklock Island" ~ "https://www.wikidata.org/entity/Q4923907",
      TRUE ~ NA_character_
    ),
    locality = case_when(
      str_detect(verbatimLocality, "Omega") ~ "Omega Island",
      verbatimLocality == "Hovgaard Islands" ~ "Hovgaard Island",
      TRUE ~ verbatimLocality
    ),
    deploymentRemarks = case_when(
      deployed_from == "Australis" ~ "deployed from Australis (https://ocean-expeditions.com/the-vessel-australis/)",
      deployed_from == "Zodiac" ~ "deployed from Zodiac",
    ),
    # coordinates
    # Clean DD columns
    lon_start_dd = clean_dd(site_lon_DD),
    lat_start_dd = clean_dd(site_lat_DD),
    lon_stop_dd  = clean_dd(site_lon_stop_DD),
    lat_stop_dd  = clean_dd(site_lat_stop_DD),
    
    # DMD columns appear swapped:
    lat_start_dmd = dmd_to_dd(site_lon_DMD),
    lon_start_dmd = dmd_to_dd(site_lat_DMD),
    
    # Prefer DD if available, otherwise DMD
    decimalLatitude_start  = round(coalesce(lat_start_dd, lat_start_dmd), 4),
    decimalLongitude_start = round(coalesce(lon_start_dd, lon_start_dmd), 4),
    
    decimalLatitude_stop   = round(lat_stop_dd, 4),
    decimalLongitude_stop  = round(lon_stop_dd, 4),
    
    # Midpoint for Darwin Core decimalLatitude / decimalLongitude
    decimalLatitude = case_when(
      !is.na(decimalLatitude_start) & !is.na(decimalLatitude_stop) ~ 
        round((decimalLatitude_start + decimalLatitude_stop) / 2, 4),
      TRUE ~ round(decimalLatitude_start, 4)
    ),
    
    decimalLongitude = case_when(
      !is.na(decimalLongitude_start) & !is.na(decimalLongitude_stop) ~ 
        round((decimalLongitude_start + decimalLongitude_stop) / 2, 4),
      TRUE ~ round(decimalLongitude_start, 4)
    ),
    footprintWKT = case_when(
      !is.na(decimalLongitude_start) & !is.na(decimalLatitude_start) &
        !is.na(decimalLongitude_stop) & !is.na(decimalLatitude_stop) ~
        # use sprintf to ensure that they are 4 decimal places
        sprintf(
          "LINESTRING (%.4f %.4f, %.4f %.4f)",
          decimalLongitude_start, decimalLatitude_start,
          decimalLongitude_stop, decimalLatitude_stop
        ) %>% as.character(),
      TRUE ~ NA_character_
    ),
    coordinatesRemarks = case_when(
      is.na(decimalLatitude_start) & is.na(decimalLongitude_start) &
        is.na(decimalLatitude_stop) & is.na(decimalLongitude_stop) ~ "start and end coordinates unknown",
      is.na(decimalLatitude_start) | is.na(decimalLongitude_start) ~ "start coordinates unknown",
      is.na(decimalLatitude_stop) | is.na(decimalLongitude_stop) ~ "end coordinates unknown",
      TRUE ~ NA_character_
    ),
    # cuim
    # coordinate-derived start–stop distance when both start and stop coordinates exist; calculated_dist as a fallback
    calculated_dist_m = parse_number(calculated_dist),
    start_stop_dist_m = case_when(
      !is.na(decimalLongitude_start) & !is.na(decimalLatitude_start) &
        !is.na(decimalLongitude_stop) & !is.na(decimalLatitude_stop) ~
        geosphere::distHaversine(
          cbind(decimalLongitude_start, decimalLatitude_start),
          cbind(decimalLongitude_stop, decimalLatitude_stop)
        ),
      TRUE ~ NA_real_
    ),
    uncertainty_base_m = coalesce(
      start_stop_dist_m / 2,
      calculated_dist_m / 2
    ),
    coordinateUncertaintyInMeters = case_when(
      !is.na(uncertainty_base_m) ~ round(uncertainty_base_m + gps_buffer_m),
      !is.na(decimalLatitude) & !is.na(decimalLongitude) ~ gps_buffer_m,
      TRUE ~ NA_real_
    ),
    georeferenceRemarks = case_when(
      !is.na(start_stop_dist_m) ~ "Coordinate uncertainty calculated as half the start-stop transect distance plus a 10 m GPS accuracy buffer.",
      is.na(start_stop_dist_m) & !is.na(calculated_dist_m) ~ "Coordinate uncertainty calculated as half the recorded ROV transect distance plus a 10 m GPS accuracy buffer.",
      !is.na(decimalLatitude) & !is.na(decimalLongitude) ~ "Coordinate uncertainty based on a 10 m GPS accuracy buffer.",
      TRUE ~ NA_character_
    ),
    # time
    # replace ? with NA
    start_time_clean = na_if(start_transect_carnet, "?"),
    stop_time_clean  = na_if(stop_transect_carnet, "?"),
    # zero-pad hour
    start_time_clean = if_else(
      !is.na(start_time_clean),
      str_replace(start_time_clean,"^(\\d):", "0\\1:"),
      NA_character_
    ),
    stop_time_clean = if_else(
      !is.na(stop_time_clean),
      str_replace(stop_time_clean, "^(\\d):", "0\\1:"),
      NA_character_
    ),
    # ISO strings
    start_time_iso = if_else(
      !is.na(start_time_clean),
      str_c(start_time_clean, "-03:00"),
      NA_character_ 
    ),
    stop_time_iso = if_else(
      !is.na(stop_time_clean),
      str_c(stop_time_clean, "-03:00"),
      NA_character_
    ),
    eventTime = case_when(
      !is.na(start_time_iso) & !is.na(stop_time_iso) ~ str_c(start_time_iso, "/", stop_time_iso),
      !is.na(start_time_iso) &  is.na(stop_time_iso) ~ start_time_iso,
      is.na(start_time_iso)  & !is.na(stop_time_iso) ~ stop_time_iso,
      TRUE ~ NA_character_
    ),
    eventTimeRemarks = case_when(
      is.na(start_time_iso) & is.na(stop_time_iso) ~ "start and end time unknown",
      is.na(start_time_iso) ~ "start time unknown",
      is.na(stop_time_iso) ~ "end time unknown",
      TRUE ~ NA_character_
    ),
    # concatenate all remarks for events
    eventRemarks = str_c(
      na_if(coordinatesRemarks, ""),
      na_if(eventTimeRemarks, ""),
      na_if(deploymentRemarks, ""),
      sep = " | "
    ) %>%
      str_replace("^ \\| ", "") %>%
      str_replace(" \\| $", "") %>%
      na_if(""),
    samplingProtocol = case_when(
      eventType == "quadrat" ~ paste(
        "ROV quadrat survey using lawn-mower type trajectories, followed by orthomosaic generation and image-based annotation of benthic organisms and substrate features.",
        "Methodological details in Danis et al. 2023 (https://doi.org/10.5281/zenodo.8013722) and Katz et al. 2025 (https://doi.org/10.1007/s00300-025-03407-4)."
      ),
      eventType == "transect" & campaign == "TANGO1" ~ paste(
        "ROV linear video transect survey with evenly spaced, non-overlapping image annotation of benthic organisms and substrate features.",
        "Methodological details in Danis et al. 2023 (https://doi.org/10.5281/zenodo.8013722) and Katz et al. 2025 (https://doi.org/10.1007/s00300-025-03407-4)."
      ),
      eventType == "transect" & campaign == "TANGO2" ~ paste(
        "ROV linear video transect survey with evenly spaced, non-overlapping image annotation of benthic organisms and substrate features.",
        "Methodological details in Danis et al. 2024 (https://doi.org/10.5281/zenodo.11653690) and Katz et al. 2026 (https://doi.org/10.1002/ece3.73392)."
      ),
      TRUE ~ NA_character_
    ),
    samplingEffort = case_when(
      !is.na(transect_time_min) & !is.na(calculated_dist_m) ~
        str_c(
          transect_time_min,
          " minutes ROV transect covering approximately ",
          round(calculated_dist_m),
          " m"
        ),
      TRUE ~ NA_character_
    ),
    dynamicProperties = as.character(toJSON(
      purrr::compact(list(
        campaign = campaign,
        rovID = rovID,
        transect = transect,
        TANGO_sample_ID = TANGO_sample_ID,
        station = station,
        number = number,
        new_name = new_name,
        name_in_paper = name_in_paper,
        site = site,
        deployed_from = deployed_from,
        heading_ROV = heading_ROV
      )),
      auto_unbox = TRUE
    ))
  ) 
  

# create rov level events
rov_events <- event %>%
  filter(!is.na(rovID)) %>%
  group_by(campaign, rovID) %>%
  summarise(
    eventID = paste0(first(campaign), "_", first(rovID)),
    parentEventID = first(parentEventID),
    eventType = "ROV deployment",
    eventDate = first(na.omit(eventDate)),
    locality = first(na.omit(locality)),
    verbatimLocality = first(na.omit(verbatimLocality)),
    higherGeographyID = first(na.omit(higherGeographyID)),
    .groups = "drop"
  ) %>%
  mutate(
    samplingProtocol = case_when(
      first(campaign) == "TANGO1" ~ paste(
        "ROV deployment supporting image-based benthic survey.",
        "Methodological details in Danis et al. 2023 (https://doi.org/10.5281/zenodo.8013722)",
        "and Katz et al. 2025 (https://doi.org/10.1007/s00300-025-03407-4)."
      ),
      first(campaign) == "TANGO2" ~ paste(
        "ROV deployment supporting image-based benthic survey.",
        "Methodological details in Danis et al. 2024 (https://doi.org/10.5281/zenodo.11653690)",
        "and Katz et al. 2026 (https://doi.org/10.1002/ece3.73392)."
      ),
      TRUE ~ NA_character_
    )
  )

event_children <- event %>%
  mutate(parentEventID = paste0(campaign, "_", rovID))

event_hierarchical <- bind_rows(
  rov_events,
  event_children
  ) %>%
  # expedition level events
  add_row(
    campaign = "TANGO1",
    eventID = "https://www.wikidata.org/entity/Q119843670",
    eventType = "expedition"
  ) %>%
  add_row(
    campaign = "TANGO2",
    eventID = "https://www.wikidata.org/entity/Q137398578",
    eventType = "expedition"
  ) %>%
  select(any_of(dwc_event_fields)) %>%  # only select columns that are in DwC Event
  relocate(eventID, parentEventID, eventType)  # put these 3 fields first



# HUMBOLDT EXTENSION for survey design and effort (only for child events, not expedition or ROV deployment level events)
eco <- event %>%
  filter(eventType != "expedition") %>%
  select(eventID, campaign, station, transect, number, site, transect_time_min, calculated_dist, name_in_paper) %>%
  rename(
    eventDuration = transect_time_min,
    samplingEffortValue = calculated_dist,
    verbatimSiteNames = name_in_paper,
  ) %>%
  mutate(
    eventDurationUnit = "minute",
    samplingEffortUnit = "meter",
    targetHabitatScope = "benthic",
    verbatimTargetScope = "Visible shallow-water Antarctic benthic fauna, macroalgae, and substrate features detectable from ROV imagery",
    protocolNames = "ROV video transect survey",
    protocolDescriptions = case_when(
      campaign == "TANGO1" ~
        paste(
          "ROV video transect/quadrant survey with image-based annotation of benthic organisms and substrate features.",
          "Methodological details in Danis et al. 2023 (https://doi.org/10.5281/zenodo.8013722)",
          "and Katz et al. 2025 (https://doi.org/10.1007/s00300-025-03407-4)."
        ),
      campaign == "TANGO2" ~
        paste(
          "ROV video transect survey with image-based annotation of benthic organisms and substrate features.",
          "Methodological details in Danis et al. 2024 (https://doi.org/10.5281/zenodo.11653690)",
          "and Katz et al. 2026 (https://doi.org/10.1002/ece3.73392)."
        ),
      TRUE ~ NA_character_
    ),
    protocolReferences = case_when(
      campaign == "TANGO1" ~
        paste(
          "Danis, Bruno, Maria Amenabar, Annette Bombosch, Axelle Brusselman, Marius Buydens, Bruno Delille, Martin Dogniez, et al. “Report of the TANGO 1 Expedition to the West Antarctic Peninsula”. Zenodo, June 7, 2023. https://doi.org/10.5281/zenodo.8013722",
          "Katz, L., Khan, T.M., Moreau, C. et al. Using Bayesian network inference and underwater imagery to understand the influence of environmental heterogeneities on benthic community structure in the Antarctic Peninsula. Polar Biol 48, 91 (2025). https://doi.org/10.1007/s00300-025-03407-4",
          sep = " | "
        ),
      campaign == "TANGO2" ~
        paste(
          "Danis, Bruno. “Report of the TANGO 2 Expedition to the West Antarctic Peninsula”. Zenodo, June 14, 2024. https://doi.org/10.5281/zenodo.11653690",
          "Katz, L., E.Mitchell, B.Danis, and H.Griffiths. 2026. “Microhabitat Patchiness Structures Benthic Biodiversity in the Western Antarctic Peninsula.” Ecology and Evolution16, no. 4: e73392. https://doi.org/10.1002/ece3.73392.",
          sep = " | "
        ),
      TRUE ~ NA_character_
    ),
  ) 

# OCCURRENCE
taxon_map <- read_tsv(here("data", "01_raw", "full_header_mapping_CORRECTION.tsv"), na = c("", "NA")) %>%
  distinct(header, .keep_all = TRUE)

catami_lookup <- read_tsv(here("data", "02_interim", "catami_dwc_lookup.tsv"), na = c("", "NA")) %>%
  select(header, catamiPath, catamiID = id, catamiName = name) %>%
  filter(!is.na(header)) %>%
  distinct(header, .keep_all = TRUE)

# TAXON MATCH
worms_lookup <- taxon_map %>%
  distinct(scientificName) %>%
  # add a row for records with no scientificName 
  bind_rows(tibble(scientificName = "Biota")) %>%
  filter(!is.na(scientificName)) %>%
  mutate(
    worms = map(scientificName, wm_records_name),
        scientificNameID = map_chr(worms, ~ .x$lsid[1] %||% NA_character_
    ),
    matchedName = map_chr(worms, ~ .x$scientificname[1] %||% NA_character_),
    taxonRank = map_chr(worms, ~ .x$rank[1] %||% NA_character_)
  ) %>%
  select(
    scientificName,
    scientificNameID,
    matchedName,
    taxonRank
  )


# ---- helper: derive eventID from abundance filename ----
derive_event_from_file <- function(x, campaign) {
  x <- str_remove(x, "\\.(png|tif|tiff|jpg|jpeg)$")
  x <- str_remove(x, "_whole$")
  if (campaign == "TANGO1") {
    # e.g. TANGO1_ROV13_q1_whole.tif -> TANGO1_ROV13_q1
    str_extract(x, "^TANGO1_[^_]+_[^_]+")
  } else {
    # e.g. TANGO2_ROV10_1_036.png -> campaign = TANGO2, rovID = ROV10, number = 1
    # use event table because number 2 is not always t2, e.g. ROV14 number 2 = q1
    NA_character_
  }
}

# ---- read and reshape TANGO1 ----
tango1_long <- tango1_abd %>%
  rename(imageID = old_file_name,
         sourceFileName = new_file_name) %>%
  mutate(
    campaign = "TANGO1",
    eventID = derive_event_from_file(sourceFileName, "TANGO1"),
    eventID = case_when(
      # manually map these 2 events to TANGO1_ROV7_t1 (confirmed with Lea)
      eventID %in% c("TANGO1_ROV7_6", "TANGO1_ROV7_7") ~ "TANGO1_ROV7_t1",
      TRUE ~ eventID
    )
  ) %>%
  pivot_longer(
    cols = -any_of(c(
      "campaign", "imageID", "sourceFileName", "eventID"
    )),
    names_to = "header",
    values_to = "organismQuantity"
  )

# ---- read and reshape TANGO2 ----
percent_cover_labels <- c(
  "Macroalgae",
  "branching_red_algae_msp1",
  "branching_red_algae_msp2",
  "desmarestia_msp1",
  "desmarestia_msp2",
  "himantothallus_msp1",
  "pink_encr_algae",
  "red_or_brown_sheet_algae",
  "red_sheet_msp1"
  # add colonial animals / substrate cover labels here too if needed
)

tango2_long <- tango2_abd %>%
  rename(sourceFileName = file_name) %>%
  mutate(
    campaign = "TANGO2",
    imageID = sourceFileName,
    rovID = str_extract(sourceFileName, "ROV\\d+|DIV\\d+"),
    number = str_match(sourceFileName, "^TANGO2_[^_]+_([^_]+)_")[, 2]
  ) %>%
  left_join(
    event %>%
      filter(campaign == "TANGO2") %>%
      mutate(number = as.character(number)) %>%
      select(campaign, rovID, number, eventID),
    by = c("campaign", "rovID", "number")
  ) %>%
  pivot_longer(
    cols = -any_of(c(
      "campaign", "imageID", "sourceFileName", "rovID",
      "number", "eventID"
    )),
    names_to = "header",
    values_to = "organismQuantity"
  )

# occurrence table
occ <- bind_rows(tango1_long, tango2_long) %>%
  mutate(
    organismQuantity = parse_number(as.character(organismQuantity))
  ) %>%
  filter(!is.na(organismQuantity)) %>%
  left_join(taxon_map, by = "header") %>%
  left_join(catami_lookup, by = "header") %>%
  mutate(
    # if eventID is empty, link occurrence to ROV deployment event based on file name
    eventID = na_if(eventID, ""),
    eventID = coalesce(
      eventID,
      str_extract(sourceFileName, "^[^_]+_[^_]+")
    ),
    occurrenceID = glue("{imageID}_{header}"),
    scientificName = if_else(
      is.na(scientificName) | str_trim(scientificName) == "",
      "Biota",
      scientificName
    ),
    basisOfRecord = "MachineObservation",
    occurrenceStatus = case_when(
      organismQuantity > 0 ~ "detected",
      organismQuantity == 0 ~ "notDetected"
    ),
    organismQuantityType = if_else(
      verbatimIdentification %in% percent_cover_labels,
      "percent cover",
      "individual count"
    ),
    organismQuantity = case_when(
      organismQuantityType == "individual count" ~ round(organismQuantity, 0),
      TRUE ~ organismQuantity
    ),
    identificationReferences = "https://doi.org/10.1371/journal.pone.0141039 | https://doi.org/10.5281/zenodo.12653521",
    identificationRemarks = if_else(
      is.na(catamiPath),
      "Image-based morphotaxon identification using CATAMI labels; taxonomic resolution varies and records represent visible organisms only.",
      str_c(
        "Image-based morphotaxon identification using CATAMI labels; taxonomic resolution varies and records represent visible organisms only. CATAMI path: ",
        catamiPath
        )
      )
    ) %>%
  select(
    occurrenceID,
    eventID,
    basisOfRecord,
    occurrenceStatus,
    organismQuantity,
    organismQuantityType,
    scientificName,
    taxonRank,
    vernacularName,
    verbatimIdentification,
    catamiPath,
    catamiID,
    catamiName,
    identificationReferences,
    identificationRemarks,
    sourceFileName,
    imageID,
    header
  ) %>%
left_join(
  worms_lookup %>%
    select(
      scientificName,
      scientificNameID,
      taxonRank
    ) %>%
    distinct(scientificName, .keep_all = TRUE),
  by = "scientificName"
) %>%
  rename(taxonRank = taxonRank.x) %>%
  select(-taxonRank.y) # remove duplicated column

# ---- QC checks ----
# eventIDs in occ not found in event_hierarchical table
missing_ids <- occ %>%
  anti_join(event_hierarchical, by = "eventID") %>%
  distinct(eventID)
missing_ids

occ %>% filter(is.na(scientificName), is.na(verbatimIdentification)) %>% count(header, sort = TRUE)
occ %>% filter(is.na(scientificName)) %>% count(verbatimIdentification, sort = TRUE)

img_eventID <- bind_rows(tango1_long, tango2_long) %>% 
  select(-header, -organismQuantity) %>%
  full_join(event, by = "eventID") %>%  # to include those with eventID = NA
  select(sourceFileName, eventID, campaign.x, rovID.x, rovID.y, number.x, number.y, transect) %>%
  rename(campaign = campaign.x) %>%
  mutate(
    rovID = coalesce(rovID.x, rovID.y),
    number = coalesce(number.x, number.y)
  ) %>%
  select(-rovID.x, -rovID.y, -number.x, -number.y)

# MEDIA
# find photos and record the directories
media_dir <- Sys.getenv("MEDIA_DIR")

media <- tibble(
  file_path = list.files(
    path = media_dir,
    recursive = TRUE,
    full.names = TRUE
  )
) %>%
  mutate(
    file_name = basename(file_path),
    relative_path = str_remove(file_path, paste0("^", fixed(media_dir), "/"))
  ) %>%
  select(file_name, everything())

# this table will also be used to upload media to Zenodo It links the media files to the eventIDs and contains metadata that will be used as Subjects
ac <- occ %>%
  full_join(media, by = c("sourceFileName" = "file_name")) %>%
  left_join(event %>% select(eventID, campaign, parentEventID, rovID, verbatimLocality, dynamicProperties), by = "eventID") %>%
  select(eventID, sourceFileName, file_path, relative_path, campaign, parentEventID, rovID, verbatimLocality, dynamicProperties) %>%
  distinct() %>% 
  mutate(
    `dc:type` = "StillImage",
    `dcterms:type` = "http://purl.org/dc/dcmitype/StillImage",
    subtype = "http://rs.tdwg.org/acsubtype/values/Photograph",
    subtypeLiteral = "Photograph",
    title = str_c("Photo of benthic habitat from ", campaign, ", ", rovID, ", (", sourceFileName, ")"),
    fundingAttribution = "This work was supported by the “Estimating Tipping points in habitability of ANtarctic benthic ecosystems under GlObal future climate change scenarios” project (TANGO; B2/212/P1/TANGO) funded by the ‘Belgian Science Policy Office (BELSPO)",
    licenseLogoURL = "https://licensebuttons.net/l/by/4.0/80x15.png",
    metadataLanguage = "http://id.loc.gov/vocabulary/iso639-2/eng",
    metadataLanguageLiteral = "eng",
    provider = "https://orcid.org/0000-0001-5748-602X",
    providerLiteral = "Lea Katz",
    `dc:format` = case_when(
      str_detect(sourceFileName, "\\.tif$") ~ "TIFF",
      str_detect(sourceFileName, "\\.png$") ~ "PNG",
      TRUE ~ NA_character_
    ),
    `dcterms:format` = case_when(
      str_detect(sourceFileName, "\\.tif$") ~ "http://rs.tdwg.org/format/values/m011",
      str_detect(sourceFileName, "\\.png$") ~ "http://rs.tdwg.org/format/values/m007",
      TRUE ~ NA_character_
    )
  )


# write files
write_tsv(event_hierarchical, here("data", "03_processed", "event.txt"), na = "")
write_tsv(eco, here("data", "03_processed", "humboldt.txt"), na = "")
write_tsv(occ, here("data", "03_processed", "occurrence.txt"), na = "")
write_tsv(ac, here("data", "03_processed", "audiovisual.txt"), na = "")



