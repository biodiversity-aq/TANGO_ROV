library(tidyverse)
library(httr2)
library(fs)
library(glue)
library(curl)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_req_perform <- function(req, max_tries = 6) {
  for (i in seq_len(max_tries)) {
    resp <- req %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
    status <- resp_status(resp)
    if (!status %in% c(429, 500, 502, 503, 504)) {
      return(resp)
    }
    wait <- as.numeric(resp_headers(resp)[["retry-after"]] %||% min(60, 5 * i))
    message("HTTP ", status, ". Waiting ", wait, " seconds before retry...")
    Sys.sleep(wait)
  }
  resp
}

get_zenodo_config <- function(sandbox = TRUE) {
  if (sandbox) {
    list(
      base_url = "https://sandbox.zenodo.org/api",
      token = Sys.getenv("ZENODO_SANDBOX_TOKEN")
    )
  } else {
    list(
      base_url = "https://zenodo.org/api",
      token = Sys.getenv("ZENODO_PROD_TOKEN")
    )
  }
}

cfg <- get_zenodo_config(sandbox = TRUE)

base_url <- cfg$base_url
token <- cfg$token

stopifnot(token != "")

test <- ac %>% head()

# Each row will become a Zenodo record
make_subjects <- function(x) {
  x <- as.list(x)
  
  subject_map <- tribble(
    ~field, ~identifier,
    "title", "http://purl.org/dc/terms/title",
    "eventID", "http://rs.tdwg.org/dwc/terms/eventID",
    "parentEventID", "http://rs.tdwg.org/dwc/terms/parentEventID",
    "verbatimLocality", "http://rs.tdwg.org/dwc/terms/verbatimLocality",
    "dc:type", "http://purl.org/dc/elements/1.1/type",
    "dcterms:type", "http://purl.org/dc/terms/type",
    "subtype", "http://rs.tdwg.org/ac/terms/subtype",
    "subtypeLiteral", "http://rs.tdwg.org/ac/terms/subtypeLiteral",
    "dc:format", "http://purl.org/dc/elements/1.1/format",
    "dcterms:format", "http://purl.org/dc/terms/format",
    "metadataLanguage", "http://rs.tdwg.org/ac/terms/metadataLanguage",
    "metadataLanguageLiteral", "http://rs.tdwg.org/ac/terms/metadataLanguageLiteral",
    "provider", "http://rs.tdwg.org/ac/terms/provider",
    "providerLiteral", "http://rs.tdwg.org/ac/terms/providerLiteral",
    "licenseLogoURL", "http://rs.tdwg.org/ac/terms/licenseLogoURL",
    "fundingAttribution", "http://rs.tdwg.org/ac/terms/fundingAttribution",
    "dynamicProperties", "http://rs.tdwg.org/dwc/terms/dynamicProperties"
  )
  
  subject_map %>%
    mutate(
      term = map_chr(field, ~ {
        value <- x[[.x]]
        
        if (is.null(value) || length(value) == 0) {
          NA_character_
        } else if (length(value) > 1) {
          paste(as.character(value), collapse = "; ")
        } else if (is.na(value)) {
          NA_character_
        } else {
          as.character(value)
        }
      })
    ) %>%
    filter(!is.na(term), term != "") %>%
    pmap(function(field, identifier, term) {
      list(
        term = term,
        identifier = identifier,
        scheme = "url"
      )
    })
}

upload_one_to_zenodo <- function(row, publish = FALSE) {
  row <- as.list(row)
  
  # 1. Create metadata
  metadata <- list(
    metadata = list(
      title = as.character(row$title),
      description = "Photo of benthic habitat from ROV imagery collected during the TANGO project in the Western Antarctic Peninsula.",
      upload_type = "image",
      image_type = "photo",
      publication_date = as.character(Sys.Date()),
      communities = list(list(identifier = "tango-rov-imagery")),
      creators = list(list(name = "TANGO expedition team")),
      access_right = "open",
      license = "cc-by-4.0",
      subjects = make_subjects(row),
      prereserve_doi = TRUE
    )
  )
  metadata_json <- jsonlite::toJSON(
    metadata,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  
  # 1. Create empty deposition
  dep <- request(glue("{base_url}/deposit/depositions")) %>%
    req_auth_bearer_token(token) %>%
    req_body_raw(metadata_json, type = "application/json") %>%
    req_method("POST") %>%
    safe_req_perform()
  
  if (resp_status(dep) >= 400) {
    stop(resp_body_string(dep))
  }
  
  resp <- resp_body_json(dep)
  deposition_id <- resp$id
  doi <- resp$metadata$prereserve_doi$doi
  doi_url <- glue("https://doi.org/{doi}")
  
  # 2. Upload file to bucket url
  bucket_url <- resp$links$bucket
  file_name <- path_file(row$file_path)
  
  file_upload <- request(paste0(bucket_url, "/", file_name)) %>%
    req_method("PUT") %>%
    req_auth_bearer_token(token) %>%
    req_body_file(row$file_path) %>%
    safe_req_perform() 
  
  if (resp_status(file_upload) >= 400) {
    stop(resp_body_string(file_upload))
  }
  
  image_url <- glue("{base_url}/records/{deposition_id}/files/{file_name}")
  
  # 4. Publish only when ready
  if (publish) {
    publish_resp <- request(resp$links$publish) %>%
      req_auth_bearer_token(token) %>%
      req_method("POST") %>%
      safe_req_perform()
    if (resp_status(publish_resp) >= 400) {
      stop(resp_body_string(publish_resp))
    }
  }
  tibble(
    sourceFileName = row$sourceFileName,
    eventID = row$eventID,
    deposition_id = deposition_id,
    doi = doi,
    doi_url = as.character(doi_url),
    image_url = as.character(image_url),
    uploaded_at = as.character(Sys.time()),
    status = "success",
    error = NA_character_
  )
}

# checkpointing
results_path <- here("data", "02_interim", "zenodo_upload_results.tsv")
failures_path <- here("data", "02_interim", "zenodo_upload_failures.tsv")

existing_results <- if (file_exists(results_path)) {
  read_tsv(results_path, show_col_types = FALSE)
} else {
  tibble(
    sourceFileName = character(),
    eventID = character(),
    status = character()
  )
}

already_done <- existing_results %>%
  filter(status == "success") %>%
  select(sourceFileName, eventID)


todo <- test %>%
  anti_join(
    existing_results %>% filter(status == "success"),
    by = c("sourceFileName", "eventID")
  )

for (i in seq_len(nrow(todo))) {
  row <- todo[i, ]
  message("Uploading ", i, " of ", nrow(todo), ": ", row$sourceFileName)
  result <- tryCatch(
    {
      upload_one_to_zenodo(row, publish = TRUE)
    },
    error = function(e) {
      tibble(
        sourceFileName = row$sourceFileName,
        eventID = row$eventID,
        deposition_id = NA_integer_,
        doi = NA_character_,
        doi_url = NA_character_,
        image_url = NA_character_,
        uploaded_at = as.character(Sys.time()),
        status = "failed",
        error = conditionMessage(e)
      )
    }
  )
  
  write_tsv(
    result,
    results_path,
    append = file_exists(results_path)
  )
  
  Sys.sleep(5)
}

zenodo_results <- read_tsv(results_path, show_col_types = FALSE) %>%
  filter(status == "success") %>%
  distinct(sourceFileName, eventID, .keep_all = TRUE)

ac_with_zenodo <- ac %>%
  left_join(
    zenodo_results %>%
      select(sourceFileName, eventID, deposition_id, doi, doi_url, image_url),
    by = c("sourceFileName", "eventID")
  )


