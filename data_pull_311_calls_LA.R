## 311 Calls Data (Los Angeles)
## Data Pulled: 10/14/2025 6:00 PM EST
## Data Source: https://data.lacity.org/browse?sortBy=relevance&pageSize=20&q=311&page=1

rm(list=ls())

library(data.table) 
library(readr) 
library(dplyr) 

## dataset ids
datasets <- c(
  "2015" = "ms7h-a45h",
  "2016" = "ndkd-k878",
  "2017" = "eq98-9f8w",
  "2018" = "h65r-yf5i",
  "2019" = "pvft-t768",
  "2020" = "rq3b-xjk8",
  "2021" = "97z7-y5bt",
  "2022" = "i5ke-k6by",
  "2023" = "4a4x-mna2",
  "2024" = "b7dx-7gc3",
  "2025" = "h73f-gn57")

base_url <- "https://data.lacity.org/resource/"

## grab dataset in chunks
fetch_year_chunks <- function(data_id, year,
                              chunk_size = 50000L,
                              sleep_seconds = 0.5,
                              verbose = TRUE) {
  offset <- 0L
  chunks <- list()
  chunk_idx <- 1L
  repeat {
    csv_url <- sprintf("%s%s.csv?$limit=%d&$offset=%d", base_url, data_id, chunk_size, offset)
    if (verbose) message(sprintf("[%s] fetching offset=%d  url=%s", year, offset, csv_url))
    df_chunk <- NULL
    try({
      df_chunk <- tryCatch(
        fread(csv_url, showProgress = FALSE, nThread = 1),
        error = function(e) {
          warning(sprintf("[%s] fread failed at offset %d: %s", year, offset, e$message))
          NULL
        }
      )
    }, silent = TRUE)
    # fallback to readr if fread failed
    if (is.null(df_chunk)) {
      try({
        df_chunk <- tryCatch(
          read_csv(csv_url, progress = FALSE, show_col_types = FALSE),
          error = function(e) {
            warning(sprintf("[%s] readr fallback failed at offset %d: %s", year, offset, e$message))
            NULL
          }
        )
        # convert to data.table for later rbind
        if (!is.null(df_chunk)) df_chunk <- as.data.table(df_chunk)
      }, silent = TRUE)
    }
    
    # If both failed -> break
    if (is.null(df_chunk)) {
      warning(sprintf("[%s] stopped fetching at offset %d (no chunk returned).", year, offset))
      break
    }
    
    nr <- nrow(df_chunk)
    if (nr == 0) {
      if (verbose) message(sprintf("[%s] zero rows returned; finishing.", year))
      break
    }
    
    # add year column
    df_chunk[, year := as.integer(year)]
    
    chunks[[chunk_idx]] <- df_chunk
    chunk_idx <- chunk_idx + 1L
    
    if (nr < chunk_size) {
      # last chunk
      if (verbose) message(sprintf("[%s] last chunk retrieved (rows=%d).", year, nr))
      break
    }
    
    offset <- offset + chunk_size
    Sys.sleep(sleep_seconds)
  } 
  
  if (length(chunks) == 0) return(NULL)
  # combine all chunks
  result <- rbindlist(chunks, fill = TRUE)
  return(result)
}

## grab all years
all_list <- list()
for (yr in names(datasets)) {
  id <- datasets[[yr]]
  message(sprintf("=== Starting year %s (id=%s) ===", yr, id))
  df_year <- tryCatch(
    fetch_year_chunks(id, yr, chunk_size = 50000L, sleep_seconds = 0.5, verbose = TRUE),
    error = function(e) {
      warning(sprintf("Error fetching %s: %s", yr, e$message))
      NULL
    }
  )
  if (!is.null(df_year)) {
    message(sprintf("[%s] fetched total rows: %d", yr, nrow(df_year)))
    # convert to data.table
    if (!is.data.table(df_year)) df_year <- as.data.table(df_year)
    all_list[[yr]] <- df_year
  } else {
    message(sprintf("[%s] no data (skipping)", yr))
  }
}

## Normalize column sets across years
combined_dt <- rbindlist(all_list, fill = TRUE, use.names = TRUE)
table(combined_dt$year) # check

## Saving
write.csv(combined_dt, "all_calls_311_LA.csv", row.names = F)
save(combined_dt, file = "all_calls_311_LA.Rda") # >12 million observations



## Filtering Homeless Calls
homeless <- combined_dt %>% 
  filter(requesttype == "Homeless Encampment")

table(homeless$year) # checks
table(homeless$requesttype)

## saving
save(homeless, file = "homeless_calls_311_LA.Rda") # total rows should be 478,777

