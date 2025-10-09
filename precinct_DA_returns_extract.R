## Precinct Level DA Returns

rm(list=ls())

library(pdftools)
library(dplyr)
library(stringr)

setwd("C:/Users/steph/Downloads/")

# pdf paths
pdf_file1_2020 <- "C:/Users/steph/Downloads/2020_precinct_pt1.pdf"
pdf_file2_2020 <- "C:/Users/steph/Downloads/2020_precinct_pt2.pdf"
pdf_file1_2024 <- "C:/Users/steph/Downloads/4324_final_precinct_public_splitfile1.pdf"
pdf_file2_2024 <- "C:/Users/steph/Downloads/4324_final_precinct_public_splitfile2.pdf"

## checking to see if I need OCR
# text_sample <- pdf_text("2020_precinct_pt2.pdf")[1]
# cat(substr(text_sample, 1, 1000)) # works
# 
# text_sample <- pdf_text("4324_final_precinct_public_splitfile1.pdf")[1]
# cat(substr(text_sample, 1, 1000)) # works

## 2020 - one
page_texts <- pdftools::pdf_text(pdf_file1_2020)

results <- lapply(page_texts, function(txt) {
  precinct <- str_extract(txt, "\\b\\d{7}[A-Z]?\\b")
  
  matches <- str_match_all(txt, "(JACKIE LACEY|GEORGE GASCON)\\D+(\\d+)")[[1]]
  
  if (nrow(matches) == 0) {
    # no matches → return empty row with precinct but NA for votes
    return(data.frame(
      precinct_number = precinct,
      DA_name = NA_character_,
      Votes = NA_real_
    ))
  }
  
  data.frame(
    precinct_number = precinct,
    DA_name = matches[, 2],
    Votes = as.numeric(matches[, 3]),
    stringsAsFactors = FALSE
  )
})

results_2020_one <- bind_rows(results)

## 2020 - two
page_texts <- pdftools::pdf_text(pdf_file2_2020)

results <- lapply(page_texts, function(txt) {
  precinct <- str_extract(txt, "\\b\\d{7}[A-Z]?\\b")
  
  matches <- str_match_all(txt, "(JACKIE LACEY|GEORGE GASCON)\\D+(\\d+)")[[1]]
  
  if (nrow(matches) == 0) {
    # no matches → return empty row with precinct but NA for votes
    return(data.frame(
      precinct_number = precinct,
      DA_name = NA_character_,
      Votes = NA_real_
    ))
  }
  
  data.frame(
    precinct_number = precinct,
    DA_name = matches[, 2],
    Votes = as.numeric(matches[, 3]),
    stringsAsFactors = FALSE
  )
})

results_2020_two <- bind_rows(results)


## 2024 - one
page_texts <- pdftools::pdf_text(pdf_file1_2024)

results <- lapply(page_texts, function(txt) {
  precinct <- str_extract(txt, "\\b\\d{7}[A-Z]?\\b")
  
  matches <- str_match_all(txt, "(NATHAN HOCHMAN|GEORGE GASCON)\\D+(\\d+)")[[1]]
  
  if (nrow(matches) == 0) {
    # no matches → return empty row with precinct but NA for votes
    return(data.frame(
      precinct_number = precinct,
      DA_name = NA_character_,
      Votes = NA_real_
    ))
  }
  
  data.frame(
    precinct_number = precinct,
    DA_name = matches[, 2],
    Votes = as.numeric(matches[, 3]),
    stringsAsFactors = FALSE
  )
})

results_2024_one <- bind_rows(results)

## 2024 - two
page_texts <- pdftools::pdf_text(pdf_file2_2024)

results <- lapply(page_texts, function(txt) {
  precinct <- str_extract(txt, "\\b\\d{7}[A-Z]?\\b")
  
  matches <- str_match_all(txt, "(NATHAN HOCHMAN|GEORGE GASCON)\\D+(\\d+)")[[1]]
  
  if (nrow(matches) == 0) {
    # no matches → return empty row with precinct but NA for votes
    return(data.frame(
      precinct_number = precinct,
      DA_name = NA_character_,
      Votes = NA_real_
    ))
  }
  
  data.frame(
    precinct_number = precinct,
    DA_name = matches[, 2],
    Votes = as.numeric(matches[, 3]),
    stringsAsFactors = FALSE
  )
})

results_2024_two <- bind_rows(results)

## adding years
results_2020_one$Year <- 2020
results_2020_two$Year <- 2020
results_2024_one$Year <- 2024
results_2024_two$Year <- 2024

## merging all together
merged_data <- rbind(results_2020_one, results_2020_two, results_2024_one, results_2024_two)

write.csv(merged_data, "precinct_DA_returns.csv", row.names = F)

