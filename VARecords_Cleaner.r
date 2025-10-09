## VA General District Court Caseload Reports (2010-2018)
## Data Wrangling: Misdemeanor and Felony Filings

rm(list=ls())

library(pdftools)
library(tidyverse)
library(stringr)

setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Minority Politics (Krishnamurthy)/VA Court Records")
getwd()

#
############## Data Extraction and cleanup ###########

## 2018
pdf_2018 <- "dbr1_2018.pdf"
records_2018 <- pdf_text(pdf_2018)
cat(records_2018[1]) # first page

## extracting data
extract_2018 <- tibble(
  year = "2018",
  court_ID = str_extract(records_2018, "Court ID: \\d+"),
  district = str_extract(records_2018, "District: \\d+"),
  name = str_extract(records_2018, "(?i)(\\b[A-Za-z]+(?:-[A-Za-z]+)?\\b\\s?){1,4}(?=\\sTotals)"),
  misdemeanor = str_extract(records_2018, "(M\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2018, "(F\\s{1,})\\d{1,3}(,\\d{3})*")
)

## 2018: Newport News, Richmond, and Norfolk are all split up and have different court IDs. 
## There are felony and misdemeanor for only some of these courts, so get some NAs

## post-cleanup of extracted 2018 data
extract_2018 <- extract_2018 %>%
  filter(!is.na(name)) # removing only NAs in name variable so I don't accidentally remove a court with NA filings
extract_2018$court_ID <- gsub("[^0-9,]", "", extract_2018$court_ID)
extract_2018$district <- gsub("[^0-9,]", "", extract_2018$district)
extract_2018$misdemeanor <- gsub("[^0-9,]", "", extract_2018$misdemeanor)
extract_2018$felony <- gsub("[^0-9,]", "", extract_2018$felony)
extract_2018$misdemeanor <- as.numeric(gsub(",", "", extract_2018$misdemeanor))
extract_2018$felony <- as.numeric(gsub(",", "", extract_2018$felony))

## 2017
pdf_2017 <- "dbr1_2017.pdf"
records_2017 <- pdf_text(pdf_2017)
extract_2017 <- tibble(
  year = "2017",
  court_ID = str_extract(records_2017, "Court ID: \\d+"),
  district = str_extract(records_2017, "District: \\d+"),
  name = str_extract(records_2017, "(?i)(\\b[A-Za-z]+(?:-[A-Za-z]+)?\\b\\s?){1,4}(?=\\sTotals)"),
  misdemeanor = str_extract(records_2017, "(M\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2017, "(F\\s{1,})\\d{1,3}(,\\d{3})*")
)
extract_2017 <- extract_2017 %>%
  filter(!is.na(name))
extract_2017$court_ID <- gsub("[^0-9,]", "", extract_2017$court_ID)
extract_2017$district <- gsub("[^0-9,]", "", extract_2017$district)
extract_2017$misdemeanor <- gsub("[^0-9,]", "", extract_2017$misdemeanor)
extract_2017$felony <- gsub("[^0-9,]", "", extract_2017$felony)
extract_2017$misdemeanor <- as.numeric(gsub(",", "", extract_2017$misdemeanor))
extract_2017$felony <- as.numeric(gsub(",", "", extract_2017$felony))

## 2016
pdf_2016 <- "dbr1_2016.pdf"
records_2016 <- pdf_text(pdf_2016)
extract_2016 <- tibble(
  year = "2016",
  court_ID = str_extract(records_2016, "Court ID: \\d+"),
  district = str_extract(records_2016, "District: \\d+"),
  name = str_extract(records_2016, "(?i)(\\b[A-Za-z]+(?:-[A-Za-z]+)?\\b\\s?){1,4}(?=\\s(Totals)\\b)"),
  misdemeanor = str_extract(records_2016, "(M\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2016, "(F\\s{1,})\\d{1,3}(,\\d{3})*")
)
extract_2016 <- extract_2016 %>%
  filter(!is.na(name))
extract_2016$court_ID <- gsub("[^0-9,]", "", extract_2016$court_ID)
extract_2016$district <- gsub("[^0-9,]", "", extract_2016$district)
extract_2016$misdemeanor <- gsub("[^0-9,]", "", extract_2016$misdemeanor)
extract_2016$felony <- gsub("[^0-9,]", "", extract_2016$felony)
extract_2016$misdemeanor <- as.numeric(gsub(",", "", extract_2016$misdemeanor))
extract_2016$felony <- as.numeric(gsub(",", "", extract_2016$felony))

## Altering additional things the 2016 extract didn't pick up
extract_2016 <- extract_2016[-132,] # Removing Total Virginia observation
extract_2016[4, 5:6] <- NA # Norfolk-Civil should be NA filings
# Manually making new obs for Newport News-Criminal & Richmond-John Marshall because they weren't picked up
new_obs1 <- data.frame(
  year = 2016,
  court_ID = 207701,
  district = 7,
  name = "Newport News-Criminal",
  misdemeanor = 4807,
  felony = 2512
)

new_obs2 <- data.frame(
  year = 2016,
  court_ID = 213762,
  district = 13,
  name = "Richmond-John Marshall",
  misdemeanor = 1345,
  felony = 592
)

extract_2016 <- rbind(extract_2016, new_obs1) # attaching new obs
extract_2016 <- rbind(extract_2016, new_obs2)

## 2016 NOTES: 
## 2016 is missing an observation because there is not a standalone Norfolk
## Richmond-Traffic became Richmond-John Marshall

## 2015 
pdf_2015 <- "dbr1_2015.pdf"
records_2015 <- pdf_text(pdf_2015)
cat(records_2015[2])
extract_2015 <- tibble(
  year = "2015",
  court_ID = str_extract(records_2015, "Court ID: \\d+"),
  district = str_extract(records_2015, "District: \\d+"),
  name = str_extract(records_2015, "(?i)(\\b[A-Za-z]+(?:-[A-Za-z]+)?\\b\\s?){1,4}(?=\\s(Totals)\\b)"),
  misdemeanor = str_extract(records_2015, "(M\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2015, "(F\\s{1,})\\d{1,3}(,\\d{3})*")
)
extract_2015 <- extract_2015[-1,] # Removing Total Virginia observation
extract_2015$court_ID <- gsub("[^0-9,]", "", extract_2015$court_ID)
extract_2015$district <- gsub("[^0-9,]", "", extract_2015$district)
extract_2015$misdemeanor <- gsub("[^0-9,]", "", extract_2015$misdemeanor)
extract_2015$felony <- gsub("[^0-9,]", "", extract_2015$felony)
extract_2015$misdemeanor <- as.numeric(gsub(",", "", extract_2015$misdemeanor))
extract_2015$felony <- as.numeric(gsub(",", "", extract_2015$felony))

extract_2015[80, 4] <- "Newport News-Civil"
extract_2015[81, 4] <- "Newport News-Criminal"
extract_2015[82, 4] <- "Newport News-Traffic"

## 2015: Missing observations because Norfolk is not split up

## 2014
pdf_2014 <- "dbr1_2014.pdf"
records_2014 <- pdf_text(pdf_2014)
cat(records_2014[2]) # there are no court IDs and table formats have changed
extract_2014 <- tibble(
  year = "2014",
  court_ID = str_extract(records_2014, "Court ID: \\d+"),
  district = str_extract(records_2014, "District \\d+"),
  name = str_extract(records_2014, "(?i)(\\b[A-Za-z]+(?:-[A-Za-z]+)?\\b\\s?){1,4}(?=\\s(Caseload)\\b)"),
  misdemeanor = str_extract(records_2014, "(Misdemeanor\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2014, "(Felony\\s{1,})\\d{1,3}(,\\d{3})*")
)
extract_2014$court_ID <- gsub("[^0-9,]", "", extract_2014$court_ID)
extract_2014$district <- gsub("[^0-9,]", "", extract_2014$district)
extract_2014$misdemeanor <- gsub("[^0-9,]", "", extract_2014$misdemeanor)
extract_2014$felony <- gsub("[^0-9,]", "", extract_2014$felony)
extract_2014$misdemeanor <- as.numeric(gsub(",", "", extract_2014$misdemeanor))
extract_2014$felony <- as.numeric(gsub(",", "", extract_2014$felony))

## 2014 missing an obs because Richmond-Marsh isn't there


## 2013
pdf_2013 <- "dbr1_2013.pdf"
records_2013 <- pdf_text(pdf_2013)
cat(records_2013[1]) # table formats have drastically changed. Contains J&DR Courts
extract_2013 <- tibble(
  year = "2013",
  court_ID = str_extract(records_2013, "(?<!\\d)(\\d{6})(?=\\s*CASELOAD)"),
  district = str_extract(records_2013, "District \\d+"), ## No actual district number per page
  name = str_extract(records_2013, "(?i)(\\b[A-Za-z&]+(?:-[A-Za-z&]+)?\\b\\s?){1,4}(?=\\s*COMMONWEALTH)"),
  misdemeanor = str_extract(records_2013, "(MISDEMEANORS\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2013, "(FELONIES\\s{1,})\\d{1,3}(,\\d{3})*")
)
extract_2013 <- extract_2013 %>%
  filter(!is.na(name))
extract_2013$misdemeanor <- gsub("[^0-9,]", "", extract_2013$misdemeanor)
extract_2013$felony <- gsub("[^0-9,]", "", extract_2013$felony)
extract_2013$misdemeanor <- as.numeric(gsub(",", "", extract_2013$misdemeanor))
extract_2013$felony <- as.numeric(gsub(",", "", extract_2013$felony))
extract_2013 <- extract_2013 %>%
  distinct(court_ID, .keep_all = TRUE) # removing dupes and keeping first occurrence
extract_2013$name <- str_to_title(extract_2013$name)

## Many Juvenile & Domestic Relations courts that were put on a separate page so became their own observation
## Removing these based on their COURT_ID
extract_2013$court_ID <- as.numeric(extract_2013$court_ID)
extract_2013 <- extract_2013 %>%
  filter(!(court_ID >= 400000 & court_ID <= 600000))


## 2012
pdf_2012 <- "dbr1_2012.pdf"
records_2012 <- pdf_text(pdf_2012)
extract_2012 <- tibble(
  year = "2012",
  court_ID = str_extract(records_2012, "(?<!\\d)(\\d{6})(?=\\s*CASELOAD)"),
  district = str_extract(records_2012, "District \\d+"),
  name = str_extract(records_2012, "(?i)(\\b[A-Za-z&]+(?:-[A-Za-z&]+)?\\b\\s?){1,4}(?=\\s*COMMONWEALTH)"),
  misdemeanor = str_extract(records_2012, "(MISDEMEANORS\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2012, "(FELONIES\\s{1,})\\d{1,3}(,\\d{3})*")
)
extract_2012 <- extract_2012 %>%
  filter(!is.na(name))
extract_2012$misdemeanor <- gsub("[^0-9,]", "", extract_2012$misdemeanor)
extract_2012$felony <- gsub("[^0-9,]", "", extract_2012$felony)
extract_2012$misdemeanor <- as.numeric(gsub(",", "", extract_2012$misdemeanor))
extract_2012$felony <- as.numeric(gsub(",", "", extract_2012$felony))
extract_2012 <- extract_2012 %>%
  distinct(court_ID, .keep_all = TRUE) # removing dupes and keeping first occurrence
extract_2012$name <- str_to_title(extract_2012$name)
extract_2012$court_ID <- as.numeric(extract_2012$court_ID)
extract_2012 <- extract_2012 %>%
  filter(!(court_ID >= 400000 & court_ID <= 600000))

## 2011
pdf_2011 <- "dbr1_2011.pdf"
records_2011 <- pdf_text(pdf_2011)
extract_2011 <- tibble(
  year = "2011",
  court_ID = str_extract(records_2011, "(?<!\\d)(\\d{6})(?=\\s*CASELOAD)"),
  district = str_extract(records_2011, "District \\d+"),
  name = str_extract(records_2011, "(?i)(\\b[A-Za-z&]+(?:-[A-Za-z&]+)?\\b\\s?){1,4}(?=\\s*COMMONWEALTH)"),
  misdemeanor = str_extract(records_2011, "(MISDEMEANORS\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2011, "(FELONIES\\s{1,})\\d{1,3}(,\\d{3})*")
)
extract_2011 <- extract_2011 %>%
  filter(!is.na(name))
extract_2011$misdemeanor <- gsub("[^0-9,]", "", extract_2011$misdemeanor)
extract_2011$felony <- gsub("[^0-9,]", "", extract_2011$felony)
extract_2011$misdemeanor <- as.numeric(gsub(",", "", extract_2011$misdemeanor))
extract_2011$felony <- as.numeric(gsub(",", "", extract_2011$felony))
extract_2011 <- extract_2011 %>%
  distinct(court_ID, .keep_all = TRUE)
extract_2011$name <- str_to_title(extract_2011$name)
extract_2011$court_ID <- as.numeric(extract_2011$court_ID)
extract_2011 <- extract_2011 %>%
  filter(!(court_ID >= 400000 & court_ID <= 600000))

## 2010
pdf_2010 <- "dbr1_2010.pdf"
records_2010 <- pdf_text(pdf_2010)
extract_2010 <- tibble(
  year = "2010",
  court_ID = str_extract(records_2010, "(?<!\\d)(\\d{6})(?=\\s*CASELOAD)"),
  district = str_extract(records_2010, "District \\d+"),
  name = str_extract(records_2010, "(?i)(\\b[A-Za-z&]+(?:-[A-Za-z&]+)?\\b\\s?){1,4}(?=\\s*COMMONWEALTH)"),
  misdemeanor = str_extract(records_2010, "(MISDEMEANORS\\s{1,})\\d{1,3}(,\\d{3})*"),
  felony = str_extract(records_2010, "(FELONIES\\s{1,})\\d{1,3}(,\\d{3})*")
)
extract_2010 <- extract_2010 %>%
  filter(!is.na(name))
extract_2010$misdemeanor <- gsub("[^0-9,]", "", extract_2010$misdemeanor)
extract_2010$felony <- gsub("[^0-9,]", "", extract_2010$felony)
extract_2010$misdemeanor <- as.numeric(gsub(",", "", extract_2010$misdemeanor))
extract_2010$felony <- as.numeric(gsub(",", "", extract_2010$felony))
extract_2010 <- extract_2010 %>%
  distinct(court_ID, .keep_all = TRUE) 
extract_2010$name <- str_to_title(extract_2010$name)
extract_2010$court_ID <- as.numeric(extract_2010$court_ID)
extract_2010 <- extract_2010 %>%
  filter(!(court_ID >= 400000 & court_ID <= 600000))

#
############## Cleaning up district variable in 2010-2013 ###########

## Changing Court ID numbers to align with future ones by altering first number to 2
extract_2013$court_ID <- sub("^.", "2", extract_2013$court_ID)
extract_2012$court_ID <- sub("^.", "2", extract_2012$court_ID)
extract_2011$court_ID <- sub("^.", "2", extract_2011$court_ID)
extract_2010$court_ID <- sub("^.", "2", extract_2010$court_ID)

## Joining by Court ID and updating district numbers in older datasets
extract_2013_updated <- extract_2013 %>%
  left_join(select(extract_2017, court_ID, district), by = "court_ID", suffix = c("_2013", "_2017")) %>%
  mutate(district = ifelse(!is.na(district_2017), district_2017, district_2013)) %>%
  select(-district_2017, -district_2013)

extract_2012_updated <- extract_2012 %>%
  left_join(select(extract_2017, court_ID, district), by = "court_ID", suffix = c("_2012", "_2017")) %>%
  mutate(district = ifelse(!is.na(district_2017), district_2017, district_2012)) %>%
  select(-district_2017, -district_2012)

extract_2011_updated <- extract_2011 %>%
  left_join(select(extract_2017, court_ID, district), by = "court_ID", suffix = c("_2011", "_2017")) %>%
  mutate(district = ifelse(!is.na(district_2017), district_2017, district_2011)) %>%
  select(-district_2017, -district_2011)

extract_2011_updated[101, 6] <- "25" # 2011 Observation 101 should be district 25 based on pdf

extract_2010_updated <- extract_2010 %>%
  left_join(select(extract_2017, court_ID, district), by = "court_ID", suffix = c("_2010", "_2017")) %>%
  mutate(district = ifelse(!is.na(district_2017), district_2017, district_2010)) %>%
  select(-district_2017, -district_2010)

extract_2010_updated[101, 6] <- "25" # 2010 Observation 101 should be district 25 based on pdf

## Copying names to older data for consistency
# extract_2013_updated <- extract_2013_updated %>%
#   left_join(select(extract_2017, court_ID, name), by = "court_ID", suffix = c("_2013", "_2017")) %>%
#   mutate(name = ifelse(!is.na(name_2017), name_2017, name_2013_updated)) %>%
#   select(-name_2017, -name_2013)
# 
# extract_2012_updated <- extract_2012_updated %>%
#   left_join(select(extract_2017, court_ID, name), by = "court_ID", suffix = c("_2012", "_2017")) %>%
#   mutate(name = ifelse(!is.na(name_2017), name_2017, name_2012_updated)) %>%
#   select(-name_2017, -name_2012)
# 
# extract_2011_updated <- extract_2011_updated %>%
#   left_join(select(extract_2017, court_ID, name), by = "court_ID", suffix = c("_2011", "_2017")) %>%
#   mutate(name = ifelse(!is.na(name_2017), name_2017, name_2011_updated)) %>%
#   select(-name_2017, -name_2011)
# 
# extract_2010_updated <- extract_2010_updated %>%
#   left_join(select(extract_2017, court_ID, name), by = "court_ID", suffix = c("_2010", "_2017")) %>%
#   mutate(name = ifelse(!is.na(name_2017), name_2017, name_2010_updated)) %>%
#   select(-name_2017, -name_2010)

#
############## Aggregating by District and Year ###########

combined_data <- rbind(extract_2018, extract_2017, extract_2016, extract_2015, extract_2014, extract_2013_updated,
                       extract_2012_updated, extract_2011_updated, extract_2010_updated)

aggregate_data <- combined_data %>% 
  group_by(year, district) %>% 
  summarise(total_felony_filings = sum(felony, na.rm = T),
            total_misdemeanor_filings = sum(misdemeanor, na.rm = T))


#
############## Export to Spreadsheet ###########

library(openxlsx)

workbook <- createWorkbook()
addWorksheet(workbook, "Aggregate")
writeData(workbook, "Aggregate", aggregate_data)
addWorksheet(workbook, "Breakdown")
writeData(workbook, "Breakdown", combined_data)

saveWorkbook(workbook, "VA_Filings_Data_2010-2018.xlsx", overwrite = TRUE)

