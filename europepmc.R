##https://cran.r-project.org/web/packages/europepmc/europepmc.pdf

library(europepmc)
library(tidyverse)
library(stringr)
library(httr)

my_query <- '(((ABSTRACT:"www" OR ABSTRACT:"http" OR ABSTRACT:"https") AND (ABSTRACT:"data" OR ABSTRACT:"resource" OR ABSTRACT:"database")) NOT (ABSTRACT:"Retraction:" OR ABSTRACT:"https://github.com" OR ABSTRACT:"http://github.com" OR ABSTRACT:"Clinical Trial Registry" OR ABSTRACT:"Clinical Trials Registry" OR ABSTRACT:"Trial registration" OR ABSTRACT:"https://clinicaltrials.gov" OR ABSTRACT:"registration number" OR ABSTRACT:"https://osf.io" OR ABSTRACT:"Pre-registration" OR ABSTRACT:"Preregistration") ) AND (((SRC:MED OR SRC:PMC OR SRC:AGR OR SRC:CBA))) AND (FIRST_PDATE:[2011 TO 2021])'
pmc_24k <- epmc_search(query=my_query, limit = 25000)

journal_counts <-count(pmc_24k, journalTitle)
year_counts <-count(pmc_24k, pubYear)
names(journal_counts)[names(journal_counts)=="journalTitle"] <- "journal"
write.csv(pmc_24k,"pmc_list_all_2021-07-09.csv", row.names = FALSE) 

#=============================================================================#

pmc_24k <- read_csv("pmc_list_all_2021-07-09.csv")

##check for NA ids....
sum(is.na(pmc_24k$id)) ## 0

## try first 5K records to see how long it takes ... 
## ~20 min 8:55 - 9:15
set1 <- slice(pmc_24k, 1:4999)
set1_list <-set1$id

## add pub year
y  <- NULL;
for (i in set1_list) {
  r <- sapply(i, epmc_details) 
  id <- r[[1]]["id"]
  title <- r[[1]]["title"]
  abstract <- r[[1]]["abstractText"]
  report <- cbind(id, title, abstract)
  y <- rbind(y, report)
}

#rename
abstracts_1_4999 <- y

#find failed queries
lost_1_4999 <- anti_join(set1, abstracts_1_4999, by = "id")
# failures largely due to source
lost_list_source <- unique(as.list(lost_1_4999$source))
lost_list <-lost_1_4999$id

# Retrieve PMC & ARC (then 1 MED fails differently)

lost_pmc  <- NULL;
for (i in lost_list) {
  r <- sapply(i, epmc_details, data_src = "PMC") 
  id <- r[[1]]["id"]
  title <- r[[1]]["title"]
  abstract <- r[[1]]["abstractText"]
  report <- cbind(id, title, abstract)
  lost_pmc <- rbind(lost_pmc, report)
}

lost_agr  <- NULL;
for (i in lost_list) {
  r <- sapply(i, epmc_details, data_src = "agr") 
  id <- r[[1]]["id"]
  title <- r[[1]]["title"]
  abstract <- r[[1]]["abstractText"]
  report <- cbind(id, title, abstract)
  lost_agr <- rbind(lost_agr, report)
}

lost_found <- rbind(lost_agr, lost_pmc)
still_lost_1_4999 <- anti_join(lost_1_4999, lost_found, by = "id")
## 12 still don't return

abstracts_1_4999 <- rbind(abstracts_1_4999, lost_found)

## Extract URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
abstracts_1_4999$ContentURL <- str_extract(abstracts_1_4999$abstractText, url_pattern)
##length to eval over long and over short
abstracts_1_4999$URL_length <- nchar(abstracts_1_4999$ContentURL)

##Clean URLs iteratively, add in new column next, also add last character column
abstracts_1_4999$cleaned_URL <- NA
abstracts_1_4999$cleaned_URL_last <- NA

abstracts_1_4999$cleaned_URL <- gsub("<.*$", "", abstracts_1_4999$ContentURL) 
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))
abstracts_1_4999$cleaned_URL <-gsub("[.)-;:?*&>]$", "", abstracts_1_4999$cleaned_URL)
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))
abstracts_1_4999$cleaned_URL <-gsub("[.)]$", "", abstracts_1_4999$cleaned_URL)
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))

##will still have a little junk but ok for now

##### NEXT ... remove URL = NA (just found WWW, etc in abstract) and dedup cleaned URLs ####

## title key words
abstracts_1_4999 <- mutate(abstracts_1_4999, title_cat = ifelse(test = grepl("data", abstracts_1_4999$title, ignore.case = T),
                                                    yes = "data",
                                                    no = ifelse(test =  grepl("base", abstracts_1_4999$title, ignore.case = T),
                                                                yes = "base",
                                                                no = ifelse(test = grepl("db", abstracts_1_4999$title, ignore.case = T),
                                                                            yes = "db",
                                                                            no = ifelse(test = grepl("model", abstracts_1_4999$title, ignore.case = T),
                                                                                        yes = "model",
                                                                                        no = ifelse(test = grepl("server", abstracts_1_4999$title, ignore.case = T),
                                                                                                    yes = "server",
                                                                                                    no = ifelse(test = grepl("tool", abstracts_1_4999$title, ignore.case = T),
                                                                                                                yes = "tool",
                                                                                                                no = ifelse(test = grepl("study", abstracts_1_4999$title, ignore.case = T),
                                                                                                                            yes = "study",
                                                                                                                            no = ifelse(test = grepl("package", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                        yes = "package",
                                                                                                                                        no = ifelse(test = grepl("program", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                                    yes = "program",
                                                                                                                                                    no = "other"))))))))))
                                                                                                                                                 
                                                                                                                                                    

## check count quick
t1 <- abstracts_1_4999 %>% count(title_cat)
## save file
write.csv(abstracts_1_4999,"abstracts_1_4999_2021-07-23.csv", row.names = FALSE) 

#============================## WORK AREA ##===============================#

## viewing single abstracts
print <- abstracts_1_4999[18,]
View(print)
print$abstractText

## am I at least catching all the journals in this time frame?
wren_journal_stats_2011_2015 <- read_csv("wren_journal_stats_2011_2015.csv")
journal_check <- full_join(wren_journal_stats_2011_2015, journal_counts, by="journal")
journal_check <- filter(journal_check, db >0)
journal_check <- filter(journal_check, is.na(n))
sum(journal_check$all)

write.csv(journal_check ,"missing_journals_2021-07-09.csv", row.names = FALSE) 
## 33 journals, 36 total urls (not necessarily dbs btw) -- 46 total (db abd programs)

sum(wren_journal_stats_2011_2015$all)
length(wren_journal_stats_2011_2015$journal)

## 36 out of 8885 urls = 0.4%
## 33 out of 1570 for this time period = 2% 

## Testing epmc returns
# Huang, et al 2020
# AcrDB: a database of anti-CRISPR operons in prokaryotes and viruses, 
# Nucleic Acids Research
# https://doi.org/10.1093/nar/gkaa857

test_full_text <- epmc_ftxt("PMC7778997") ## not working?
test_details <- epmc_details(ext_id = 33068435)

## from epmc_details can get ...
## * basic -> affiliation - DO NOT USE ... first author affiliation only
## * author_details -> lastName (for list of last names only)
## * authorAffiliationDetailsList -> authorAffiliation = for each author... last will most likely be the cooresponding author
## * mesh_topic -> descriptorName  - returns more than descriptorName for qualifiers (below)!
## * mesh_qualifiers -> qualifiersName 
## * grants -> DO NOT USE ... did not return above