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
##Clean URLs iteratively
abstracts_1_4999$ContentURL <- gsub("<.*$", "", abstracts_1_4999$ContentURL) 
abstracts_1_4999$ContentURL <-gsub("[.)-;:?*&>]$", "", abstracts_1_4999$ContentURL)
abstracts_1_4999$ContentURL <-gsub("[.)-;:?*&>]$", "", abstracts_1_4999$ContentURL)
##will still have a little junk but ok for now
## title key words
abstracts_1_4999 <- mutate(abstracts_1_4999, title_cat = ifelse(test = grepl("data", abstracts_1_4999$title, ignore.case = T),
                                                    yes = "data",
                                                    no = ifelse(test =  grepl("base", abstracts_1_4999$title, ignore.case = T),
                                                                yes = "base",
                                                                no = ifelse(test = grepl("db", test$title, ignore.case = T),
                                                                            yes = "db",
                                                                            no = ifelse(test = grepl("model", test$title, ignore.case = T),
                                                                                        yes = "model",
                                                                                        no = ifelse(test = grepl("server", test$title, ignore.case = T),
                                                                                                    yes = "server",
                                                                                                    no = ifelse(test = grepl("program", test$title, ignore.case = T),
                                                                                                                yes = "program",
                                                                                                                no = ifelse(test = grepl("study", test$title, ignore.case = T),
                                                                                                                            yes = "study",
                                                                                                                            no = "other"))))))))
## check count quick
t1 <- abstracts_1_4999 %>% count(title_cat)

write.csv(abstracts_1_4999,"abstracts_1_4999_2021-07-23.csv", row.names = FALSE) 


#============================## WORK AREA ##===============================#

#retrieve status codes
test_status_list <- abstracts_1_4999$ContentURL

# NOTE: websites that aren't secure or throw other kinds of hand-off errors are ignored via 
# tryCatch function e.g. see this return -- GET("https://echinobase.org")...
# Error in curl::curl_fetch_memory(url, handle = handle) : 
#   SSL: no alternative certificate subject name matches target host name 'echinobase.org'

##URL testing loop
## start 2:43 PM
##
## do a job next time https://blog.rstudio.com/2019/03/14/rstudio-1-2-jobs/
test_status  <- NULL;
for (i in test_status_list) {
  delayedAssign("do.next", {next})
  r <- tryCatch(sapply(i, GET), error = function(e) force(do.next))
  ContentURL <- r[[1]]
  status_code <- r[[2]]
  report <- cbind(ContentURL, status_code)
  test_status <- as.data.frame(rbind(test_status, report))
}
## several hundred duplicated
## many should have returned but didn't
## ***** NEEDS TROUBLE SHOOTING ****
test_status_dedup <- unique(test_status) 
write.csv(test_status_dedup,"abstracts_1_4999_test_status_2021-07-23.csv", row.names = FALSE) 

## ***** NEEDS TROUBLE SHOOTING ****
abstracts_1_4999_URL_checked <- left_join(abstracts_1_4999, test_status, by="ContentURL")

##check for failed URL handoffs
sum(is.na(abstracts_1_4999_URL_checked$status_code))
##summarize results
summary_URL_checks_1_4999 <- aggregate(abstracts_1_4999_URL_checked$status_code, by=list(status_code=abstracts_1_4999_URL_checked$status_code), FUN=sum)

##examine last characters of URL string
n = 5
last_char <- unique(as.data.frame(substr(test_1_4999$ContentURL,(nchar(test_1_4999$ContentURL)+1)-n,nchar(test_1_4999$ContentURL))))


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

## data in title 

test <- mutate(abstracts_1_4999, title_cat = ifelse(test = grepl("data", abstracts_1_4999$title, ignore.case = T),
                                 yes = "data",
                                 no = ifelse(test =  grepl("base", abstracts_1_4999$title, ignore.case = T),
                                             yes = "base",
                                             no = ifelse(test = grepl("db", test$title, ignore.case = T),
                                                         yes = "db",
                                                         no = ifelse(test = grepl("model", test$title, ignore.case = T),
                                                                    yes = "model",
                                                                    no = ifelse(test = grepl("server", test$title, ignore.case = T),
                                                                                yes = "server",
                                                                                no = ifelse(test = grepl("program", test$title, ignore.case = T),
                                                                                            yes = "program",
                                                                                            no = ifelse(test = grepl("study", test$title, ignore.case = T),
                                                                                                        yes = "study",
                                                                                                        no = "other"))))))))

t1 <- test %>% count(title_cat)


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