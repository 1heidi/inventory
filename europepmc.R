##https://cran.r-project.org/web/packages/europepmc/europepmc.pdf

library(europepmc)
library(tidyverse)
library(stringr)
library(httr)

##query string version 2021-08-06
my_query <- '(((ABSTRACT:"www" OR ABSTRACT:"http" OR ABSTRACT:"https") AND (ABSTRACT:"data" OR ABSTRACT:"resource" OR ABSTRACT:"database"))  NOT (TITLE:"retraction" OR TITLE:"retracted" OR TITLE:"withdrawn" OR TITLE:"withdrawal" OR TITLE:"erratum") NOT ((ABSTRACT:"retract" OR ABSTRACT:"withdraw" ABSTRACT:"erratum" OR ABSTRACT:"github.com" OR ABSTRACT:"github.io" OR ABSTRACT:"cran.r" OR ABSTRACT:"youtube.com" OR ABSTRACT:"bitbucket.org" OR ABSTRACT:"links.lww.com" OR ABSTRACT:"osf.io" OR ABSTRACT:"bioconductor.org" OR ABSTRACT:"annualreviews.org" OR ABSTRACT:"creativecommons.org" OR ABSTRACT:"sourceforge.net" OR ABSTRACT:".pdf" OR ABSTRACT:"clinical trial" OR ABSTRACT:"registry" OR ABSTRACT:"registration" OR ABSTRACT:"trial registration" OR ABSTRACT:"clinicaltrial" OR ABSTRACT:"registration number" OR ABSTRACT:"pre-registration" OR ABSTRACT:"preregistration"))) AND (((SRC:MED OR SRC:PMC OR SRC:AGR OR SRC:CBA))) AND (FIRST_PDATE:[2011 TO 2021])'
pmc_seed <- epmc_search(query=my_query, limit = 25000)

##a few checks, not a good time to de-pub by title - do later when URLs are extractec
sum(is.na(pmc_seed$id)) ## 2
pmc_seed <- filter(pmc_seed, !is.na(pmc_seed$id))
year_counts <-count(pmc_seed, pubYear) ## 18 years returned - a few pre-2011 sneak in
pmc_seed <- filter(pmc_seed, pmc_seed$pubYear > 2010)
year_counts <-count(pmc_seed, pubYear)
journal_counts <-count(pmc_seed, journalTitle)

write.csv(pmc_seed,"pmc_seed_all_2021-08-06.csv", row.names = FALSE) ## 21,235 total records

#=============================================================================#

pmc_seed <- read_csv("pmc_seed_all_2021-08-06.csv")
manual_checks <- read_csv("manual_checks_hji_2021-07-28.csv")

## using 5K records first 
##include the 653 manually checked already
man_list <-manual_checks$id
set1 <- slice(pmc_seed, 10001:14346)
set1_list <-set1$id
set1_list <- c(set1_list, man_list)

set1_list <- as.data.frame(set1_list)
check_dup_id <- data.frame(table(set1_list$set1_list)) ##doubling checking

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

# some lost - find failed queries
set1_list <- as.data.frame(set1_list)
names(set1_list)[names(set1_list)=="set1_list"] <- "id"
lost_1_4999 <- anti_join(set1_list, abstracts_1_4999, by = "id")
lost_1_4999 <- right_join(pmc_seed, lost_1_4999, by = "id")

# failures largely due to source
source_check <- lost_1_4999 %>% count(source)
lost_list <- lost_1_4999$id

# retrieve PMC & ARC

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

#combine
lost_found <- rbind(lost_agr, lost_pmc)
abstracts_1_4999 <- rbind(abstracts_1_4999, lost_found)

## extract URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
abstracts_1_4999$ContentURL <- str_extract(abstracts_1_4999$abstractText, url_pattern)
##some no URLs - there was just WWW or HTTP in the abstract
abstracts_1_4999 <- filter(abstracts_1_4999, !is.na(abstracts_1_4999$ContentURL))
##length to eval very long and very short, do not thougth out, just useful
abstracts_1_4999$URL_length <- nchar(abstracts_1_4999$ContentURL)

##Clean URLs iteratively, add in new column next, also add last character column
abstracts_1_4999$cleaned_URL <- NA
abstracts_1_4999$cleaned_URL_last <- NA

abstracts_1_4999$cleaned_URL <- gsub("<.*$", "", abstracts_1_4999$ContentURL) 
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))
abstracts_1_4999$cleaned_URL <-gsub("[.)-;:?*&>]$", "", abstracts_1_4999$cleaned_URL)
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))
abstracts_1_4999$cleaned_URL <-gsub("[.)>-]$", "", abstracts_1_4999$cleaned_URL)
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))
abstracts_1_4999$cleaned_URL <-gsub("[]\\]$", "", abstracts_1_4999$cleaned_URL)
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))

url_check <-count(abstracts_1_4999, cleaned_URL_last) ##will still have a little junk (~10) but ok for now

##### check dup cleaned URLs - remove very stuborn junk ####
abstracts_1_4999$cleaned_URL <- gsub('\\s+', '', abstracts_1_4999$cleaned_URL)
abstracts_1_4999 <- abstracts_1_4999[!grepl("clinicaltrials",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("https://www.clinicaltrials.gov",abstracts_1_4999$cleaned_URL),] ## stupidly, this is necessary
abstracts_1_4999 <- abstracts_1_4999[!grepl("ClinicalTrials",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("annualreviews",abstracts_1_4999$cleaned_URL),]

check_dup_url <- data.frame(table(abstracts_1_4999$cleaned_URL))

## merge abstracts and URLs with other info in the whole seed set
keep <- select(pmc_seed, 1, 8,9,11, 12)
abstracts_1_4999 <- right_join(keep, abstracts_1_4999, by ="id")

## de-duplicating - newest paper may not be the resource paper
## match strings in title and URL, will need this later anyway for naming
## e.g. in title = "Integration of 1:1 orthology maps and updated datasets into Echinobase."
## URL = "https://echinobase.org"
## => "echinobase" 

abstracts_1_4999$blank_URL <- tolower(str_replace_all(abstracts_1_4999$cleaned_URL, "[[:punct:]]", " "))
abstracts_1_4999$blank_title <- tolower(str_replace_all(abstracts_1_4999$title, "[[:punct:]]", " "))

## reorder for my sanity during fussing

match_test <- select(abstracts_1_4999, 1, 6, 12, 13)
## match_test$blank_URL 
## match_test$blank_title

match_test_ids <- match_test$id

title_url_match  <- NULL;

for (row in 1:nrow(match_test)) {
  t2 <- unique(strsplit(match_test$blank_URL, " +")[[row]])
  t1 <- unique(strsplit(match_test$blank_title, " +")[[row]])
  id <- (match_test$id)[[row]]
  all_t <- c(t2, t1)
  found <- paste0(all_t[duplicated(all_t)], collapse = ", ")
  found[found == ""] = NA
  report <- cbind(id, found)
  title_url_match <- rbind(title_url_match, report)
}

title_url_match <- as.data.frame(title_url_match)
abstracts_1_4999 <- right_join(abstracts_1_4999, title_url_match, by = "id")
names(abstracts_1_4999)[names(abstracts_1_4999)=="found"] <- "title_url_match"
abstracts_1_4999 <- select(abstracts_1_4999, 1, 6, 4, 2, 3, 5, 7, 8:14)

## classification by title key words
abstracts_1_4999 <- mutate(abstracts_1_4999, title_class = ifelse(test = grepl("database", abstracts_1_4999$title, ignore.case = T),
                                                    yes = "database",
                                                    no = ifelse(test = grepl("data", abstracts_1_4999$title, ignore.case = T),
                                                                yes = "data",
                                                                no = ifelse(test = (grepl("base", abstracts_1_4999$title, ignore.case = T) & !grepl("based", abstracts_1_4999$title, ignore.case = T)),
                                                                            yes = "base",
                                                                            no = ifelse(test = grepl("db", abstracts_1_4999$title, ignore.case = T),
                                                                                        yes = "db",
                                                                                        no = ifelse(test = grepl("bank", abstracts_1_4999$title, ignore.case = T),
                                                                                                    yes = "bank",
                                                                                                    no = ifelse(test = grepl("package", abstracts_1_4999$title, ignore.case = T),
                                                                                                                yes = "package",
                                                                                                                no = ifelse(test = grepl("program", abstracts_1_4999$title, ignore.case = T),
                                                                                                                            yes = "program",
                                                                                                                            no = ifelse(test = grepl("tool", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                        yes = "tool",
                                                                                                                                        no = ifelse(test = grepl("server", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                                    yes = "server",
                                                                                                                                                    no = ifelse(test = grepl("study", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                                                yes = "study",
                                                                                                                                                                no = ifelse(test = grepl("doi", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                                                            yes = "doi",
                                                                                                                                                                            no = "other"))))))))))))
                           
                                                                                                                                                 
                                                                                                                                                    

## check count quick
t1 <- abstracts_1_4999 %>% count(title_class)
## save file
write.csv(abstracts_1_4999,"abstracts_1_4999_2021-08-06.csv", row.names = FALSE) 

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