##Purpose: Retrieve records from Europe PMC and clean initial "seed set" before classification
##Parts: 1) retrieve initial records from query, 2) retrieve abstracts, 3) extract and clean URLs from abstracts
##Package(s): europepmc, tidyverse, stringr, httr
##Input file(s): manual_checks_hji_2021-07-28.csv
##Output file(s): pmc_seed_all_2021-08-06.csv, (temp!) abstracts_1_4999_2021-08-06.csv
##NOTE: depending on Europe PMC query, will need to check ids that do not return w/ details, URL cleaning efficacy, etc. There
## are some very stubborn strings that aren't excluded in the query despite trying (especially re: clinical trials registries)

library(europepmc)
library(tidyverse)
library(stringr)
library(httr)

#===================================================================##
####### PART 1: get seed set via query string built [2021-08-06] ####### 
#===================================================================##

my_query <- '(((ABSTRACT:"www" OR ABSTRACT:"http" OR ABSTRACT:"https") AND (ABSTRACT:"data" OR ABSTRACT:"resource" OR ABSTRACT:"database"))  NOT (TITLE:"retraction" OR TITLE:"retracted" OR TITLE:"withdrawn" OR TITLE:"withdrawal" OR TITLE:"erratum") NOT ((ABSTRACT:"retract" OR ABSTRACT:"withdraw" ABSTRACT:"erratum" OR ABSTRACT:"github.com" OR ABSTRACT:"github.io" OR ABSTRACT:"cran.r" OR ABSTRACT:"youtube.com" OR ABSTRACT:"bitbucket.org" OR ABSTRACT:"links.lww.com" OR ABSTRACT:"osf.io" OR ABSTRACT:"bioconductor.org" OR ABSTRACT:"annualreviews.org" OR ABSTRACT:"creativecommons.org" OR ABSTRACT:"sourceforge.net" OR ABSTRACT:".pdf" OR ABSTRACT:"clinical trial" OR ABSTRACT:"registry" OR ABSTRACT:"registration" OR ABSTRACT:"trial registration" OR ABSTRACT:"clinicaltrial" OR ABSTRACT:"registration number" OR ABSTRACT:"pre-registration" OR ABSTRACT:"preregistration"))) AND (((SRC:MED OR SRC:PMC OR SRC:AGR OR SRC:CBA))) AND (FIRST_PDATE:[2011 TO 2021])'
pmc_seed <- epmc_search(query=my_query, limit = 25000)

##a few checks, not a good time to de-dup later
sum(is.na(pmc_seed$id)) ## 2
pmc_seed <- filter(pmc_seed, !is.na(pmc_seed$id))
year_counts <-count(pmc_seed, pubYear) ## a few pre-2011 years may sneak in
pmc_seed <- filter(pmc_seed, pmc_seed$pubYear > 2010)
year_counts <-count(pmc_seed, pubYear) ##check for craziness
journal_counts <-count(pmc_seed, journalTitle) ##check for craziness

##save entire seed set
write.csv(pmc_seed,"pmc_seed_all_2021-08-06.csv", row.names = FALSE) ## 21,235 total records

#============================================================================================##
#######  PART 2: starting with entire seed set, query detailed records to get abstracts ####### 
#============================================================================================##

pmc_seed <- read.csv("pmc_seed_all_2021-08-06.csv")
manual_checks <- read.csv("manual_checks_hji_2021-07-28.csv")

## using 5K records first 
##include the 653 manually checked already
man_list <-manual_checks$id
set1 <- slice(pmc_seed, 10001:14346)
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

# not all 4999 are returned - find failed queries
set1_list <- as.data.frame(c(set1_list, man_list))
names(set1_list)[names(set1_list)=="c(set1_list, man_list)"] <- "id"
lost_1_4999 <- anti_join(set1_list, abstracts_1_4999, by = "id")
lost_1_4999 <- right_join(pmc_seed, lost_1_4999, by = "id")

# check but failures largely due to source
source_check <- lost_1_4999 %>% count(source)

# retrieve those lost by source - PMC & ARC & MED

lost_list <- lost_1_4999$id

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

lost_med  <- NULL;
for (i in lost_list) {
  r <- sapply(i, epmc_details, data_src = "med") 
  id <- r[[1]]["id"]
  title <- r[[1]]["title"]
  abstract <- r[[1]]["abstractText"]
  report <- cbind(id, title, abstract)
  lost_med <- rbind(lost_med, report)
}

#round up back together
lost_found <- rbind(lost_agr, lost_pmc, lost_med)
abstracts_1_4999 <- rbind(abstracts_1_4999, lost_found)
check_dup_id <- data.frame(table(abstracts_1_4999$id)) ## doubling checking

#============================================================##
#######  Part 3 - extract and clean URLs from abstracts ####### 
#============================================================##

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
abstracts_1_4999$ContentURL <- str_extract(abstracts_1_4999$abstractText, url_pattern)
##some have no URLs - there was just WWW or HTTP in the abstract
abstracts_1_4999 <- filter(abstracts_1_4999, !is.na(abstracts_1_4999$ContentURL))

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

url_check <-count(abstracts_1_4999, cleaned_URL_last) ##may still have a little junk (~10) but ok enough

##### check dup cleaned URLs - remove very stubborn junk - may need to be more on whole seed set - see below####
abstracts_1_4999$cleaned_URL <- gsub('\\s+', '', abstracts_1_4999$cleaned_URL)
abstracts_1_4999 <- abstracts_1_4999[!grepl("clinicaltrials",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("https://www.clinicaltrials.gov",abstracts_1_4999$cleaned_URL),] ## stupidly, this is necessary
abstracts_1_4999 <- abstracts_1_4999[!grepl("ClinicalTrials",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("annualreviews",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("onlinelibrary.wiley.com",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("cos.io",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("elsevier.com",abstracts_1_4999$cleaned_URL),]

## check for more junk missed
check_dup_url <- data.frame(table(abstracts_1_4999$cleaned_URL))

## merge abstracts and URLs with other info in the whole seed set
keep <- select(pmc_seed, 1, 8, 9, 11, 12)
abstracts_1_4999 <- right_join(keep, abstracts_1_4999, by ="id")

## de-duplicating - newest paper may not be the resource paper
## match strings in title and URL, will need this later anyway for naming
## e.g. in title = "Integration of 1:1 orthology maps and updated datasets into Echinobase."
## URL = "https://echinobase.org"
## => "echinobase" 

matches  <- NULL;
for (row in 1:nrow(abstracts_1_4999)) {
  t2 <- unique(strsplit(tolower(str_replace_all(abstracts_1_4999$cleaned_URL, "[[:punct:]]", " ")), " +")[[row]]) ## replace all punct w/ space & split into character strings, change to all lower case, and de-deup
  t1 <- unique(strsplit(tolower(str_replace_all(abstracts_1_4999$title, "[[:punct:]]", " ")), " +")[[row]]) ## replace all punct w/ space & split into character strings, change to all lower case, and de-deup
  id <- (abstracts_1_4999$id)[[row]]
  all_t <- c(t2, t1)
  title_url_match <- paste0(all_t[duplicated(all_t)], collapse = ", ")
  title_url_match[title_url_match == ""] = NA
  report <- cbind(id, title_url_match)
  matches <- as.data.frame(rbind(matches, report))
}

abstracts_1_4999 <- right_join(abstracts_1_4999, matches, by = "id")
##abstracts_1_4999 <- select(abstracts_1_4999, 1, 6, 4, 2, 3, 5, 7, 8:14)

## save file
write.csv(abstracts_1_4999,"abstracts_1_4999_2021-08-06.csv", row.names = FALSE) 

#============================## WORK AREA ##===============================#

## viewing single abstracts
print <- abstracts_1_4999[18,]
View(print)
print$abstractText

## Testing returns
# Huang, et al 2020
# AcrDB: a database of anti-CRISPR operons in prokaryotes and viruses, 
# Nucleic Acids Research
# https://doi.org/10.1093/nar/gkaa857

test_details <- epmc_details(ext_id = 33068435)

## from epmc_details can get ...
## * basic -> affiliation - DO NOT USE ... first author affiliation only
## * author_details -> lastName (for list of last names only)
## * authorAffiliationDetailsList -> authorAffiliation = for each author... last will most likely be the cooresponding author
## * mesh_topic -> descriptorName  - returns more than descriptorName for qualifiers (below)!
## * mesh_qualifiers -> qualifiersName 
## * grants -> DO NOT USE ... did not return above

test_full_text <- epmc_ftxt("PMC7778997") ## only accepts PMCID!

## See: https://github.com/ropensci/europepmc/issues/37#issuecomment-854424678
## Can create a warning
# ext_id <- "31762656"
# if (!grepl(pattern = "PMC\\d*", ext_id)) {
#   warning("Input needs to be a PMCID.")
# }
# #> Warning: Input needs to be a PMCID.
# ext_id <- "PMC6864198"
# if (!grepl(pattern = "PMC\\d*", ext_id)) {
#   warning("Input needs to be a PMCID.")
# }
