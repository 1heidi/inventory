## Purpose: Retrieve records from Europe PMC and clean initial "seed set" before classification
## Parts: 1) retrieve initial records from query and 2) retrieve abstracts
## Package(s): europepmc, tidyverse
## Input file(s): manual_checks_hji_2021-08-20.csv (temp! to enrich for testing workflow)
## Output file(s): pmc_seed_all_2021-08-06.csv, (temp!) abstracts_1_4999_2021-08-06.csv (temp!)
## NOTE: depending on Europe PMC query, will need to check ids that do not return w/ details, URL cleaning efficacy, etc. There
## are some very stubborn strings that aren't excluded in the query despite trying (especially re: clinical trial registries).
## There are definitely better ways to do this.

library(europepmc)
library(tidyverse)

##====================================================================##
####### PART 1: get seed set via query string built [2021-08-06] ####### 
##====================================================================##

my_query <- '(((ABSTRACT:"www" OR ABSTRACT:"http" OR ABSTRACT:"https") AND (ABSTRACT:"data" OR ABSTRACT:"resource" OR ABSTRACT:"database"))  NOT (TITLE:"retraction" OR TITLE:"retracted" OR TITLE:"withdrawn" OR TITLE:"withdrawal" OR TITLE:"erratum") NOT ((ABSTRACT:"retract" OR ABSTRACT:"withdraw" ABSTRACT:"erratum" OR ABSTRACT:"github.com" OR ABSTRACT:"github.io" OR ABSTRACT:"cran.r" OR ABSTRACT:"youtube.com" OR ABSTRACT:"bitbucket.org" OR ABSTRACT:"links.lww.com" OR ABSTRACT:"osf.io" OR ABSTRACT:"bioconductor.org" OR ABSTRACT:"annualreviews.org" OR ABSTRACT:"creativecommons.org" OR ABSTRACT:"sourceforge.net" OR ABSTRACT:".pdf" OR ABSTRACT:"clinical trial" OR ABSTRACT:"registry" OR ABSTRACT:"registration" OR ABSTRACT:"trial registration" OR ABSTRACT:"clinicaltrial" OR ABSTRACT:"registration number" OR ABSTRACT:"pre-registration" OR ABSTRACT:"preregistration"))) AND (((SRC:MED OR SRC:PMC OR SRC:AGR OR SRC:CBA))) AND (FIRST_PDATE:[2011 TO 2021])'
query_count <-epmc_hits(query = my_query)
pmc_seed <- epmc_search(query=my_query, limit = 25000) ##takes a few minutes

##a few checks (fyi - not a good time to de-dup, do later)
sum(is.na(pmc_seed$id)) ## usually a few all NA rows return
pmc_seed <- filter(pmc_seed, !is.na(pmc_seed$id))
year_counts <-count(pmc_seed, pubYear) ## a few pre-2011 years may sneak in
pmc_seed <- filter(pmc_seed, pmc_seed$pubYear > 2010)
year_counts <-count(pmc_seed, pubYear) ##check for craziness
journal_counts <-count(pmc_seed, journalTitle) ##check for craziness

##save entire seed set - update date
write.csv(pmc_seed,"pmc_seed_all_2021-08-06.csv", row.names = FALSE) ## 21,235 total records as of date

##===========================================================================================##
#######  PART 2: starting with entire seed set, query detailed records to get abstracts ####### 
##===========================================================================================##

pmc_seed <- read.csv("pmc_seed_all_2021-08-06.csv")
manual_checks <- read.csv("manual_checks_hji_2021-08-20.csv") ## manually curated set

## using 5K records first in developing workflow for speed
## want to include the manually checked records so have something to test against
man_list <-manual_checks$id
set1 <- slice(pmc_seed, 10001:14346)
set1_list <-set1$id

## retrieve details from Europe PMC
## takes a several minutes
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

# check, but failures largely due to source
source_check <- lost_1_4999 %>% count(source)

### retrieve those lost by source - PMC & ARC & MED ### 

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

## for testing - just first 5K set, but later will be full set
write.csv(abstracts_1_4999,"abstracts_1_4999_2021-08-06.csv", row.names = FALSE) 