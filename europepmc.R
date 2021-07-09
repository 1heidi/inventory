##https://cran.r-project.org/web/packages/europepmc/europepmc.pdf

library(europepmc)
library(tidyverse)

my_query <- '(((ABSTRACT:"www" OR ABSTRACT:"http" OR ABSTRACT:"https") AND (ABSTRACT:"data" OR ABSTRACT:"resource" OR ABSTRACT:"database")) NOT (ABSTRACT:"Retraction:" OR ABSTRACT:"https://github.com" OR ABSTRACT:"http://github.com" OR ABSTRACT:"Clinical Trial Registry" OR ABSTRACT:"Clinical Trials Registry" OR ABSTRACT:"Trial registration" OR ABSTRACT:"https://clinicaltrials.gov" OR ABSTRACT:"registration number" OR ABSTRACT:"https://osf.io" OR ABSTRACT:"Pre-registration" OR ABSTRACT:"Preregistration") ) AND (((SRC:MED OR SRC:PMC OR SRC:AGR OR SRC:CBA))) AND (FIRST_PDATE:[2011 TO 2021])'

pmc_list_all <- epmc_search(query=my_query, limit = 25000)

journal_counts <-count(pmc_list_all, journalTitle)
year_counts <-count(pmc_list_all, pubYear)

names(journal_counts)[names(journal_counts)=="journalTitle"] <- "journal"

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


## TESTING example is:
# Huang, et al 2020
# AcrDB: a database of anti-CRISPR operons in prokaryotes and viruses, 
# Nucleic Acids Research
# https://doi.org/10.1093/nar/gkaa857

## working intermittently (2021-07-09 ...)
test_full_text <- epmc_ftxt(ext_id = "PMC7778997")
test_details <- epmc_details(ext_id = 33068435, data_src = "med")

## from epmc_details can get ...
## * basic -> affiliation - DO NOT USE ... first author affiliation only
## * author_details -> lastName (for list of last names only)
## * authorAffiliationDetailsList -> authorAffiliation = for each author... last will most likely be the cooresponding author
## * mesh_topic -> descriptorName  - returns more than descriptorName for qualifiers (below)!
## * mesh_qualifiers -> qualifiersName 
## * grants -> DO NOT USE ... did not return above