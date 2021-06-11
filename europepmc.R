##https://cran.r-project.org/web/packages/europepmc/europepmc.pdf

library(europepmc)
library(tidyverse)

my_query <- '(ABSTRACT:"www" OR ABSTRACT:"http") AND (TITLE:"database")'

pmc_list_all <- epmc_search(query=my_query, limit = 5000)
pmc_list_trim <- select(pmc_list_all, 1, 3:7, 10, 11,13:16,20,22,28)

journal_counts <-count(pmc_list_trim, journalTitle)
year_counts <-count(pmc_list_trim, pubYear)

## TESTING example is:
# Huang, et al 2020
# AcrDB: a database of anti-CRISPR operons in prokaryotes and viruses, 
# Nucleic Acids Research
# https://doi.org/10.1093/nar/gkaa857

test_full_text <- epmc_ftxt("PMC7778997") ## see below for citation

test_details <- epmc_details(ext_id = 33068435, data_src = "med")

## from epmc_details can get ...
## * basic -> affiliation - DO NOT USE ... first author affiliation only
## * author_details -> lastName (for list of last names only)
## * authorAffiliationDetailsList -> authorAffiliation = for each author... last will most likely be the cooresponding author
## * mesh_topic -> descriptorName  - returns more than descriptorName for qualifiers (below)!
## * mesh_qualifiers -> qualifiersName 
## * grants -> DO NOT USE ... did not return above