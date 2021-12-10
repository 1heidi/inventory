## Purpose: Scratch file for testing what returns from the EuropePMC API

library(europepmc)
library(tidyverse)
library(stringr)
library(httr)

## trouble shooting europepmc package
library(reprex)

reprex({
  library(europepmc)
  my_query <- '(((ABSTRACT:"www" OR ABSTRACT:"http" OR ABSTRACT:"https") AND (ABSTRACT:"data" OR ABSTRACT:"resource" OR ABSTRACT:"database"))  NOT (TITLE:"retraction" OR TITLE:"retracted" OR TITLE:"withdrawn" OR TITLE:"withdrawal" OR TITLE:"erratum") NOT ((ABSTRACT:"retract" OR ABSTRACT:"withdraw" ABSTRACT:"erratum" OR ABSTRACT:"github.com" OR ABSTRACT:"github.io" OR ABSTRACT:"cran.r" OR ABSTRACT:"youtube.com" OR ABSTRACT:"bitbucket.org" OR ABSTRACT:"links.lww.com" OR ABSTRACT:"osf.io" OR ABSTRACT:"bioconductor.org" OR ABSTRACT:"annualreviews.org" OR ABSTRACT:"creativecommons.org" OR ABSTRACT:"sourceforge.net" OR ABSTRACT:".pdf" OR ABSTRACT:"clinical trial" OR ABSTRACT:"registry" OR ABSTRACT:"registration" OR ABSTRACT:"trial registration" OR ABSTRACT:"clinicaltrial" OR ABSTRACT:"registration number" OR ABSTRACT:"pre-registration" OR ABSTRACT:"preregistration"))) AND (((SRC:MED OR SRC:PMC OR SRC:AGR OR SRC:CBA))) AND (FIRST_PDATE:[2011 TO 2021])'
  pmc_seed <- epmc_search(query=my_query, limit = 25000)
})

abstracts_1_4999 <- read.csv("abstracts_1_4999_2021-08-06.csv") 

## handy viewing of single abstracts, e.g. from row 18
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
