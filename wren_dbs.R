# Starting with Wren's data from: 
# Wren, Jonathan D et al. 
# “Use it or lose it: citations predict the continued online availability of 
# published bioinformatics resources.” 
# Nucleic acids research vol. 45,7 (2017): 3627-3633. doi:10.1093/nar/gkx182

library(tidyverse)

wren_all <- read_csv("wren_all.csv")
wren_class <- read_csv("wren_class.csv")
wren_master <- full_join(wren_all, wren_class, by="PMID")
## spot check via jounral names
wren_master <- select(wren_master, 1, 3, 4, 6, 7, 2, 5)

# some "duplication" e.g. PMIDs when multiple urls and multiple urls when diff journals, etc
# checks

journal_count <- wren_master %>% count(journal)
class_count <- wren_master %>% count(off_class)

## remove no url and NA from wren_master = 27,350 to 27,301

wren_master <- filter(wren_master, off_class != "NA" & off_class != "no_URL")
wren_dbs <- filter(wren_master, off_class == "a_database")
wren_dbs <- select(wren_dbs, 1, 4, 6, 3, 5, 7)

db_journal_count <- wren_dbs %>% count(journal)

wren_non_nar_dbs <- filter(wren_dbs, journal != "Nucleic Acids Res")
wren_non_nar_dbs <- filter(wren_non_nar_dbs, confidence > 0.75)
# 2109 of 5175 

wren_nar_dbs <- filter(wren_dbs, journal == "Nucleic Acids Res")
wren_nar_high_conf <- filter(wren_nar_dbs, confidence > 0.75)
# 978 of 1590

wren_db_o_dbs <- filter(wren_dbs, journal == "Database (Oxford)")
wren_db_o_high_conf <- filter(wren_db_o_dbs, confidence > 0.75)
# 178 of 252
