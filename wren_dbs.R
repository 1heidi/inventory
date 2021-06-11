##Purpose: evaluation of databases documented in Wren et 2017  doi:10.1093/nar/gkx182
##Package(s): tidyverse
##Input file(s): wren_File003_all.csv, wren_File004_class.csv
##Output file(s): wren_dbs.csv

library(tidyverse)

wren_all <- read_csv("wren_File003_all.csv")
wren_class <- read_csv("wren_File004_class.csv") # Note: off_class = official classification
wren_master <- full_join(wren_all, wren_class, by="PMID")
## spot check via journal names
wren_master <- select(wren_master, 1, 3, 4, 6, 7, 2, 5)

# some "duplication" e.g. PMIDs when multiple urls and multiple urls when diff journals, etc
# checks

class_count <- wren_master %>% count(off_class)

## remove no url and NA from wren_master = 27,350 to 27,301

wren_master <- filter(wren_master, off_class != "NA" & off_class != "no_URL") # correct
wren_dbs <- filter(wren_master, off_class == "a_database")
wren_dbs <- select(wren_dbs, 1, 2, 4, 6, 3, 5, 7)

write.csv(wren_dbs ,"wren_dbs.csv", row.names = FALSE) 

##checks on confidence for NAR, Database-O, and non-NAR dbs - should be higher for first two

wren_non_nar_dbs <- filter(wren_dbs, journal != "Nucleic Acids Res")
wren_non_nar_dbs <- filter(wren_non_nar_dbs, confidence > 0.75)
# 2109 of 5175 

wren_nar_dbs <- filter(wren_dbs, journal == "Nucleic Acids Res")
wren_nar_high_conf <- filter(wren_nar_dbs, confidence > 0.75)
# 978 of 1590

wren_db_o_dbs <- filter(wren_dbs, journal == "Database (Oxford)")
wren_db_o_high_conf <- filter(wren_db_o_dbs, confidence > 0.75)
# 178 of 252
