##Purpose: evaluation of journals documented in Wren et 2017  doi:10.1093/nar/gkx182
##Package(s): tidyverse
##Input file(s): wren_File003_all.csv, wren_File004_class.csv
##Output file(s): wren_journal_stats.csv

library(tidyverse)

wren_all <- read_csv("wren_File003_all.csv")
wren_class <- read_csv("wren_File004_class.csv") # Note: off_class = official classification
wren_master <- full_join(wren_all, wren_class, by="PMID")
## spot check via journal names
wren_master <- select(wren_master, 1, 3, 4, 6, 7, 2, 5)

# some "duplication" e.g. PMIDs when multiple urls and multiple urls when diff journals, etc
# checks

## remove no url and NA from wren_master = 27,350 to 27,301

wren_master <- filter(wren_master, off_class != "NA" & off_class != "no_URL")

journal_count_all <- wren_master %>% count(journal)
# 3096 

journal_stats <- wren_master %>%
  group_by(journal) %>%
    mutate(all = sum(length(url))) %>%
      mutate(db = sum(off_class == "a_database")) %>%
        mutate(program = sum(off_class == "a_program")) %>%
          mutate(other = sum(off_class == "other"))

journal_stats <- select(journal_stats,2, 8:11)
journal_stats <- distinct(journal_stats)

write.csv(journal_stats,"wren_journal_stats.csv", row.names = FALSE) 


## for 2011 on...

wren_master_2011_2015 <- filter(wren_master, pub_year >2011)
wren_master_2011_2015 <- filter(wren_master_2011_2015, off_class != "NA" & off_class != "no_URL")

journal_count_2011_2015 <- wren_master_2011_2015 %>% count(journal)

journal_stats_2011_2015 <- wren_master_2011_2015 %>%
  group_by(journal) %>%
    mutate(all = sum(length(url))) %>%
      mutate(db = sum(off_class == "a_database")) %>%
         mutate(program = sum(off_class == "a_program")) %>%
           mutate(other = sum(off_class == "other"))

journal_stats_2011_2015 <- select(journal_stats_2011_2015,2, 8:11)
wren_journal_stats_2011_2015 <- distinct(journal_stats_2011_2015)

write.csv(wren_journal_stats_2011_2015 ,"wren_journal_stats_2011_2015.csv", row.names = FALSE) 
