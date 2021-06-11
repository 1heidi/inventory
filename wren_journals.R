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

journal_stats <- wren_master %>%
  group_by(journal) %>%
    mutate(all = sum(length(url))) %>%
      mutate(db = sum(off_class == "a_database")) %>%
        mutate(program = sum(off_class == "a_program")) %>%
          mutate(other = sum(off_class == "other"))

journal_stats <- select(journal_stats,2, 8:11)
journal_stats <- distinct(journal_stats)

write.csv(journal_stats,"wren_journal_stats.csv", row.names = FALSE) 

top_20 <- filter(journal_stats, db > 20)