##Validation vs. Wren

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
