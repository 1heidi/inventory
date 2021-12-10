## Purpose: Classify records by title and abstract features - testing via expected words, e.g. "database"
## Parts: 1) testing via title and 2) testing via abstract
## Package(s): tidyverse, stringr, httr
## Input file(s): manual_checks_hji_2021-08-20.csv, abstract_classifiers_2021-09-03.csv
## Output file(s): classification_summary_2021-09-03.csv
## Notes: classification summary for titles promising, based entirely on a crude cascade - abstracts not done, have a list of expected 
## words in file instead of directly in the code. Words based on observations during manual curation.

library(tidyverse)
library(stringr)
library(httr)

##============================================================================================##
######### PART 1 - TESTING via TITLE CLASSIFICATION with manually checked set from [2021-08-20] ######### 
##============================================================================================##

checks <- read.csv("manual_checks_hji_2021-08-20.csv")

## classification by title keywords via crude cascade
checks <- mutate(checks, title_class = ifelse(test = grepl("database", checks$title, ignore.case = T),
                                                                  yes = "database",
                                                                  no = ifelse(test = grepl("data", checks$title, ignore.case = T),
                                                                              yes = "data",
                                                                              no = ifelse(test = (grepl("base", checks$title, ignore.case = T) & !grepl("based", checks$title, ignore.case = T)),
                                                                                          yes = "base",
                                                                                          no = ifelse(test = grepl("db", checks$title, ignore.case = T),
                                                                                                      yes = "db",
                                                                                                      no = ifelse(test = grepl("bank", checks$title, ignore.case = T),
                                                                                                                  yes = "bank",
                                                                                                                  no = ifelse(test = grepl("package", checks$title, ignore.case = T),
                                                                                                                              yes = "package",
                                                                                                                              no = ifelse(test = grepl("program", checks$title, ignore.case = T),
                                                                                                                                          yes = "program",
                                                                                                                                          no = ifelse(test = grepl("tool", checks$title, ignore.case = T),
                                                                                                                                                      yes = "tool",
                                                                                                                                                      no = ifelse(test = grepl("server", checks$title, ignore.case = T),
                                                                                                                                                                  yes = "server",
                                                                                                                                                                  no = ifelse(test = grepl("study", checks$title, ignore.case = T),
                                                                                                                                                                              yes = "study",
                                                                                                                                                                              no = ifelse(test = grepl("doi", checks$title, ignore.case = T),
                                                                                                                                                                                          yes = "doi",
                                                                                                                                                                                          no = "other"))))))))))))

## summarize

classification_summary <- checks %>% 
  select(hji_recheck, title_class) %>% 
    group_by(title_class) %>%
      mutate(total = length(title_class)) %>%
        mutate(class_percent = total/nrow(checks)) %>%
          mutate(excluded = sum(hji_recheck == 0)) %>%
            mutate(included = sum(hji_recheck == 1)) %>%
              mutate(included_percent = included/total)

classification_summary <- unique(select(classification_summary, 2:7))

write.csv(classification_summary,"classification_summary_2021-09-03.csv", row.names = FALSE) 

##===============================================================================================##
######### PART 2 - TESTING via ABSTRACT CLASSIFICATION with manually checked set from [2021-08-20] ######### 
##===============================================================================================##

abs_class <- read.csv("abstract_classifiers_2021-09-03.csv")
abs_class <- dplyr::pull(abs_class, word)

# extract the list of matching words
x <- sapply(abs_class, function(x) grepl(tolower(x), tolower(checks$abstractText)))

# paste the matching words together
checks$abs_match <- apply(x, 1, function(i) paste0(names(i)[i], collapse = ","))

# count the number of matching words
checks$abs_match_count <- apply(x, 1, function(i) sum(i))

