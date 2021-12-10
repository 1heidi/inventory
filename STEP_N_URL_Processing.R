## STEP_N_URL_Processing - Developing process for extracting and matching multiple URLs 
## Parts: 1) extract and clean URLs from abstracts and 2) find string matches b/t title and URLs
## Package(s): tidyverse, stringr, httr
## Input file(s): 
## Notes: total work in progress, just got far enough to be able to tell this strategy will be helpful

library(tidyverse)
library(stringr)
library(httr)
## library(urltools)

##===========================================================##
#######  Part 1 - extract and clean URLs from abstracts ####### 
##===========================================================##

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
abstracts_1_4999$ContentURL <- str_extract_all(abstracts_1_4999$abstractText, url_pattern) ## extracts all URL, concatenates/comma sep 
##some have no URLs - there was just WWW or HTTP(S) in the abstract
abstracts_1_4999 <- filter(abstracts_1_4999, !is.na(abstracts_1_4999$ContentURL))

## Iterative cleaning of URLs to trim off the junk, add in new column for comparison, also add last character column for checks
abstracts_1_4999$cleaned_URL <- NA
abstracts_1_4999$cleaned_URL_last <- NA

abstracts_1_4999$cleaned_URL <- gsub("<.*$", "", abstracts_1_4999$ContentURL) 
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))
abstracts_1_4999$cleaned_URL <-gsub("[.)-;:?*&>]$", "", abstracts_1_4999$cleaned_URL)
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))
abstracts_1_4999$cleaned_URL <-gsub("[.)>-]$", "", abstracts_1_4999$cleaned_URL)
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))
abstracts_1_4999$cleaned_URL <-gsub("[]\\]$", "", abstracts_1_4999$cleaned_URL)
abstracts_1_4999$cleaned_URL_last <- substr(abstracts_1_4999$cleaned_URL,(nchar(abstracts_1_4999$cleaned_URL)+1)-1,nchar(abstracts_1_4999$cleaned_URL))

url_check <-count(abstracts_1_4999, cleaned_URL_last) ##may still have a little junk (~10) but ok enough

##### check for duplicated cleaned URLs - remove very stubborn junk - may need to be more on whole seed set ####
abstracts_1_4999$cleaned_URL <- gsub('\\s+', '', abstracts_1_4999$cleaned_URL)
abstracts_1_4999 <- abstracts_1_4999[!grepl("clinicaltrials",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("https://www.clinicaltrials.gov",abstracts_1_4999$cleaned_URL),] ## stupidly, this is necessary
abstracts_1_4999 <- abstracts_1_4999[!grepl("ClinicalTrials",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("annualreviews",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("onlinelibrary.wiley.com",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("cos.io",abstracts_1_4999$cleaned_URL),]
abstracts_1_4999 <- abstracts_1_4999[!grepl("elsevier.com",abstracts_1_4999$cleaned_URL),]

## check for more junk missed
check_dup_url <- data.frame(table(abstracts_1_4999$cleaned_URL))

## merge abstracts and URLs with other info in the whole seed set
keep <- select(pmc_seed, 1, 8, 9, 11, 12)
abstracts_1_4999 <- right_join(keep, abstracts_1_4999, by ="id")

##==================================================================##
####### PART 3: Find matching strings between titles and URLs  ####### 
##==================================================================##

## will help for 1) de-publication down to one resource for the inventory and 2) for resource naming
## e.g. in title = "Integration of 1:1 orthology maps and updated datasets into Echinobase."
## URL = "https://echinobase.org"
## => "echinobase"

matches  <- NULL;
for (row in 1:nrow(abstracts_1_4999)) {
  t2 <- unique(strsplit(tolower(str_replace_all(abstracts_1_4999$cleaned_URL, "[[:punct:]]", " ")), " +")[[row]]) ## replace all punct w/ space & split into character strings, change to all lower case, and de-deup
  t1 <- unique(strsplit(tolower(str_replace_all(abstracts_1_4999$title, "[[:punct:]]", " ")), " +")[[row]]) ## replace all punct w/ space & split into character strings, change to all lower case, and de-deup
  id <- (abstracts_1_4999$id)[[row]]
  all_t <- c(t2, t1)
  title_url_match <- paste0(all_t[duplicated(all_t)], collapse = ", ")
  title_url_match[title_url_match == ""] = NA
  report <- cbind(id, title_url_match)
  matches <- as.data.frame(rbind(matches, report))
}

abstracts_1_4999 <- right_join(abstracts_1_4999, matches, by = "id")
abstracts_1_4999 <- select(abstracts_1_4999, 1, 6, 4, 2, 5, 3, 5, 7, 8:11)

## save file - updated date 
write.csv(abstracts_1_4999,"abstracts_1_4999_2021-08-06.csv", row.names = FALSE) 

##============================## WORK AREA ##===============================#

## using a doctored dataset of manually curated since I know has multiple URLS in abstracts
man <- as.data.frame(read.csv("man_testing_20.csv"))

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
man$ContentURL_mult <- str_extract_all(man$abstractText, url_pattern)
man <- filter(man, man$ContentURL_mult != "character(0)")
man <- select(man, 1, 2, 3, 5)
## separate URLS
man <- man %>% unnest(ContentURL_mult) %>%
  group_by(id) %>%
  mutate(counter=paste0("URL_", str_pad(1:n(),2,"left","0"))) %>%
  spread(counter, ContentURL_mult)
man <- ungroup(man)
url_colnames <- colnames(select(man, grep(pattern="URL_", colnames(man))))

## create stoplist
abs_raw <- as_tibble(t(read.table("abstracts_raw_2021-08-10.txt", fill = TRUE)))
abs_raw <- as.data.frame(unname(unlist(abs_raw)))
names(abs_raw)[names(abs_raw) == "unname(unlist(abs_raw))"] <- "word"
abs_raw$word <- str_trim(abs_raw$word)
abs_raw$word <- tolower(abs_raw$word)

abs_raw_counted <-  abs_raw %>%
  count(word, sort = TRUE)
abs_raw_counted$length <- nchar(as.character(abs_raw_counted$word))

abs_raw_counted <- filter(abs_raw_counted, n < 8000) ## blanks
abs_raw_counted <- filter(abs_raw_counted, n > 100)
abs_raw_counted <- filter(abs_raw_counted, length > 2)
names(abs_raw_counted)[names(abs_raw_counted) == "n"] <- "freq"

write.csv(abs_raw_counted,"man_abstracts_stopwords_2021-08-10.csv", row.names = FALSE)

## just nchar 3, 4 and https
my_stopwords <- read.csv("man_abstracts_stopwords_3_4_https_2021-08-10.csv")
my_stopwords <- as.character(my_stopwords$word)

##remove punctuation, make all lower case, remove anything less than 3 char long, 
## and remove custom stopwords - add each as new column since will need the URL later for testing

for(i in url_colnames) {
  tmp <- tolower(as.character(gsub("[[:punct:]]", " ", man[[i]])))
  tmp1 <- str_split(tmp, " +") ## " +" concatenates, " ," does not
  tmp3 <- sapply(tmp1, unique)
  tmp4 <- gsub(paste0(my_stopwords,collapse = "|"),"", tmp3)
  man[,paste0(i,"_stripped")] <- tmp4
} 

## strip title and abstract fields

man$title_stripped <- strsplit(tolower(str_replace_all(man$title, "[[:punct:]]", " ")), " +")
man$title_stripped <- sapply(man$title_stripped, unique)
man$title_stripped <- gsub(paste0(my_stopwords,collapse = "|"),"", man$title_stripped)

man$abs_stripped <- str_remove_all(man$abstractText, url_pattern)
man$abs_stripped <- strsplit(tolower(str_replace_all(man$abs_stripped, "[[:punct:]]", " ")), " +")
man$abs_stripped <- sapply(man$abs_stripped, unique)
man$abs_stripped <- gsub(paste0(my_stopwords,collapse = "|"),"", man$abs_stripped)

## GOOD UP TO HERE -  ## below set up for report, does not work yet

man2 = data.frame(lapply(man, as.character), stringsAsFactors=FALSE)
write.csv(man2,"man_partially_processed_2021-08-10.csv", row.names = FALSE)

##  next try to match title and URLs in a loop - below works but needs to loop through the columns now
##  these strings are constantly dirty!

tit <- test_title$title_stripped_sub
test_title <- select(man, 1, 10, 7:9)

for(i in names(test_title)){
  test_title[[paste(i, 'sub', sep="_")]] <- str_sub(test_title[[i]])
}

for(i in names(test_title)){
  test_title[[paste(i, 'added', sep="_")]] <- str_sub(test_title[[i]])
}

z <- NULL
for (n in sub_1) { if(str_length(n) > 4) { z <- append(z, tit} }

out <- lapply(test_title, str_sub)    
test_title <- cbind(test_title, out)
test_title <- select(test_title, 1, 10:15)
test_title2  <- lapply(test_title, str_length > 4)

matches  <- NULL;
for (row in 1:nrow(test_title)) { {
    
  tt <- str_split(str_replace_all(test_title$title, "[[:punct:]]", " "), " +")[[row]]
    sub_t <- str_sub(tt)
    x <- NULL
    for (t in sub_t) { if(str_length(t) > 4) { x <- append(x, t) } }
    tt <- x
    t1 <- str_split(str_replace_all(test_t$URL_01_stripped, "[[:punct:]]", " "), " +")[[row]] ## just doing one now to test loop, BUT loop through them all!
    sub_1 <- str_sub(t1)
    z <- NULL
    for (n in sub_1) { if(str_length(n) > 4) { z <- append(z, n) } }
    t1 <- z
    id <- (man$id)[[row]]
    all_t <- append(t1, tt)
    title_url_match <- paste0(all_t[duplicated(all_t)], collapse = ", ")
    title_url_match[title_url_match == ""] = NA
    title_matched_URL <- if(is.na(title_url_match)) { paste0(NA) } else { paste0("test") }
    ##title_matched_URL <- if(is.na(title_matched_URL)) { paste0(NA) } else { str_replace_all(title_matched_URL, "[c()]", "")}
    all_a <- append(t1, ta)
    abs_url_match <- paste0(all_a[duplicated(all_a)], collapse = ", ")
    abs_url_match [abs_url_match == ""] = NA
    abs_matched_URL  <- if(is.na(abs_url_match)) { paste0(NA) } else { paste0("test") }
    ##abs_matched_URL <- if(is.na(abs_url_match)) { paste0(NA) } else { str_replace_all(abs_matched_URL, "[c()]", " ")}
    report <- cbind(id, title_url_match, title_matched_URL, abs_url_match, abs_matched_URL)
    matches <- as.data.frame(rbind(matches, report))
  }
}

##works
t <- man[[17]][[1]]
t1 <- str_split(str_replace_all(man[[17]], "[[:punct:]]", " "), " +")[[1]]
##column names
url_colnames_stripped <- colnames(select(man, 17))
## returns NA - see https://stackoverflow.com/questions/1699046/for-each-row-in-an-r-dataframe/45130671
## https://stackoverflow.com/questions/48404052/for-loop-returns-nas-return-only-the-first-match-of-ifelse-statement
h <- c(16,17)
for(i in h) {
t1 <- man[[18]][[1]]
t2 <- i
} 
## check levels
h <- slice(man, 1)
lev <- sapply(h$ContentURL_mult_01_stripped, levels)

matches  <- NULL;
  for (row in 1:nrow(man)) {
    for(i in url_colnames) {
    tt <- str_split(str_replace_all(man$title_stripped, "[[:punct:]]", " "), " +")[[row]]
      sub_t <- str_sub(tt)
      x <- NULL
      for (t in sub_t) { if(str_length(t) > 4) { x <- append(x, t) } }
      tt <- x
    ta <- str_split(str_replace_all(man$abs_stripped, "[[:punct:]]", " "), " +")[[row]]
      sub_a <- str_sub(ta)
      y <- NULL
      for (a in sub_a) { if(str_length(a) > 4) { y <- append(y, a) } }
      ta <- y
      t1 <- str_split(str_replace_all(man$URL_01_stripped, "[[:punct:]]", " "), " +")[[row]] ## just doing one now to test loop, BUT loop through them all!
      sub_1 <- str_sub(t1)
      z <- NULL
      for (n in sub_1) { if(str_length(n) > 4) { z <- append(z, n) } }
      t1 <- z
    id <- (man$id)[[row]]
    all_t <- append(t1, tt)
    title_url_match <- paste0(all_t[duplicated(all_t)], collapse = ", ")
    title_url_match[title_url_match == ""] = NA
    title_matched_URL <- if(is.na(title_url_match)) { paste0(NA) } else { paste0("test") }
    ##title_matched_URL <- if(is.na(title_matched_URL)) { paste0(NA) } else { str_replace_all(title_matched_URL, "[c()]", "")}
    all_a <- append(t1, ta)
    abs_url_match <- paste0(all_a[duplicated(all_a)], collapse = ", ")
    abs_url_match [abs_url_match == ""] = NA
    abs_matched_URL  <- if(is.na(abs_url_match)) { paste0(NA) } else { paste0("test") }
    ##abs_matched_URL <- if(is.na(abs_url_match)) { paste0(NA) } else { str_replace_all(abs_matched_URL, "[c()]", " ")}
    report <- cbind(id, title_url_match, title_matched_URL, abs_url_match, abs_matched_URL)
    matches <- as.data.frame(rbind(matches, report))
    }
  }

## OLD - this worked fine if only one URL ## from STEP_1_europepmc.R

matches  <- NULL;
for (row in 1:nrow(man)) {
  t2 <- unique(strsplit(tolower(str_replace_all(man$ContentURL_mult, "[[:punct:]]", " ")), " +")[[row]]) ## replace all punct w/ space & split into character strings, change to all lower case, and de-deup
  t1 <- unique(strsplit(tolower(str_replace_all(man$title, "[[:punct:]]", " ")), " +")[[row]]) ## replace all punct w/ space & split into character strings, change to all lower case, and de-deup
  id <- (man$id)[[row]]
  all_t <- c(t2, t1)
  title_url_match <- paste0(all_t[duplicated(all_t)], collapse = ", ")
  title_url_match[title_url_match == ""] = NA
  report <- cbind(id, title_url_match)
  matches <- as.data.frame(rbind(matches, report))
}