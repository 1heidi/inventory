# For running separate job to check HTTP status for URLs
## see https://blog.rstudio.com/2019/03/14/rstudio-1-2-jobs/
## this takes a long time so on console pane -> jobs -> start local job

library(tidyverse)
library(httr)

abstracts_1_4999 <- read_csv("abstracts_1_4999_2021-07-23.csv")
test <- slice(abstracts_1_4999, 1:20)
test_status_list <- test$cleaned_URL

# NOTE: websites that aren't secure or throw other kinds of hand-off errors are ignored via 
# tryCatch function e.g. see this return -- GET("https://echinobase.org")...
# Error in curl::curl_fetch_memory(url, handle = handle) : 
#   SSL: no alternative certificate subject name matches target host name 'echinobase.org'

  test_status  <- NULL;
  for (i in test_status_list) {
    delayedAssign("do.next", {next})
    r <- tryCatch(sapply(i, GET), error = function(cond) {
      message(paste("URL issue:", i, sep="\n"))
      message(cond, sep="\n")
      return(NA)
      force(do.next)})
    cleaned_URL <- i
    returnedURL <- r[[1]]
    status_code <- if (is.na(r)) {paste0("fail")} else {r[[2]]}
    report <- cbind(cleaned_URL, returnedURL, status_code)
    test_status <- as.data.frame(rbind(test_status, report))
  }
  
##check for failed URL handoffs
sum(is.na(URL_join_check$status_code))
##summarize results
summary_URL_checks_1_4999 <- aggregate(abstracts_1_4999_URL_checked$status_code, by=list(status_code=abstracts_1_4999_URL_checked$status_code), FUN=sum)
  
abstracts_1_4999_URL_checked <- left_join(abstracts_1_4999, test_status, by="ContentURL")
write.csv(abstracts_1_4999_URL_checked ,"abstracts_1_4999_URL_checked.csv", row.names = FALSE) 

##end
