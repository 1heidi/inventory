# For running separate job to check HTTP status for URLs
## see https://blog.rstudio.com/2019/03/14/rstudio-1-2-jobs/
## on console pane -> jobs -> start local job

library(tidyverse)
library(httr)

abstracts_1_4999 <- read_csv("abstracts_1_4999_2021-07-23.csv")
test_status_list <- abstracts_1_4999$ContentURL

# NOTE: websites that aren't secure or throw other kinds of hand-off errors are ignored via 
# tryCatch function e.g. see this return -- GET("https://echinobase.org")...
# Error in curl::curl_fetch_memory(url, handle = handle) : 
#   SSL: no alternative certificate subject name matches target host name 'echinobase.org'

test_status  <- NULL;
for (i in test_status_list) {
  delayedAssign("do.next", {next})
  r <- tryCatch(sapply(i, GET), error = function(e) force(do.next))
  ContentURL <- r[[1]]
  status_code <- r[[2]]
  report <- cbind(ContentURL, status_code)
  test_status <- as.data.frame(rbind(test_status, report))
}

abstracts_1_4999_URL_checked <- left_join(abstracts_1_4999, test_status, by="ContentURL")
write.csv(abstracts_1_4999_URL_checked ,"abstracts_1_4999_URL_checked.csv", row.names = FALSE) 

##end
