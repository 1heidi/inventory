## re3data API

## retrieving all resources in re3data via API
## Note: correct schema (2.2) is here: https://gfzpublic.gfz-potsdam.de/pubman/faces/ViewItemOverviewPage.jsp?itemId=item_758898
## https://www.re3data.org/api/doc
## https://github.com/re3data/using_the_re3data_API/blob/main/re3data_API_certification_by_type.ipynb

library(httr)
library(xml2)
library(dplyr)
library(tidyr)

re3data_request <- GET("http://re3data.org/api/v1/repositories")
re3data_IDs <- xml_text(xml_find_all(read_xml(re3data_request), xpath = "//id"))
URLs <- paste("https://www.re3data.org/api/v1/repository/", re3data_IDs, sep = "")

## check returns to see XML - will want most of that info, can start with life sciences subset
## step by step way done in GitHub, but see looping from europePMC.R - may work better?

extract_repository_info <- function(url) {
  list(
    re3data_ID = xml_text(xml_find_all(repository_metadata_XML, "//r3d:re3data.orgIdentifier")),
    type = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:type"))), collapse = "_AND_"),
    certificate = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:certificate"))), collapse = "_AND_"),
    repositoryURL = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:repositoryURL"))), collapse = "_AND_"),
    repositoryName = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:repositoryName"))), collapse = "_AND_"),
    subject = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:subject"))), collapse = "_AND_"),
    providerType = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:providerType"))), collapse = "_AND_"),
    dataLicenseName = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:dataLicenseName"))), collapse = "_AND_"),
    databaseAccessType = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:databaseAccessType"))), collapse = "_AND_"),
    dataAccessType = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:dataAccessType"))), collapse = "_AND_"),
    policyName = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:policyName"))), collapse = "_AND_"),
    description = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:description"))))
  )
}

repository_info <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(repository_info) <- c("re3data_ID","repositoryName", "repositoryURL", "subject", "description", "type", "providerType", "certificate", "dataLicenseName", ",databaseAccessType", "dataAccessType", "policyName")

for (url in URLs) {
  repository_metadata_request <- GET(url)
  repository_metadata_XML <-read_xml(repository_metadata_request) 
  results_list <- extract_repository_info(repository_metadata_XML)
  repository_info <- rbind(repository_info, results_list)
}

##returned 2714 repos

write.csv(repository_info,"re3data_all_repos_2021-08-06.csv", row.names = FALSE) 

## filter down to only life science (domain focused) - NOTE: do not remove service providers, many are data resouces

## extract "life sci" only
## Note 1: many in nat sci look to be life sci too but not classified as such, also can have multiple domains but 
## must be either life or natural sci
## Note 2: removing "other" gets rid of too much
life_sci_re3data <- filter(repository_info, grepl("Life", subject) | grepl("Natur", subject))
## remove any strictly institutional and not disciplinary (both okay)
life_sci_re3data <- filter(life_sci_re3data, life_sci_re3data$type != "institutional")

write.csv(life_sci_re3data,"re3data_life_sci_repos_2021-08-06.csv", row.names = FALSE) 

##what was removed - this is okay
removed <- anti_join(repository_info, life_sci_re3data)



