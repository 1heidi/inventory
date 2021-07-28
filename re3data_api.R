## re3data API

## retreiving all resouces in re3data via API
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
    repositoryUrl = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:repositoryUrl"))), collapse = "_AND_"),
    repositoryName = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:repositoryName"))), collapse = "_AND_"),
    citationReferences = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:citationReferences"))), collapse = "_AND_"),
    policyTypes = paste(unique(xml_text(xml_find_all(repository_metadata_XML, "//r3d:policyTypes"))), collapse = "_AND_")
  )
}

repository_info <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(repository_info) <- c("re3data_ID", "type", "certificate", "repositoryUrl", "repositoryName","citationReferences", "policyTypes")

for (url in URLs) {
  repository_metadata_request <- GET(url)
  repository_metadata_XML <-read_xml(repository_metadata_request) 
  results_list <- extract_repository_info(repository_metadata_XML)
  repository_info <- rbind(repository_info, results_list)
}