##Classifications by title

## classification by title key words
abstracts_1_4999 <- mutate(abstracts_1_4999, title_class = ifelse(test = grepl("database", abstracts_1_4999$title, ignore.case = T),
                                                                  yes = "database",
                                                                  no = ifelse(test = grepl("data", abstracts_1_4999$title, ignore.case = T),
                                                                              yes = "data",
                                                                              no = ifelse(test = (grepl("base", abstracts_1_4999$title, ignore.case = T) & !grepl("based", abstracts_1_4999$title, ignore.case = T)),
                                                                                          yes = "base",
                                                                                          no = ifelse(test = grepl("db", abstracts_1_4999$title, ignore.case = T),
                                                                                                      yes = "db",
                                                                                                      no = ifelse(test = grepl("bank", abstracts_1_4999$title, ignore.case = T),
                                                                                                                  yes = "bank",
                                                                                                                  no = ifelse(test = grepl("package", abstracts_1_4999$title, ignore.case = T),
                                                                                                                              yes = "package",
                                                                                                                              no = ifelse(test = grepl("program", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                          yes = "program",
                                                                                                                                          no = ifelse(test = grepl("tool", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                                      yes = "tool",
                                                                                                                                                      no = ifelse(test = grepl("server", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                                                  yes = "server",
                                                                                                                                                                  no = ifelse(test = grepl("study", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                                                              yes = "study",
                                                                                                                                                                              no = ifelse(test = grepl("doi", abstracts_1_4999$title, ignore.case = T),
                                                                                                                                                                                          yes = "doi",
                                                                                                                                                                                          no = "other"))))))))))))




## check count quick
t1 <- abstracts_1_4999 %>% count(title_class)
write.csv(abstracts_1_4999,"abstracts_1_4999_2021-08-06.csv", row.names = FALSE) 