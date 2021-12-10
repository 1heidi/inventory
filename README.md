# GBC Inventory Preliminary Exploration

### Done summer/fall 2021

#### Testing EuropePMC API and europePMC R package

* Script = STEP 1 

* Summary: Contains query for retrieving records and the code for getting the abstracts; some fussing to getting abstracts for records from other sources (def better way to do this) - API and package work great themselves.

#### Early coarse classification via title/abstract keywords

* Script = STEP 2

Summary: Contains expected keywords that can be used as classifiers based on observations from manual curation (e.g. 'database' = positive, 'package' = negative). Titles tested via a crude cascade with a set of 653 manually curated records (variable hji_recheck) - looks promising. Scripts for abstract not complete. Note that classifiers for title vs. abstracts different (e.g. more and more granular for abstract). Title classifiers tested are directly in code and abstracts classifers are in a file. All preliminary.

#### Early URL processing

* Script = Step N

* Summary:  A challenge will be getting resource name - sometimes are contained within the URL. This was early cleaning/processing and testing of URL so see if they'll be useful - looks promising. Scripts partially work. Will be somewhere downstream in workflow, hence "Step N"

#### Misc - in Prelim folder

 * Also did testing against data from Wren et 2017 (doi:10.1093/nar/gkx182), some testing of stopwords, and use of re3data API for registered data resources (some may count for this inventory and some may not). 
