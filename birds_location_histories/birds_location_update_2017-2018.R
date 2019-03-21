
# README ------------------------------------------------------------------

# Update birds_location_histories (in this case all at Salt River (SRBP) sites) to
# reflect changes to those sites made in 2017-2018. These changes are documented
# in SRBP_Changes2017-2018_forStevan.xlsx provided by Sally.

# There are some references to herps here as this update affected both bird
# (documented here) and herp (documented in knb-lter-cap.627) sites.


# libraries ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)


# database connections ----------------------------------------------------

source('~/Documents/localSettings/mysql_prod.R')
mysql <- mysql_prod_connect('lter34birds')

source('~/Documents/localSettings/mysql_local.R')
mysql <- mysql_local_connect('lter34birds')

dbGetInfo(mysql)


# import changes ----------------------------------------------------------

siteUpdate <- read_excel("SRBP_Changes2017-2018_forStevan.xlsx")
siteUpdate <- siteUpdate[!apply(siteUpdate, 2, function(x) all(is.na(x)) | all(x==""))]
siteUpdate <- siteUpdate %>% 
  filter(!is.na(`BIRD POINTS`)) %>% 
  mutate(
    `BIRD POINTS` = str_replace_all(`BIRD POINTS`, " ", "_"),
    `BIRD POINTS` = tolower(`BIRD POINTS`),
    `Last survey` = as.Date(`Last survey`),
    `First survey` = as.Date(`First survey`)
  )


# birds -------------------------------------------------------------------

birdUpdate <- siteUpdate %>% 
  slice(1:4) %>% 
  mutate(
    `BIRD POINTS` = replace(`BIRD POINTS`, grepl("ave35", `BIRD POINTS`), "ave35_dwn_b1_core"),
    Reason = paste0(Reason, ": ", `...12`)
  )

birdSites <- dbGetQuery(mysql,"
                        SELECT * 
                        FROM lter34birds.sites
                        WHERE sample LIKE 'SRBP';") %>% 
  mutate(site_code = tolower(site_code))

birdUpdate <- inner_join(birdUpdate, birdSites, by = c("BIRD POINTS" = "site_code"))


# add end dates to current moved sites

# note that this join works because there is only one instance of each of these
# sites, if one had been moved previously (i.e., more than one of location
# history per site) we would have needed an additional where such as where
# end_date is not null or something like that

baseUpdateEndDate <- '
UPDATE birds_location_histories blh
JOIN sites ON (sites.site_id = blh.site_id)
SET
  blh.site_id = ?siteid,
  blh.end_date = ?enddate,
  blh.end_date_month = ?endmonth,
  blh.end_date_year = ?endyear,
  blh.location_histories_notes = ?notes
WHERE sites.site_code LIKE ?sitename;'


for (i in 1:nrow(birdUpdate)) {
  
  updateEndDate <- sqlInterpolate(ANSI(),
                                  baseUpdateEndDate,
                                  siteid = birdUpdate[i,]$site_id,
                                  enddate = as.character(birdUpdate[i,]$`Last survey`),
                                  endmonth = month(birdUpdate[i,]$`Last survey`),
                                  endyear = year(birdUpdate[i,]$`Last survey`),
                                  sitename = birdUpdate[i,]$`BIRD POINTS`,
                                  notes = birdUpdate[i,]$Reason
  )
  
  dbExecute(mysql, updateEndDate)
  
}

# insert new coords for moved sites

baseInsertNewBird <- '
INSERT INTO birds_location_histories
(
  site_id,
  lat,
  `long`,
  begin_date,
  begin_date_month,
  begin_date_year
)
VALUES
(
  ?siteid,
  ?lat,
  ?long,
  ?begindate,
  ?beginmonth,
  ?beginyear
);'
  
  
  for (i in 1:nrow(birdUpdate)) {
    
    
    insertNewBird <- sqlInterpolate(ANSI(),
                                    baseInsertNewBird,
                                    siteid = birdUpdate[i,]$site_id,
                                    lat = birdUpdate[i,]$`New lat`,
                                    long = birdUpdate[i,]$`New long`,
                                    begindate = as.character(birdUpdate[i,]$`First survey`),
                                    beginmonth = month(birdUpdate[i,]$`First survey`),
                                    beginyear = year(birdUpdate[i,]$`First survey`)
    )
    
    dbExecute(mysql, insertNewBird)
    
  }
