# README

# the following steps detail updates to birding locations, mostly, in response 
# to relocation(s) associated with the Smart Sampling approach of CAP IV. Sally 
# has provided numerous files detailing needed changes. First is the addition of
# new Core sites. These sites were first birded the winter of 2016-2017. These 
# sites include new desert and residential locations associated with DesFert and
# the new PASS. It is important to note that there are eight existing PASS 
# birding locations that will be maintained and effectively become new Core 
# sites. Two of those were also Core sites (V14 and W15). The other PASS sites 
# that were retained are AA9, Q15, R18, U18, U21, and X17. Each of these kept 
# their name and then had an “b” and “c” site added to the neighborhood. In the 
# four new PASS neighborhoods (IBW, PWR, TRS, and 711) “a,” “b,” and “c” sites 
# were added to each neighborhood. So all of the above are added as new sites 
# save V14 and W15, which already existed as Core sites. Another update are 
# sites that are to be discontinued (e.g., NDV, the old riparian sites, and some
# others). Note that in the sites to be discontinued, V18 and W17 had other 
# location history updates to document owing to some movement prior to their 
# being discontinued, notably development at V18 (Waldon Farm) and reduced 
# access at W17 (Greyhound Park). Note also an inexplicable discrepency at the 
# PASS point X17. Sally noticed that the GPS point was different from the 
# birding point, but we are unsure when the error was introduced. As we had not 
# started tracking the locational histories of PASS birding sites, we will use 
# the updated GPS point as the starting lat long for X17 without noting any 
# change. Finally, Sally had the points re-GPS'd to address some noted 
# discrepencies between the known, established birding location and the GPS 
# reading. Sites with updated GPS points inlude: AB19, T11, T13, U12, U13, U21, 
# U8, V14, Y19, and Z23. The end dates for these points has been put into the 
# database, and the new locations have been added to the database but the one 
# step that we cannot address at this point is the begin dates for these changed
# sites since the change/update takes place spring 2017. As soon as the spring 
# data are entered, go back and populate begin dates for this subset of sites 
# with their respective first survey dates. Note that the dump:
# lter34birds_20170413 is a dump immediately prior to any of these changes
# implemented to the production database.

# libraries ----
library(tidyverse)
library(readxl)
library(RMySQL)
library(stringr)

# db connection ----
lter41 <- dbConnect(MySQL(),
                    user='srearl',
                    password=.rs.askForPassword("Enter password:"),
                    dbname='lter41passbirds',
                    host='mysql.research.gios.asu.edu')

con <- dbConnect(MySQL(),
                    user='srearl',
                    password=.rs.askForPassword("Enter password:"),
                    dbname='lter34birds',
                    host='mysql.research.gios.asu.edu')

con <- dbConnect(MySQL(),
                 user = 'srearl',
                 password=.rs.askForPassword("Enter password:"),
                 host = "127.0.0.1",
                 port = 3306,
                 dbname= "lter34birds")

# add the new Smart Sampling birding location ----
newBirdPoints <- read_excel('NewCoreBirdPoints_2016-2017.xlsx')
newBirdPoints <- newBirdPoints[,c(1:3)]
newBirdPoints <- newBirdPoints %>% 
  filter(!is.na(ID)) %>%
  mutate(ID = toupper(ID))
colnames(newBirdPoints) <- str_trim(colnames(newBirdPoints), side = c("both"))

# there are eight PASS sites that will be carried forward. These will
# essentially become new core sites. Two of those eight sites are already core
# sites (i.e., overlap sites), including V14 and W15. The only thing required
# for these two locations is to add the b and c sub locations, which is
# addressed in NewCoreBirdPoints_2016-2017. The other six sites will
# have to be imported from elsewhere. As the PASS sites table in MySQL has the
# location only as UTMs, these six sites will be imported from a shapefile of
# PASS birding locations where the lat long has been calculated in QGIS.
passCarryover <- read_csv('pass_from_shapefile.csv')
passCarryover <- passCarryover %>% 
  filter(POINT_ID %in% c('AA9', 'Q15', 'R18', 'U18', 'U21', 'X17')) %>% 
  select(ID = POINT_ID, Latitude = Y, Longitude = X) %>% 
  mutate(Longitude = replace(Longitude, ID=='X17', -111.959822)) # old PASS coord was wrong, this is an updated location that was used at least since winter 2016. As we do not have any X17 data in this core set, we will use this as the established location but beware that this does differ from the X17 location data associated with PASS

newBirdPoints <- bind_rows(newBirdPoints, passCarryover)

if (dbExistsTable(con, 'newsites')) dbRemoveTable(con, 'newsites') # make sure tbl does not exist
dbWriteTable(conn = con, name = 'newsites', value = newBirdPoints, row.names = F) # write temp table

dbSendStatement(con,'
INSERT INTO lter34birds.sites
(
  site_code, 
  sample, 
  latitude, 
  longitude
)
(
  SELECT
    ID,
    "CAPIV",
    Latitude,
    Longitude
  FROM
  lter34birds.newsites
);')

# clean up
if (dbExistsTable(con, 'newsites')) dbRemoveTable(con, 'newsites') 

# insert new sites and locations into blh
# just the new sites
dbSendStatement(con,'
INSERT INTO lter34birds.birds_location_histories
(
  site_id,
  lat,
  `long`,
  location_histories_notes
)
(
  SELECT
    site_id,
    latitude,
    longitude,
    "new site added for CAPIV"
  FROM lter34birds.sites
  WHERE 
	  sample LIKE "CAPIV" AND
    site_code NOT IN ("AA9","U18","U21","X17","R18","Q15")
);')

# insert new sites and locations into blh
# just the PASS sites that are essentially new core sites, "carryovers"
dbSendStatement(con,'
INSERT INTO lter34birds.birds_location_histories
(
  site_id,
  lat,
  `long`,
  location_histories_notes
)
(
  SELECT
    site_id,
    latitude,
    longitude,
    "former PASS site carried over to CAPIV"
  FROM lter34birds.sites
  WHERE 
	  sample LIKE "CAPIV" AND
    site_code IN ("AA9","U18","U21","X17","R18","Q15")
);')

# to avoid confusion, remove lat longs from sites table                
dbSendStatement(con,'
UPDATE lter34birds.sites 
  SET latitude = NULL
WHERE sample LIKE "CAPIV";')

dbSendStatement(con,'
UPDATE lter34birds.sites 
  SET longitude = NULL
WHERE sample LIKE "CAPIV";')




# update new locations of existing sites ----

# address V18 (the old urban farm site) separately
dbSendStatement(con, '
UPDATE lter34birds.birds_location_histories
JOIN lter34birds.sites ON (sites.site_id = birds_location_histories.site_id)
SET
  end_date = "2014-03-28",
  end_date_month = 3,
  end_date_year = 2014,
  location_histories_notes = "discontinued due to lack of access when development started"
WHERE sites.site_code LIKE "V-18";')

# document change to W-17 beginning winter 2013
# first terminate existing location details
dbSendStatement(con, '
UPDATE lter34birds.birds_location_histories
JOIN lter34birds.sites ON (sites.site_id = birds_location_histories.site_id)
SET 
  end_date = "2012-05-07",
  end_date_month = 5,
  end_date_year = 2012,
location_histories_notes = "moved outside of fenced area"
WHERE sites.site_code LIKE "W-17";')


# then add a new location instance with a beginning date (when it was moved) and
# an end date since it is on the discontinued list. make sure W-17 is site 88
# when run on AWS (it is!)

dbSendStatement(con, '
INSERT INTO lter34birds.birds_location_histories
(
  site_id, 
  lat, 
  `long`,
  begin_date, 
  begin_date_month, 
  begin_date_year, 
  end_date, 
  end_date_month, 
  end_date_year, 
  location_histories_notes
)
VALUES (
  88, 
  33.444710, 
  -111.999657, 
  "2013-01-04",
  1, 
  2013, 
  "2016-05-01",
  5,
  2016,
  "new location outside of fenced area"
);')


# now for the list of changed points that Sally provided
changedPoints <- read_excel('LatLongChangesforExistingBirdPoints.xlsx', skip = 1)
changedPoints <- changedPoints[c(1:10),]

# get list of existing sites & add site_id to changePoints
sites <- dbGetQuery(con,'
           SELECT site_id, site_code, sample FROM lter34birds.sites') %>% 
  mutate(site_code = gsub("-", "", site_code))
changedPoints <- left_join(changedPoints, sites, by = c("ID" = "site_code"))

# before updating changed points, add end dates to existing points
updatedSitesEndDateFn <- function(siteID) {
  updatedSitesEndDateQuery <- sprintf("
  UPDATE lter34birds.birds_location_histories blh 
  SET 
    blh.end_date = (SELECT MAX(survey_date) FROM lter34birds.surveys WHERE surveys.site_id = %d),
    blh.end_date_month = (SELECT MONTH(MAX(survey_date)) FROM lter34birds.surveys WHERE surveys.site_id = %d),
    blh.end_date_year = (SELECT YEAR(MAX(survey_date)) FROM lter34birds.surveys WHERE surveys.site_id = %d),
    blh.location_histories_notes = 'Because some of the existing latitudes and longitudes did not seem accurate, i.e., did not correctly locate the bird point, new latitude and longitude were recorded for all birding points during the 2016-2017 Winter season. Points whose new coordinates were greater than 5 m from the point indicated by the old coordinates were corrected. None of the physical birding points changed, i.e., in desert locations the cairns were not moved and in urban locations the physical description of the point remained the same.'
  WHERE blh.site_id = %d;", siteID, siteID, siteID, siteID)
  
  dbSendStatement(con, updatedSitesEndDateQuery)

}

# updatedSitesEndDateFn(79)
lapply(changedPoints$site_id, updatedSitesEndDateFn)

# need to update U21 separately since those survey dates are in lter41
dbSendStatement(con, '
UPDATE lter34birds.birds_location_histories
JOIN lter34birds.sites ON (sites.site_id = birds_location_histories.site_id)
SET 
  end_date = "2016-03-14",
  end_date_month = 3,
  end_date_year = 2016,
  location_histories_notes = "this is a former PASS (lter41birds) site retained for CAPIV; because some of the existing latitudes and longitudes did not seem accurate, i.e., did not correctly locate the bird point, new latitude and longitude were recorded for all birding points during the 2016-2017 Winter season. Points whose new coordinates were greater than 5 m from the point indicated by the old coordinates were corrected. None of the physical birding points changed, i.e., in desert locations the cairns were not moved and in urban locations the physical description of the point remained the same."
WHERE sites.site_code LIKE "U21";')


# finally back to the changed points that Sally provided now that we have added
# end dates set for all of these sites in blh

# add a temp table with updated locations and site_id
if (dbExistsTable(con, 'changedsites')) dbRemoveTable(con, 'changedsites') # make sure tbl does not exist
dbWriteTable(conn = con, name = 'changedsites', value = changedPoints, row.names = F) # write temp table

dbSendStatement(con, '
INSERT INTO lter34birds.birds_location_histories
(
  site_id, 
  lat, 
  `long`, 
  location_histories_notes
)
(
  SELECT site_id, 
    Latitude, 
    Longitude, 
    "Because some of the existing latitudes and longitudes did not seem accurate, i.e., did not correctly locate the bird point, new latitude and longitude were recorded for all birding points during the 2016-2017 Winter season. Points whose new coordinates were greater than 5 m from the point indicated by the old coordinates were corrected. None of the physical birding points changed, i.e., in desert locations the cairns were not moved and in urban locations the physical description of the point remained the same." 
  FROM lter34birds.changedsites
);')

# cleanup
if (dbExistsTable(con, 'changedsites')) dbRemoveTable(con, 'changedsites') 

# the one step that we cannot address at this point is the begin dates for these
# changed sites since the change/update takes place spring 2017. As soon as the
# spring data are entered, go back and populate begin dates for this subset of
# sites with their respective first survey dates



# document discountinued sites ----

discontinuedSites <- read_excel('Deleted birding and pitfall points_currentDec2016.xlsx')
colnames(discontinuedSites)[4] <- "tempName"
colnames(discontinuedSites)[7] <- "anotherTempName"
discontinuedSites <- Filter(function(x)!all(is.na(x)), discontinuedSites)
discontinuedSites <- discontinuedSites %>% 
  filter(!`CORE Points` %in% c('V-18', 'W-17')) %>% # these two sites were address separately (above)
  mutate(`CORE Points` = gsub("-", "", `CORE Points`)) %>% 
  mutate(`CORE Points` = gsub(" ", "", `CORE Points`)) %>% 
  mutate(comments = paste0(Notes, "; ", Reason)) %>% 
  select(siteID = `CORE Points`, latitude = Lat, longitude = Long, comments) %>% 
  mutate(comments = gsub(",", "", comments))
discontinuedSites <- inner_join(discontinuedSites, sites, by = c("siteID" = "site_code"))

addEndDatesFn <- function(siteID) {
  addEndDatesFnQuery <- sprintf("
  UPDATE lter34birds.birds_location_histories blh 
  SET 
    blh.end_date = (SELECT MAX(survey_date) FROM lter34birds.surveys WHERE surveys.site_id = %d),
    blh.end_date_month = (SELECT MONTH(MAX(survey_date)) FROM lter34birds.surveys WHERE surveys.site_id = %d),
    blh.end_date_year = (SELECT YEAR(MAX(survey_date)) FROM lter34birds.surveys WHERE surveys.site_id = %d),
    blh.location_histories_notes = '%s'
  WHERE blh.site_id = %d;", siteID, siteID, siteID, discontinuedSites[discontinuedSites$site_id == siteID,]$comments, siteID)
  
  dbSendStatement(con, addEndDatesFnQuery)
  
}

# addEndDatesFn(2)
lapply(discontinuedSites$site_id, addEndDatesFn)
