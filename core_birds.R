
# README ----

# SPATIAL DATA

# SPATIAL DATA (DATASET NO. 160) the spatial data are in a separate data set,
# and yes, it is in PASTA: 
# https://sustainability.asu.edu/caplter/data/data-catalog/view/knb-lter-cap.160/
# going to need to, of course, add the spatial data to the new dataset, and
# update 160 to note that the data are in the new/updated package

# and see birds_locationHistories_database.R

# DATA REGARDING ODD BALL SITES
# THESE ARE OF TYPE 'VOLUNTEER' AND ARE NOT INCLUDED

# REGARDING ODD BALL SITES Christopher Hensley 
# <chrishensley@mail.fresnostate.edu> 12/28/15
# 
# Hi Stevan,
# 
# Thanks for the tip, the shape files are working really nicely. I'm wondering, 
# though, if these are all the survey sites? There are lots of entries in the 
# surveys table with site_ids that don't exist in the shape file, like gc-4, 
# r1-B, RD-1, etc. Do you know anything about where these observations might be 
# coming from?
# 
# Thanks, Chris
# 
# below is Paige's response to Chris (and me):
# 
# I'm afraid I have no idea what project those points are associated with. I
# would advise you not to use them at all, since they are undoubtedly selected
# in a different way and have so little data associated with them.
# 
# Paige makes a great point here in that we have no idea how these points were
# selected, and regardless are not in keeping with what constitute the weight of
# this data set. We need to either not include them or to keep something akin to
# the sample desigation so that these sites (and SRBP & riparian) can be
# isolated.
  
# DO NOT FORGET TO ADD THE NOISE DATA
# I am thinking to publish these separately


# SRBP

# FOR SRBP: there are six birding points at each site, two birding points along
# each of the three transects, yielding, for example, Tonto_mid_B1, Tonto_mid_B2
# (recall that there are three herp plots along each transect). To this point, a
# single birder (formerly Melanie), birded at all six points four times per
# year. The other two birders, birded at a single core site during the two
# regular birding seasons.

# Here pulling only SRBP core sites (i.e., those visited by all three birders),
# and will publish SRBP (including the core sites so some overlap) separately


# reml slots ----
getSlots("dataset")
  getSlots("distribution")
  getSlots("keywordSet")
    getSlots("keyword")
getSlots("dataTable")
getSlots("physical")
  getSlots("dataFormat")
    getSlots("textFormat")
  getSlots("size")
  getSlots("distribution")
    getSlots("online")
      getSlots("url")
getSlots("additionalInfo")
  getSlots("section")
  getSlots("para")
getSlots("metadataProvider")
  getSlots("individualName")
  getSlots("userId")
getSlots("creator")
  getSlots("individualName")
  getSlots("userId")

# libraries ----
library("EML")
library('RPostgreSQL')
library('RMySQL')
library('tidyverse')
library("tools")
library("readr")
library("readxl")
library("stringr")

# functions and working dir ----
source('~/Dropbox (ASU)/localRepos/dataPublishing/writeAttributesFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createKMLFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createdataTableFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createDataTableFromFileFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/address_publisher_contact_language_rights.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createOtherEntityFn.R')
setwd("~/db_asu/tempeTownLake/data_ready_to_process")

# DB connections ----
con <- dbConnect(MySQL(),
                 user='srearl',
                 password=.rs.askForPassword("Enter password:"),
                 dbname='lter34birds',
                 host='stegosaurus.gios.asu.edu')

prod <- dbConnect(MySQL(),
                 user='srearl',
                 password=.rs.askForPassword("Enter password:"),
                 dbname='gios2_production',
                 host='mysql.prod.aws.gios.asu.edu')

# pg <- dbConnect(dbDriver("PostgreSQL"),
#                 user="srearl",
#                 dbname="working",
#                 host="localhost",
#                 password=.rs.askForPassword("Enter password:"))
# 
# pg <- dbConnect(dbDriver("PostgreSQL"),
#                 user="srearl",
#                 dbname="caplter",
#                 host="stegosaurus.gios.asu.edu",
#                 password=.rs.askForPassword("Enter password:"))

# dataset details to set first ----
projectid <- 46
packageIdent <- 'knb-lter-cap.46.12'
pubDate <- '2016-12-14'

# data processing ----

# CORE BIRDS

# 2016-12-02. The meaning of 'flying' in the birds table is unclear. There are
# 1962 records where flying = 1. All of these except for two records have
# distance = FT. However, not all records where distance = FT have a distance =
# 1 (any any value for that matter). Adding confusion, in her metadata, Corinna
# has listed that flying = NULL is true, but then what would be the meaning of
# flying = 0, and that would mean that most birds were flying. I am not certain,
# but my impression is that flying was a precursor to FT. A "flying" option is
# not on the current datasheet, nor on an earlier one revised in 2004. I am
# going to omit flying from the publication of these data as I think it is more
# confusing than helpful (and I cannot explain its meaning).

core_birds <- dbGetQuery(con, "
SELECT
  sites.site_code,
  sites.sample AS location_type,
  surveys.survey_date,
  surveys.time_start,
  surveys.time_end,
  surveys.observer,
  surveys.wind_speed,
  surveys.wind_dir,
  surveys.air_temp,
  surveys.cloud_cover,
  surveys.notes AS survey_notes,
  surveys.human_activity_notes,
  surveys.wind,
  surveys.precipitation,
  surveys.disturbances,
  surveys.sight_obstruct,
  surveys.noise_level,
  surveys.site_condition,
  surveys.non_bird_species,
  bird_taxons.code,
  bird_taxons.common_name,
  birds.distance,
  birds.bird_count,
  birds.notes AS observation_notes,
  birds.seen,
  birds.heard,
  birds.direction,
  birds.QCcomment
FROM lter34birds.surveys
JOIN lter34birds.sites ON (surveys.site_id = sites.site_id)
JOIN lter34birds.birds ON (surveys.survey_id = birds.survey_id)
JOIN lter34birds.bird_taxons ON (birds.bird_taxon_id = bird_taxons.id)
WHERE 
  sites.sample LIKE '200 point' OR
  sites.sample LIKE 'riparian' OR 
  sites.sample LIKE 'north desert village' OR
  (sites.sample LIKE 'SRBP' AND sites.site_code LIKE CONCAT('%','CORE'))
ORDER BY survey_date
LIMIT 500000;")

core_birds[core_birds == ''] <- NA # lots of missing values, convert to NA

# pulling this code out separately owing to its verbosity for a singular 
# purpose: getting the intials of the observers and presenting those instead of
# the full name
core_birds <- core_birds %>% 
  mutate(observer = toupper(observer)) %>% 
  separate(observer, c("name1", "name2"), " ", remove = T) %>% 
  mutate(init1 = str_extract(name1, "\\b\\w")) %>% 
  mutate(init2 = str_extract(name2, "\\b\\w")) %>% 
  unite(observer_initials, init1, init2, sep = "", remove = T) 

core_birds <- core_birds %>% 
  mutate(survey_date = as.Date(survey_date)) %>% 
  mutate(location_type = replace(location_type, location_type == "200 point", "ESCA")) %>% 
  mutate(location_type = replace(location_type, location_type == "North Desert Village", "NDV")) %>% 
  mutate(location_type = as.factor(location_type)) %>% 
  mutate(wind_dir = as.factor(wind_dir)) %>% 
  mutate(wind = as.factor(wind)) %>% 
  mutate(precipitation = as.factor(precipitation)) %>% 
  mutate(disturbances = as.factor(disturbances)) %>% 
  mutate(noise_level = as.factor(noise_level)) %>% 
  mutate(distance = as.factor(distance)) %>% 
  mutate(seen = as.factor(seen)) %>% 
  mutate(heard = as.factor(heard)) %>% 
  mutate(direction = as.factor(direction)) %>% 
  select(site_code:time_end, observer_initials, wind_speed:QCcomment)
  
writeAttributes(core_birds) # write data frame attributes to a csv in current dir to edit metadata
core_birds_desc <- "bird survey sampling details (site, date, time, observer, site conditions, and notes) and birds surveyed (type, number, distance from observer, behavior)"
  
location_type <- c(ESCA = "colocated with Ecological Survey of Central Arizona (ESCA)",
                   NDV = "North Desert Village, ASU Polytechnic Campus",
                   Riparian = "Riparian habitat",
                   SRBP = "Salt River Biological Project")
wind_dir <- c(NE = "north east",
              S = "south",
              SE = "south east",
              NW = "north west",
              E = "east",
              N = "north",
              W = "west",
              SW = "south west")
wind <- c(none = "no perecptible wind",
          light = "light wind",
          gusts = "wind gusts")
precipitation <- c(none = "no precipitation",
                   light_rain = "light rain")
disturbances <- c(`0` = "no perceptible disturbance to observer or in the vicinity during the survey",
                  `1` = "disturbance occurred during the survey")
noise_level <- c(none = "no noise during survey",
                 low = "low level noise during the survey",
                 high = "high level of noise during the survey")
distance <- c(`0-5` = "bird observed within zero to five meters of observer",
              `5-10` = "bird observed five to ten meters from observer",
              `10-20` = "bird observed ten to twenty meters from observer",
              `20-40` = "bird observed twenty to forty meters from observer",
              `>40` = "bird observed forty or more meters from observer",
              FT = "bird is seen flying through the count area below the tallest structure or vegetation, and not observed taking off or landing")
seen <- c(`0` = "bird not identified by sight",
          `1` = "bird identified by sight")
heard <- c(`0` = "bird not identified by sound",
           `1` = "bird identified by sound")
direction <- c(NW = "north west",
               S = "south",
               N = "north",
               NE = "north east",
               SE = "south east",
               W = "west",
               SW = "south west",
               E = "east")

listOfFactors <- sapply(core_birds, is.factor)
trueList <- which(listOfFactors)

core_birds_factors <- data.frame()
for(i in 1:length(trueList)) {
factor_elements <- get(names(trueList)[i])
temp_frame <- rbind(
  data.frame(
    attributeName = names(trueList)[i],
    code = names(factor_elements),
    definition = unname(factor_elements)
  ))
core_birds_factors <- rbind(core_birds_factors, temp_frame)
}

core_birds_DT <- createDTFF(dfname = core_birds,
                            factors = core_birds_factors,
                            description = core_birds_desc)

# CORE_BIRD_SITES 
core_bird_sites <- dbGetQuery(con, "
SELECT
  sites.site_code,
  sites.sample AS location_type,
  sites.description,
  sites.address
FROM lter34birds.sites
WHERE 
  sample LIKE '200 point' OR
  sample LIKE 'riparian' OR
  sample LIKE 'north desert village' OR
  (sites.sample LIKE 'SRBP' AND sites.site_code LIKE CONCAT('%','CORE'));")

core_bird_sites <- core_bird_sites %>%
  mutate(location_type = replace(location_type, location_type == "200 point", "ESCA")) %>% 
  mutate(location_type = replace(location_type, location_type == "North Desert Village", "NDV")) %>% 
  mutate(location_type = as.factor(location_type))
  
writeAttributes(core_bird_sites) # write data frame attributes to a csv in current dir to edit metadata
core_bird_sites_desc <- "bird survey location identifier, location type, general description, and approximate address of bird survey locations"

location_type <- c(ESCA = "colocated with Ecological Survey of Central Arizona (ESCA)",
                   NDV = "North Desert Village, ASU Polytechnic Campus",
                   Riparian = "Riparian habitat",
                   SRBP = "Salt River Biological Project")

listOfFactors <- sapply(core_bird_sites, is.factor)
trueList <- which(listOfFactors)

core_bird_sites_factors <- data.frame()
for(i in 1:length(trueList)) {
factor_elements <- get(names(trueList)[i])
temp_frame <- rbind(
  data.frame(
    attributeName = names(trueList)[i],
    code = names(factor_elements),
    definition = unname(factor_elements)
  ))
core_bird_sites_factors <- rbind(core_bird_sites_factors, temp_frame)
}

core_bird_sites_DT <- createDTFF(dfname = core_bird_sites,
                                 factors = core_bird_sites_factors,
                                 description = core_bird_sites_desc)

# spatial data ----

# Get the bird survey locations. Here we are extracting these data from the 
# database as opposed to using an existing shapefile as I am presenting only the
# most up-to-date location information (as opposed to the locations and their 
# changes through time). Double-check the query, it worked with the small number
# of SRBP sites with updated locations at the time these were pulled but not sure
# its accuracy when the data are more complicated (e.g., a given location having
# moved multiple times) and note that I am using only year to reflect the most 
# recent location, if a site moved twice in a year, month would have to be 
# considered as well. Also note that I had to include blh.end_date_year in the
# query to be able to include it in the HAVING clause.

# These spatial data reflect the locations updated by Shero in spring 2013. 
# However, these data lacked M-9, which is an old site (2000-2001) but
# referenced in these bird data (note that I have excluded the volunteer sites,
# which are all of those really odd, early sites). The previous published
# spatial data (knb-lter-cap.160) included M-9 but did not reflect any updates
# through 2013. I merged M-9 from the 160 dataset with the updated through 2013
# data, and exported it to KML for inclusion here. These spatial data include
# also the most up-to-date SRBP core sites (core sites only!). I will update 160
# with a reference to this data set.

core_bird_locations <- dbGetQuery(con, "
SELECT 
  s.site_code,
  CASE
    WHEN s.sample LIKE '200 point' THEN 'ESCA'
    WHEN s.sample LIKE 'North Desert Village' THEN 'NDV'
    ELSE s.sample
  END AS location_type,
  blh.lat,
  blh.`long`,
  blh.end_date_year
FROM lter34birds.birds_location_histories blh
JOIN lter34birds.sites s ON (s.site_id = blh.site_id)
WHERE 
  s.sample LIKE '200 point' OR
  s.sample LIKE 'riparian' OR
  s.sample LIKE 'north desert village' OR
  (s.sample LIKE 'SRBP' AND s.site_code LIKE CONCAT('%','CORE'))
GROUP BY s.site_code
HAVING blh.end_date_year = MAX(blh.end_date_year) OR blh.end_date_year IS NULL
ORDER BY location_type, site_code;") %>% 
  select(-end_date_year)

# convert tabular data to kml
library("sp")
library("rgdal")
coordinates(core_bird_locations) <- c("long", "lat")
proj4string(core_bird_locations) <- CRS("+init=epsg:4326")
# core_bird_locations <- spTransform(core_bird_locations, CRS("+proj=longlat +datum=WGS84")) 
# spTransform not required here as already in WGS 84
writeOGR(core_bird_locations, "core_bird_locations.kml", layer = "core_bird_locations", driver = "KML")

kml_desc <- "bird survey locations"
core_bird_locations <- createKML(kmlobject = 'core_bird_locations.kml',
                                 description = kml_desc)


# title and abstract ----
title <- 'Point-count bird censusing: long-term monitoring of bird abundance and diversity in central Arizona-Phoenix, ongoing since 2000'

abstract <- 
"Over the past half-century, the greater Phoenix metropolitan area (GPMA) has been one of the fastest growing regions in the US, experiencing rapid urban expansion in addition to urban intensification. This backdrop provides an ideal setting to monitor biodiversity changes in response to urbanization, and the CAP LTER has been using a standardized point-count protocol to monitor the bird community in the GPMA and surrounding Sonoran desert region since 2000. 

The bird survey locations in this CAP LTER core monitoring program include three general groups of sites. Forty bird survey locations were selected from a subset of the CAP LTER's Ecological Survey of Central Arizona (ESCA; formerly named Survey200) long-term monitoring sites. ESCA sites were located using a tessellation-stratified dual-density sampling design, and, as such, span a diversity of habitats including urban, suburban, rural, commercial areas, parks, agricultural fields, and native Sonoran desert. Earlier versions of this data package included data from ESCA. However, while positioned in close proximity, the bird survey locations do not necessarily overlap with the 30m X 30m plot that constitutes an ESCA sampling location, and leveraging data from these two monitoring programs should be addressed carefully. ESCA data have corresponding survey location names, and those data are available through the CAP LTER and LTER network data portals. Additional bird survey locations were positioned in treatment areas of the North Desert Village (NDV). This was a site of intense study on the Arizona State University Polytechnic Campus in which the CAP LTER converted the landscaping of small neighborhoods to reflect the dominant landscaping preferences employed throughout the GPMA. NDV landscape types include: oasis (NDV-O), xeric (NDV-X), mesic (NDV-M), control (NDV-C), and native (NDV-N). Finally, while the forty bird survey locations that were selected to coincide with ESCA sampling locations span a wide diversity of habitats throughout the GPMA, because of the generally random nature of selecting those sites, they did not reflect riparian habitats. Riparian areas are important bird habitat but constitute a very small area of the GPMA. To address this deficiency, bird survey locations were established specifically in twelve riparian habitats. Riparian habitat sub-types include: (1) ephemeral-engineered (EE, n=4), (2) ephemeral-natural (EN, n=2), (3) perennial-engineered (PE, n=3), and (4) perennial-natural (PN, n=3). 

In a given season, each bird survey location is visited independently by three birders who count all birds seen or heard within a 15-minute window. The frequency of surveys has varied through the life of the project. The first year of the project (2000) was generally a pilot year in which each site was visited approximately twice by a varying number of birders. The monitoring became more formalized beginning in 2001, and each site was visited in each of four seasons by three birders. The frequency of visits was reduced to three seasons in 2005, and to two season (spring, winter) beginning in 2006." 


# people ----

# Eyal

eyal <- dbGetQuery(prod, "
SELECT
	people.first_name,
	people.last_name,
	people.email,
	people_address.institution,
	people_address.department
FROM gios2_production.people
JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
WHERE people.last_name LIKE 'shochat'
;")

eyal_name <- new('individualName',
                 givenName = eyal$first_name,
                 surName = eyal$last_name)

eyalShochat <- new('creator',
                   individualName = eyal_name,
                   organizationName = eyal$institution,
                   electronicMailAddress = eyal$email)

# Madhu

madhu <- dbGetQuery(prod, "
SELECT
	people.first_name,
	people.last_name,
	people.email,
	people_address.institution,
	people_address.department
FROM gios2_production.people
JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
WHERE people.last_name LIKE 'katti'
;")

madhu_name <- new('individualName',
                  givenName = mahdu$first_name,
                  surName = mahdu$last_name)

madhu_orcid <- new('userId',
                   'http://orcid.org/0000-0003-3076-3562',
                   directory = 'orcid.org')

madhuKatti <- new('creator',
                   individualName = madhu_name,
                   organizationName = madhu$institution,
                   electronicMailAddress = madhu$email,
                   userId = madhu_orcid)

# Paige

paige <- dbGetQuery(prod, "
SELECT
	people.first_name,
	people.last_name,
	people.email,
	people_address.institution,
	people_address.department
FROM gios2_production.people
JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
WHERE 
  people.last_name LIKE 'warren' AND
  people.first_name LIKE 'paige';")

paige_name <- new('individualName',
                  givenName = paige$first_name,
                  surName = paige$last_name)

paigeWarren <- new('creator',
                   individualName = paige_name,
                   organizationName = paige$institution,
                   electronicMailAddress = paige$email)

# Dan

dan <- dbGetQuery(prod, "
SELECT
	people.first_name,
	people.last_name,
	people.email,
	people_address.institution,
	people_address.department
FROM gios2_production.people
JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
WHERE 
  people.last_name LIKE 'childers' AND
  people.first_name LIKE 'dan'
;")

dan_name <- new('individualName',
                givenName = dan$first_name,
                surName = dan$last_name)

danChilders <- new('creator',
                   individualName = dan_name,
                   organizationName = dan$institution,
                   electronicMailAddress = dan$email)

# Heather

heather <- dbGetQuery(prod, "
SELECT
	people.first_name,
	people.last_name,
	people.email,
	people_address.institution,
	people_address.department
FROM gios2_production.people
JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
WHERE 
  people.last_name LIKE 'bateman'
;")

heather_name <- new('individualName',
                    givenName = heather$first_name,
                    surName = heather$last_name)

heatherBateman <- new('creator',
                      individualName = heather_name,
                      organizationName = heather$institution,
                      electronicMailAddress = heather$email)

# Stevan

stevan <- dbGetQuery(prod, "
SELECT
	people.first_name,
	people.last_name,
	people.email,
	people_address.institution,
	people_address.department
FROM gios2_production.people
JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
WHERE people.last_name LIKE 'earl'
;")

stevan_name <- new('individualName',
                   givenName = stevan$first_name,
                   surName = stevan$last_name)

stevan_orcid <- new('userId',
                    'http://orcid.org/0000-0002-4465-452X',
                    directory = 'orcid.org')

stevanEarl <- new('metadataProvider',
                   individualName = stevan_name,
                   organizationName = stevan$institution,
                   electronicMailAddress = stevan$email,
                   userId = stevan_orcid)


creators <- c(as(heatherBateman, 'creator'),
              as(danChilders, 'creator'),
              as(madhuKatti, 'creator'),
              as(eyalShochat, 'creator'),
              as(paigeWarren, 'creator'))

metadataProvider <-c(as(stevanEarl, 'metadataProvider'))

# keywords ----
keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("urban",
                     "birds",
                     "species abundance",
                     "species composition",
                     "communities",
                     "community composition")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas and CAP LTER IRTs",
        keyword =  c("disturbance patterns",
                     "population studies",
                     "adapting to city life")),
    new("keywordSet",
        keywordThesaurus = "Creator Defined Keyword Set",
        keyword =  c("aves",
                     "avifauna")),
    new("keywordSet",
        keywordThesaurus = "CAPLTER Keyword Set List",
        keyword =  c("cap lter",
                     "cap",
                     "caplter",
                     "central arizona phoenix long term ecological research",
                     "arizona",
                     "az",
                     "arid land"))
    )


# methods and coverages ----
methods <- set_methods("~/Dropbox (ASU)/localRepos/lterBirdMonitoring/lterBirds_46_methods.md")

begindate <- as.character(min(core_birds$survey_date))
enddate <- as.character(max(core_birds$survey_date))
geographicDescription <- "CAP LTER study area"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         # sci_names = c("Salix spp",
                         #               "Ambrosia deltoidea"),
                         geographicDescription = geographicDescription,
                         west = -112.742, east = -111.622,
                         north = +33.8814, south = +33.2187)


# construct the dataset ----

# address, publisher, contact, and rights come from a sourced file

# XML DISTRUBUTION
  xml_url <- new("online",
                 onlineDescription = "CAPLTER Metadata URL",
                 url = paste0("https://sustainability.asu.edu/caplter/data/data-catalog/view/", packageIdent, "/xml/"))
metadata_dist <- new("distribution",
                 online = xml_url)

# DATASET
dataset <- new("dataset",
               title = title,
               creator = creators,
               pubDate = pubDate,
               metadataProvider = metadataProvider,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               distribution = metadata_dist,
               dataTable = c(core_birds_DT,
                             core_bird_sites_DT),
               otherEntity = c(core_bird_locations))

# construct the eml ----

# ACCESS
allow_cap <- new("allow",
                 principal = "uid=CAP,o=LTER,dc=ecoinformatics,dc=org",
                 permission = "all")
allow_public <- new("allow",
                    principal = "public",
                    permission = "read")
lter_access <- new("access",
                   authSystem = "knb",
                   order = "allowFirst",
                   scope = "document",
                   allow = c(allow_cap,
                             allow_public))

# CUSTOM UNITS
# standardUnits <- get_unitList()
# unique(standardUnits$unitTypes$id) # unique unit types

# custom_units <- rbind(
#   data.frame(id = "microsiemenPerCentimeter",
#              unitType = "conductance",
#              parentSI = "siemen",
#              multiplierToSI = 0.000001,
#              description = "electric conductance of lake water in the units of microsiemenPerCentimeter"),
# data.frame(id = "nephelometricTurbidityUnit",
#            unitType = "unknown",
#            parentSI = "unknown",
#            multiplierToSI = 1,
#            description = "(NTU) ratio of the amount of light transmitted straight through a water sample with the amount scattered at an angle of 90 degrees to one side"))
# unitList <- set_unitList(custom_units)

eml <- new("eml",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset)

# write the xml to file ----
write_eml(eml, "knb-lter-cap.46.12.xml")
